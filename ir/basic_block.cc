#include "ir/basic_block.h"

#include <cstddef>
#include <cstdint>
#include <string_view>

#include "absl/types/span.h"
#include "ir/instruction/serializer.h"

namespace ir {

std::ostream &operator<<(std::ostream &os, BasicBlock const &b) {
  os << ' ' << b.debug().header << '\n';
  for (auto const &inst : b.instructions_) {
    if (not inst) { continue; }
    os << "  " << inst.to_string() << '\n';
  }
  os << b.jump_.DebugString() << '\n';
  return os;
}

BasicBlock::BasicBlock(BasicBlock const &b) noexcept
    : instructions_(b.instructions_), jump_(b.jump_), debug_(b.debug_) {}

BasicBlock::BasicBlock(BasicBlock &&b) noexcept
    : instructions_(std::move(b.instructions_)),
      jump_(std::move(b.jump_)),
      debug_(b.debug_) {
  b.jump_ = JumpCmd::Return();
}

BasicBlock &BasicBlock::operator=(BasicBlock const &b) noexcept {
  instructions_ = b.instructions_;
  jump_         = b.jump_;
  debug_        = b.debug_;
  return *this;
}

BasicBlock &BasicBlock::operator=(BasicBlock &&b) noexcept {
  instructions_ = std::move(b.instructions_);
  jump_         = std::exchange(b.jump_, JumpCmd::Return());
  return *this;
}

void BasicBlock::Append(BasicBlock &&b) {
  ASSERT(jump_.kind() == JumpCmd::Kind::Uncond);
  instructions_.insert(instructions_.end(),
                       std::make_move_iterator(b.instructions_.begin()),
                       std::make_move_iterator(b.instructions_.end()));
  b.instructions_.clear();
  jump_ = std::move(b.jump_);
}

void BasicBlock::ReplaceJumpTargets(BasicBlock *old_target,
                                    BasicBlock *new_target) {
  jump_.Visit([&](auto &j) {
    using type = std::decay_t<decltype(j)>;
    if constexpr (std::is_same_v<type, JumpCmd::UncondJump>) {
      if (j.block == old_target) { j.block = new_target; }
    } else if constexpr (std::is_same_v<type, JumpCmd::CondJump>) {
      if (j.true_block == old_target) { j.true_block = new_target; }
      if (j.false_block == old_target) { j.false_block = new_target; }
    }
  });
}

BasicBlockProto BasicBlock::ToProto(InstructionSerializer &serializer) const {
  BasicBlockProto result;
  result.mutable_instruction()->Reserve(instructions().size());
  for (auto const &instruction : instructions()) {
    serializer.set_output(*result.add_instruction());
    base::Serialize(serializer, instruction);

    jump().Visit([&](auto &j) {
      constexpr auto type = base::meta<std::decay_t<decltype(j)>>;
      if constexpr (type == base::meta<ir::JumpCmd::RetJump>) {
        *result.mutable_return_jump() = {};
      } else if constexpr (type == base::meta<ir::JumpCmd::UncondJump>) {
        result.set_unconditional_jump(serializer.block(j.block));
      } else if constexpr (type == base::meta<ir::JumpCmd::CondJump>) {
        auto &conditional_jump = *result.mutable_conditional_jump();
        switch (j.reg.kind()) {
          case Reg::Kind::Value:
            conditional_jump.set_value_register(
                j.reg.template as<Reg::Kind::Value>());
            break;
          case Reg::Kind::Output:
            conditional_jump.set_output_register(
                j.reg.template as<Reg::Kind::Output>());
            break;
          case Reg::Kind::Parameter:
            conditional_jump.set_parameter_register(
                j.reg.template as<Reg::Kind::Parameter>());
            break;
          case Reg::Kind::StackAllocation:
            conditional_jump.set_stack_allocation_register(
                j.reg.template as<Reg::Kind::StackAllocation>());
            break;
        }
        conditional_jump.set_true_block(serializer.block(j.true_block));
        conditional_jump.set_false_block(serializer.block(j.false_block));
      } else if constexpr (type == base::meta<ir::JumpCmd::BlockJump>) {
        // TODO: Implement me
      } else if constexpr (type == base::meta<ir::JumpCmd::UnreachableJump>) {
        // We very well may have built up a representation in IR that has
        // unreachable blocks. That's okay and we can simply ignore them when
        // emitting IR to byte-code. It's only when executing an unreachable
        // jump that we know a problem occurred.
      } else {
        static_assert(base::always_false(type));
      }
    });
  }
  return result;
}

bool BasicBlock::FromProto(
    BasicBlockProto const &proto, base::PtrSpan<BasicBlock> blocks,
    absl::FunctionRef<Inst(InstructionProto const &)> deserializer,
    BasicBlock &result) {
  result.instructions_.reserve(proto.instruction().size());
  for (auto const &i : proto.instruction()) {
    result.AppendInstruction(deserializer(i));
  }

  switch (proto.jump_case()) {
    case BasicBlockProto::kReturnJump: result.jump() = JumpCmd::Return(); break;
    case BasicBlockProto::kUnconditionalJump:
      result.jump() = JumpCmd::Uncond(blocks[proto.unconditional_jump()]);
      break;
    case BasicBlockProto::kBlockJump: NOT_YET(); break;
    case BasicBlockProto::kConditionalJump: {
      Reg r;
      auto const &c = proto.conditional_jump();
      switch (c.register_case()) {
        case BasicBlockProto::ConditionalJump::kValueRegister:
          r = Reg(c.value_register());
          break;
        case BasicBlockProto::ConditionalJump::kOutputRegister:
          r = Reg::Output(c.output_register());
          break;
        case BasicBlockProto::ConditionalJump::kParameterRegister:
          r = Reg::Parameter(c.parameter_register());
          break;
        case BasicBlockProto::ConditionalJump::kStackAllocationRegister:
          r = Reg::StackAllocation(c.stack_allocation_register());
          break;
        default: UNREACHABLE();
      }
      result.jump() =
          JumpCmd::Cond(r, blocks[c.true_block()], blocks[c.false_block()]);
    } break;
    default: UNREACHABLE();
  }
  return true;
}

}  // namespace ir

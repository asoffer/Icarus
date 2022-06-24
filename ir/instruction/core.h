#ifndef ICARUS_IR_INSTRUCTION_CORE_H
#define ICARUS_IR_INSTRUCTION_CORE_H

#include <ostream>
#include <sstream>
#include <string>
#include <utility>
#include <vector>

#include "absl/strings/str_cat.h"
#include "base/extend.h"
#include "base/extend/serialize.h"
#include "base/extend/traverse.h"
#include "base/serialize.h"
#include "ir/basic_block.h"
#include "ir/instruction/debug.h"
#include "ir/interpreter/interpreter.h"
#include "ir/out_params.h"
#include "ir/value/reg_or.h"
#include "type/function.h"

namespace ir {
// These instructions are required to appear in every instruction set. They
// consist of any instruction having to do with control-flow or reading/writing
// memory.

struct LoadInstruction
    : base::Extend<LoadInstruction>::With<base::BaseSerializeExtension,
                                          DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "%3$s = load %2$s (%1$s)";

  friend bool InterpretInstruction(interpreter::Interpreter& interpreter,
                                   LoadInstruction const& inst) {
    core::Bytes num_bytes = inst.type.bytes(core::Host);
    interpreter.frame().Load(num_bytes, inst.addr, inst.result);
    return true;
  }

  friend void BaseTraverse(Inliner& inliner, LoadInstruction& inst) {
    base::Traverse(inliner, inst.addr, inst.result);
  }

  type::Type type;
  RegOr<addr_t> addr;
  Reg result;
};

// TODO consider changing these to something like 'basic block arguments'
template <typename T>
struct PhiInstruction
    : base::Extend<PhiInstruction<T>,
                   3>::template With<base::BaseTraverseExtension> {
  using type = T;

  PhiInstruction() = default;
  PhiInstruction(std::vector<BasicBlock const*> blocks,
                 std::vector<RegOr<T>> values)
      : blocks(std::move(blocks)), values(std::move(values)) {}

  void add(BasicBlock const* block, RegOr<T> value) {
    blocks.push_back(block);
    values.push_back(value);
  }

  friend bool InterpretInstruction(interpreter::Interpreter& interpreter,
                                   PhiInstruction const& inst) {
    auto const* previous = interpreter.previous_basic_block();
    auto iter = std::find(inst.blocks.begin(), inst.blocks.end(), previous);
    ASSERT(iter != inst.blocks.end());
    size_t i = std::distance(inst.blocks.begin(), iter);
    interpreter.frame().set(inst.result,
                            interpreter.frame().resolve(inst.values[i]));
    return true;
  }

  friend void BaseSerialize(base::Serializer auto& w,
                            PhiInstruction const& inst) {
    base::Serialize(w, static_cast<uint16_t>(inst.values.size()));
    for (auto block : inst.blocks) { base::Serialize(w, block); }
    for (auto value : inst.values) { base::Serialize(w, value); }
    base::Serialize(w, inst.result);
  }

  friend bool BaseDeserialize(base::Deserializer auto& d,
                              PhiInstruction& inst) {
    uint16_t count;
    if (not base::Deserialize(d, count)) { return false; }
    inst.blocks.reserve(count);
    for (size_t i = 0; i < count; ++i) {
      if (not base::Deserialize(d, inst.blocks.emplace_back())) {
        return false;
      }
    }
    inst.blocks.reserve(count);
    for (size_t i = 0; i < count; ++i) {
      if (not base::Deserialize(d, inst.values.emplace_back())) {
        return false;
      }
    }
    return base::Deserialize(d, inst.result);
  }

  friend std::ostream& operator<<(std::ostream& os,
                                  PhiInstruction const& inst) {
    os << inst.result << " = phi ";
    for (size_t i = 0; i < inst.blocks.size(); ++i) {
      os << "\n    " << inst.blocks[i] << ": " << inst.values[i];
    }

    return os;
  }

  std::string to_string() const {
    std::stringstream ss;
    ss << *this;
    return std::move(ss).str();
  }

  friend base::EnableExtensions;
  std::vector<BasicBlock const*> blocks;
  std::vector<RegOr<T>> values;
  Reg result;
};

// This instruction is a bit strange sets a register to either another registor,
// or an immediate value. By the very nature of Single-Static-Assignment, every
// use of this instruction is an optimization opportunity. If a register is
// initialized with an immediate value, we can do constant propagation. If it is
// initialized with another register, the two registers can be folded into a
// single register.
//
// The benefit of such an instruction is that it enables us to inline code
// without worrying about rewriting register names immediately. This instruction
// should never be visible in the final code.
template <typename T>
struct RegisterInstruction
    : base::Extend<RegisterInstruction<T>>::template With<
          base::BaseTraverseExtension, base::BaseSerializeExtension,
          DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "%2$s = %1$s";

  friend bool InterpretInstruction(interpreter::Interpreter& interpreter,
                                   RegisterInstruction const& inst) {
    interpreter.frame().set(inst.result,
                            interpreter.frame().resolve(inst.operand));
    return true;
  }

  RegOr<T> operand;
  Reg result;
};

template <typename T>
struct StoreInstruction
    : base::Extend<StoreInstruction<T>>::template With<
          base::BaseTraverseExtension, base::BaseSerializeExtension,
          DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "store %1$s into [%2$s]";
  using type                                     = T;

  friend bool InterpretInstruction(interpreter::Interpreter& interpreter,
                                   StoreInstruction const& inst) {
    interpreter.frame().Store(inst.value, inst.location);
    return true;
  }

  RegOr<T> value;
  RegOr<addr_t> location;
};

struct CallInstruction
    : base::Extend<CallInstruction, 4>::With<base::BaseSerializeExtension> {
  CallInstruction() = default;
  CallInstruction(type::Function const* fn_type, RegOr<Fn> const& fn,
                  PartialResultBuffer args, OutParams outs)
      : fn_type_(fn_type),
        fn_(fn),
        args_(std::move(args)),
        outs_(std::move(outs)) {
    ASSERT(args_.num_entries() ==
           fn_type_.as<type::Function>().parameters().size());
    ASSERT(this->outs_.size() ==
           fn_type_.as<type::Function>().return_types().size());
  }

  friend bool InterpretInstruction(interpreter::Interpreter& interpreter,
                                   CallInstruction const& inst);

  std::string to_string() const;
  friend std::ostream& operator<<(std::ostream& os,
                                  CallInstruction const& inst) {
    return os << inst.to_string();
  }

  friend void BaseTraverse(Inliner& inliner, CallInstruction& inst) {
    base::Traverse(inliner, inst.fn_, inst.args_);
    for (auto& reg : inst.outs_.regs()) { base::Traverse(inliner, reg); }
  }

  type::Function const* func_type() const {
    return &fn_type_.as<type::Function>();
  }
  RegOr<Fn> func() const { return fn_; }
  PartialResultBuffer const& arguments() const { return args_; }
  OutParams const& outputs() const { return outs_; }

 private:
  friend base::EnableExtensions;

  type::Type fn_type_;
  RegOr<Fn> fn_;
  PartialResultBuffer args_;
  OutParams outs_;
};

struct CommentInstruction
    : base::Extend<CommentInstruction>::With<base::BaseTraverseExtension,
                                             DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "comment: %1$s";

  template <typename ExecContext>
  void Apply(ExecContext&) const {}

  std::string comment;
};

}  // namespace ir

#endif  // ICARUS_IR_INSTRUCTION_CORE_H

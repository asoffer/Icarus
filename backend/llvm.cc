#include "backend/llvm.h"

#include "absl/container/flat_hash_map.h"
#include "backend/type.h"
#include "base/log.h"
#include "base/meta.h"
#include "ir/instruction/arithmetic.h"
#include "ir/instruction/compare.h"
#include "ir/instruction/instructions.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"

namespace backend {

LlvmEmitter::function_type *LlvmEmitter::DeclareFunction(
    ir::Subroutine const *fn, module::Linkage linkage,
    module_type &output_module) {
  return llvm::Function::Create(
      llvm::cast<llvm::FunctionType>(ToLlvmType(fn->type(), context_)),
      [&] {
        switch (linkage) {
          case module::Linkage::Internal: return llvm::Function::PrivateLinkage;
          case module::Linkage::External:
            return llvm::Function::ExternalLinkage;
        }
      }(),
      "fn", &output_module);
}

LlvmEmitter::LlvmEmitter(llvm::IRBuilder<> &builder, module_type *module)
    : Emitter<LlvmEmitter, LlvmBackendTraits>(module),
      builder_(builder),
      context_(builder.getContext()) {}

LlvmEmitter::basic_block_type *LlvmEmitter::DeclareBasicBlock(
    function_type &fn) {
  return llvm::BasicBlock::Create(context_, "block", &fn);
}

void LlvmEmitter::PrepareForStackAllocation(
    ir::Subroutine const &fn,
    const absl::flat_hash_map<ir::BasicBlock const *, basic_block_type *>
        &block_map) {
  PrepareForBasicBlockAppend(block_map.at(fn.entry()));
}

void LlvmEmitter::PrepareForBasicBlockAppend(basic_block_type *block) {
  builder_.SetInsertPoint(block);
}

LlvmEmitter::value_type *LlvmEmitter::StackAllocate(type::Type t) {
  return builder_.CreateAlloca(ToLlvmType(t, context_));
}

template <typename Inst>
bool EmitInstruction(LlvmEmitter &emitter, LlvmEmitter::context_type &context,
                     ir::Inst const &instruction) {
  static constexpr auto instruction_t = base::meta<Inst>;
  auto &inst                          = instruction.as<Inst>();
  if constexpr (instruction_t.template is_a<ir::AddInstruction>()) {
    using num_type = typename Inst::num_type;
    ASSIGN_OR(return false, auto &lhs, emitter.Resolve(inst.lhs, context));
    ASSIGN_OR(return false, auto &rhs, emitter.Resolve(inst.rhs, context));

    if constexpr (base::meta<num_type> == base::meta<float> or
                  base::meta<num_type> == base::meta<double>) {
      context.registers.emplace(inst.result,
                                emitter.builder().CreateAdd(&lhs, &rhs));
    } else if constexpr (std::is_signed_v<num_type>) {
      context.registers.emplace(inst.result,
                                emitter.builder().CreateNSWAdd(&lhs, &rhs));
    } else {
      context.registers.emplace(inst.result,
                                emitter.builder().CreateNUWAdd(&lhs, &rhs));
    }
  } else if constexpr (instruction_t.template is_a<ir::SubInstruction>()) {
    using num_type = typename Inst::num_type;
    ASSIGN_OR(return false, auto &lhs, emitter.Resolve(inst.lhs, context));
    ASSIGN_OR(return false, auto &rhs, emitter.Resolve(inst.rhs, context));

    if constexpr (base::meta<num_type> == base::meta<float> or
                  base::meta<num_type> == base::meta<double>) {
      context.registers.emplace(inst.result,
                                emitter.builder().CreateSub(&lhs, &rhs));
    } else if constexpr (std::is_signed_v<num_type>) {
      context.registers.emplace(inst.result,
                                emitter.builder().CreateNSWSub(&lhs, &rhs));
    } else {
      context.registers.emplace(inst.result,
                                emitter.builder().CreateNUWSub(&lhs, &rhs));
    }
  } else if constexpr (instruction_t.template is_a<ir::MulInstruction>()) {
    using num_type = typename Inst::num_type;
    ASSIGN_OR(return false, auto &lhs, emitter.Resolve(inst.lhs, context));
    ASSIGN_OR(return false, auto &rhs, emitter.Resolve(inst.rhs, context));

    if constexpr (base::meta<num_type> == base::meta<float> or
                  base::meta<num_type> == base::meta<double>) {
      context.registers.emplace(inst.result,
                                emitter.builder().CreateMul(&lhs, &rhs));
    } else if constexpr (std::is_signed_v<num_type>) {
      context.registers.emplace(inst.result,
                                emitter.builder().CreateNSWMul(&lhs, &rhs));
    } else {
      context.registers.emplace(inst.result,
                                emitter.builder().CreateNUWMul(&lhs, &rhs));
    }
  } else if constexpr (instruction_t.template is_a<ir::DivInstruction>()) {
    using num_type = typename Inst::num_type;
    ASSIGN_OR(return false, auto &lhs, emitter.Resolve(inst.lhs, context));
    ASSIGN_OR(return false, auto &rhs, emitter.Resolve(inst.rhs, context));

    if constexpr (base::meta<num_type> == base::meta<float> or
                  base::meta<num_type> == base::meta<double>) {
      context.registers.emplace(inst.result,
                                emitter.builder().CreateFDiv(&lhs, &rhs));
    } else if constexpr (std::is_signed_v<num_type>) {
      context.registers.emplace(inst.result,
                                emitter.builder().CreateSDiv(&lhs, &rhs));
    } else {
      context.registers.emplace(inst.result,
                                emitter.builder().CreateUDiv(&lhs, &rhs));
    }
  } else if constexpr (instruction_t.template is_a<ir::ModInstruction>()) {
    using num_type = typename Inst::num_type;
    ASSIGN_OR(return false, auto &lhs, emitter.Resolve(inst.lhs, context));
    ASSIGN_OR(return false, auto &rhs, emitter.Resolve(inst.rhs, context));

    if constexpr (std::is_signed_v<num_type>) {
      context.registers.emplace(inst.result,
                                emitter.builder().CreateSRem(&lhs, &rhs));
    } else {
      context.registers.emplace(inst.result,
                                emitter.builder().CreateURem(&lhs, &rhs));
    }
  } else if constexpr (instruction_t.template is_a<ir::LtInstruction>()) {
    using num_type = typename Inst::num_type;
    ASSIGN_OR(return false, auto &lhs, emitter.Resolve(inst.lhs, context));
    ASSIGN_OR(return false, auto &rhs, emitter.Resolve(inst.rhs, context));

    if constexpr (base::meta<num_type> == base::meta<float> or
                  base::meta<num_type> == base::meta<double>) {
      context.registers.emplace(inst.result,
                                emitter.builder().CreateFCmpULT(&lhs, &rhs));
    } else if constexpr (std::is_signed_v<num_type>) {
      context.registers.emplace(inst.result,
                                emitter.builder().CreateICmpSLT(&lhs, &rhs));
    } else {
      context.registers.emplace(inst.result,
                                emitter.builder().CreateICmpULT(&lhs, &rhs));
    }
  } else if constexpr (instruction_t.template is_a<ir::LeInstruction>()) {
    using num_type = typename Inst::num_type;
    ASSIGN_OR(return false, auto &lhs, emitter.Resolve(inst.lhs, context));
    ASSIGN_OR(return false, auto &rhs, emitter.Resolve(inst.rhs, context));

    if constexpr (base::meta<num_type> == base::meta<float> or
                  base::meta<num_type> == base::meta<double>) {
      context.registers.emplace(inst.result,
                                emitter.builder().CreateFCmpULE(&lhs, &rhs));
    } else if constexpr (std::is_signed_v<num_type>) {
      context.registers.emplace(inst.result,
                                emitter.builder().CreateICmpSLE(&lhs, &rhs));
    } else {
      context.registers.emplace(inst.result,
                                emitter.builder().CreateICmpULE(&lhs, &rhs));
    }
  } else if constexpr (instruction_t
                           .template is_a<ir::SetReturnInstruction>()) {
    ASSIGN_OR(return false, auto &value, emitter.Resolve(inst.value, context));
    if (inst.index == 0) {
      emitter.builder().CreateRet(&value);
    } else {
      NOT_YET("Not yet supporting multiple returns: index = ", inst.index);
    }
  } else if constexpr (instruction_t.template is_a<ir::StoreInstruction>()) {
    ASSIGN_OR(return false, auto &value, emitter.Resolve(inst.value, context));
    ASSIGN_OR(return false, auto &loc, emitter.Resolve(inst.location, context));
    emitter.builder().CreateStore(&value, &loc);
  } else if constexpr (instruction_t == base::meta<ir::LoadInstruction>) {
    ASSIGN_OR(return false, auto &addr, emitter.Resolve(inst.addr, context));
    context.registers.emplace(
        inst.result, emitter.builder().CreateLoad(
                         ToLlvmType(inst.type, emitter.context()), &addr));
  } else if constexpr (instruction_t == base::meta<ir::PtrIncrInstruction>) {
    context.registers.emplace(
        inst.result,
        emitter.builder().CreateGEP(ToLlvmType(inst.ptr, emitter.context()),
                                    emitter.Resolve(inst.addr, context),
                                    emitter.Resolve(inst.index, context)));
  } else if constexpr (instruction_t == base::meta<ir::CallInstruction>) {
    // TODO: support multiple outputs
    if (inst.outputs().size() > 1) { NOT_YET(); }
    std::vector<typename LlvmEmitter::value_type *> args;
    args.reserve(inst.arguments().num_entries());
    auto *fn_type   = inst.func_type();
    auto param_iter = fn_type->parameters().begin();
    NOT_YET();
    // for (auto const &arg : inst.arguments()) {
    //   arg.template apply<bool, ir::Char, int8_t, int16_t, int32_t, int64_t,
    //                      uint8_t, uint16_t, uint32_t, uint64_t, float,
    //                      double, ir::Reg, ir::addr_t, ir::Fn>([&](auto v) {
    //     using T = std::decay_t<decltype(v)>;
    //     if constexpr (base::meta<T> == base::meta<ir::Reg>) {
    //       if (param_iter->value.type().template is<type::Pointer>()) {
    //         args.push_back(emitter.Resolve<ir::addr_t>(v, context));
    //       } else {
    //         param_iter->value.type().template as<type::Primitive>().Apply(
    //             [&]<typename T>() {
    //               args.push_back(emitter.Resolve<T>(v, context));
    //             });
    //       }
    //     } else {
    //       args.push_back(emitter.Resolve<T>(v, context));
    //     }
    //   });
    //   ++param_iter;
    // }
    auto *result = emitter.builder().CreateCall(
        llvm::cast<llvm::FunctionType>(
            ToLlvmType(fn_type, emitter.builder().getContext())),
        emitter.Resolve(inst.func(), context), args);
    if (inst.outputs().size() == 1) {
      context.registers.emplace(inst.outputs()[0], result);
    }

  } else {
    static_assert(base::always_false(instruction_t));
  }

  return true;
}

template <typename... Insts>
absl::flat_hash_map<base::MetaValue,
                    bool (*)(LlvmEmitter &, LlvmEmitter::context_type &,
                             ir::Inst const &)> const &
InstructionMap() {
  static absl::flat_hash_map<
      base::MetaValue,
      bool (*)(LlvmEmitter &, LlvmEmitter::context_type &, ir::Inst const &)>
      inst_map = {{base::meta<Insts>, EmitInstruction<Insts>}...};
  return inst_map;
}

bool LlvmEmitter::EmitInstruction(ir::Inst const &instruction,
                                  context_type &context) {
  LOG("EmitInstruction", "%s", instruction);
  auto const &inst_map = InstructionMap<
      ir::AddInstruction<int8_t>, ir::AddInstruction<uint8_t>,
      ir::AddInstruction<int16_t>, ir::AddInstruction<uint16_t>,
      ir::AddInstruction<int32_t>, ir::AddInstruction<uint32_t>,
      ir::AddInstruction<int64_t>, ir::AddInstruction<uint64_t>,
      ir::AddInstruction<float>, ir::AddInstruction<double>,
      ir::SubInstruction<int8_t>, ir::SubInstruction<uint8_t>,
      ir::SubInstruction<int16_t>, ir::SubInstruction<uint16_t>,
      ir::SubInstruction<int32_t>, ir::SubInstruction<uint32_t>,
      ir::SubInstruction<int64_t>, ir::SubInstruction<uint64_t>,
      ir::SubInstruction<float>, ir::SubInstruction<double>,
      ir::MulInstruction<int8_t>, ir::MulInstruction<uint8_t>,
      ir::MulInstruction<int16_t>, ir::MulInstruction<uint16_t>,
      ir::MulInstruction<int32_t>, ir::MulInstruction<uint32_t>,
      ir::MulInstruction<int64_t>, ir::MulInstruction<uint64_t>,
      ir::MulInstruction<float>, ir::MulInstruction<double>,
      ir::DivInstruction<int8_t>, ir::DivInstruction<uint8_t>,
      ir::DivInstruction<int16_t>, ir::DivInstruction<uint16_t>,
      ir::DivInstruction<int32_t>, ir::DivInstruction<uint32_t>,
      ir::DivInstruction<int64_t>, ir::DivInstruction<uint64_t>,
      ir::DivInstruction<float>, ir::DivInstruction<double>,
      ir::ModInstruction<int8_t>, ir::ModInstruction<uint8_t>,
      ir::ModInstruction<int16_t>, ir::ModInstruction<uint16_t>,
      ir::ModInstruction<int32_t>, ir::ModInstruction<uint32_t>,
      ir::ModInstruction<int64_t>, ir::ModInstruction<uint64_t>,
      ir::LtInstruction<int8_t>, ir::LtInstruction<uint8_t>,
      ir::LtInstruction<int16_t>, ir::LtInstruction<uint16_t>,
      ir::LtInstruction<int32_t>, ir::LtInstruction<uint32_t>,
      ir::LtInstruction<int64_t>, ir::LtInstruction<uint64_t>,
      ir::LtInstruction<float>, ir::LtInstruction<double>,
      ir::LeInstruction<int8_t>, ir::LeInstruction<uint8_t>,
      ir::LeInstruction<int16_t>, ir::LeInstruction<uint16_t>,
      ir::LeInstruction<int32_t>, ir::LeInstruction<uint32_t>,
      ir::LeInstruction<int64_t>, ir::LeInstruction<uint64_t>,
      ir::LeInstruction<float>, ir::LeInstruction<double>,
      ir::SetReturnInstruction<bool>, ir::SetReturnInstruction<int8_t>,
      ir::SetReturnInstruction<uint8_t>, ir::SetReturnInstruction<int16_t>,
      ir::SetReturnInstruction<uint16_t>, ir::SetReturnInstruction<int32_t>,
      ir::SetReturnInstruction<uint32_t>, ir::SetReturnInstruction<int64_t>,
      ir::SetReturnInstruction<uint64_t>, ir::SetReturnInstruction<float>,
      ir::SetReturnInstruction<double>, ir::StoreInstruction<bool>,
      ir::StoreInstruction<ir::Char>, ir::StoreInstruction<int8_t>,
      ir::StoreInstruction<uint8_t>, ir::StoreInstruction<int16_t>,
      ir::StoreInstruction<uint16_t>, ir::StoreInstruction<int32_t>,
      ir::StoreInstruction<uint32_t>, ir::StoreInstruction<int64_t>,
      ir::StoreInstruction<uint64_t>, ir::StoreInstruction<float>,
      ir::StoreInstruction<double>, ir::LoadInstruction, ir::CallInstruction,
      ir::PtrIncrInstruction>();
  LOG("EmitInstruction", "Emitting LLVM IR for %s", instruction.to_string());
  if (auto iter = inst_map.find(instruction.rtti()); iter != inst_map.end()) {
    return iter->second(*this, context, instruction);
  } else {
    UNREACHABLE("Failed to find emitter for %s", instruction);
    return false;
  }
}

void LlvmEmitter::EmitBasicBlockJump(ir::BasicBlock const *block,
                                     context_type &context, bool returns_void) {
  builder_.SetInsertPoint(context.blocks.at(block));

  switch (block->jump().kind()) {
    case ir::JumpCmd::Kind::Return:
      if (returns_void) { builder_.CreateRetVoid(); }
      return;
    case ir::JumpCmd::Kind::Uncond:
      builder_.CreateBr(context.blocks.at(block->jump().UncondTarget()));
      return;
    case ir::JumpCmd::Kind::Cond:
      builder_.CreateCondBr(context.registers.at(block->jump().CondReg()),
                            context.blocks.at(block->jump().CondTarget(true)),
                            context.blocks.at(block->jump().CondTarget(false)));
      return;
    default: UNREACHABLE();
  }
}

}  // namespace backend

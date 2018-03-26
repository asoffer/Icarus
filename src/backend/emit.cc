#include "backend/emit.h"

#include <string>
#include <unordered_map>
#include <vector>

#include "../architecture.h"
#include "ir/func.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "type/all.h"

// TODO remove dependence on printf, malloc, free
//
// TODO worry about concurrent access as well as modularity (global? per
// module?)
static llvm::Value *StringConstant(llvm::IRBuilder<> &builder,
                                   const std::string &str) {
  static std::unordered_map<std::string, llvm::Value *> global_strs;
  auto &result = global_strs[str];
  if (!result) { result = builder.CreateGlobalStringPtr(str); }
  return result;
}

namespace backend {
static llvm::Value *EmitValue(
    llvm::LLVMContext &ctx, llvm::IRBuilder<> &builder, const IR::Val &val,
    const std::unordered_map<IR::Register, llvm::Value *> &regs,
    const std::vector<llvm::BasicBlock *> &llvm_blocks) {
  return std::visit(
      base::overloaded{
          [&](IR::Register reg) -> llvm::Value * {
            // TODO still need to be sure these are read in the correct order
            return regs.at(reg);
          },
          [&](IR::ReturnValue ret) -> llvm::Value * { NOT_YET(); },
          [&](IR::Addr addr) -> llvm::Value * { NOT_YET(); },
          [&](bool b) -> llvm::Value * {
            return llvm::ConstantInt::get(ctx,
                                          llvm::APInt(1, b ? 1 : 0, false));
          },
          [&](char c) -> llvm::Value * {
            return llvm::ConstantInt::get(ctx, llvm::APInt(8, c, false));
          },
          [&](double d) -> llvm::Value * { NOT_YET(); },
          [&](i32 n) -> llvm::Value * {
            return llvm::ConstantInt::get(ctx, llvm::APInt(32, n, true));
          },
          [&](IR::EnumVal e) -> llvm::Value * {
            return llvm::ConstantInt::get(ctx, llvm::APInt(32, e.value, true));
          },
          [&](const type::Type *t) -> llvm::Value * { 
            // TODO this is probably a bad idea?
            return llvm::ConstantInt::get(
                ctx, llvm::APInt(64, reinterpret_cast<uintptr_t>(t), false));
          },
          [&](IR::Func *f) -> llvm::Value * { return f->llvm_fn_; },
          [&](AST::ScopeLiteral *s) -> llvm::Value * { NOT_YET(); },
          [&](const AST::CodeBlock &c) -> llvm::Value * { NOT_YET(); },
          [&](AST::Expression *e) -> llvm::Value * { NOT_YET(); },
          [&](IR::BlockIndex b) -> llvm::Value * { NOT_YET(); },
          [&](const std::string &s) -> llvm::Value * {
            // TODO this is wrong because strings aren't char*, but maybe it's
            // okay and we can do promotion later?
            return StringConstant(builder, s);
          },
          [&](AST::FunctionLiteral *fn) -> llvm::Value * { NOT_YET(); }},
      val.value);
}

static llvm::Value *EmitCmd(
    llvm::Module *module, llvm::IRBuilder<> &builder, const IR::Cmd &cmd,
    const std::unordered_map<IR::Register, llvm::Value *> &regs,
    const std::vector<llvm::BasicBlock *> &llvm_blocks) {
  auto &ctx = module->getContext();
  switch (cmd.op_code_) {
    case IR::Op::Alloca:
      return builder.CreateAlloca(
          cmd.type->as<type::Pointer>().pointee->llvm(ctx));
    case IR::Op::Store:
      return builder.CreateStore(
          EmitValue(ctx, builder, cmd.args[0], regs, llvm_blocks),
          EmitValue(ctx, builder, cmd.args[1], regs, llvm_blocks));
    case IR::Op::Load:
      return builder.CreateLoad(
          EmitValue(ctx, builder, cmd.args[0], regs, llvm_blocks));
#define ARITHMETIC_CASE(op, llvm_float, llvm_int)                        \
  case IR::Op::op: {                                                     \
    auto *lhs = EmitValue(ctx, builder, cmd.args[0], regs, llvm_blocks); \
    auto *rhs = EmitValue(ctx, builder, cmd.args[1], regs, llvm_blocks); \
    if (cmd.type == type::Real) {                                        \
      return builder.llvm_float(lhs, rhs);                               \
    } else {                                                             \
      return builder.llvm_int(lhs, rhs);                                 \
    }                                                                    \
  } break
      ARITHMETIC_CASE(Add, CreateFAdd, CreateAdd);
      ARITHMETIC_CASE(Sub, CreateFSub, CreateSub);
      ARITHMETIC_CASE(Mul, CreateFMul, CreateMul);
      ARITHMETIC_CASE(Div, CreateFDiv, CreateSDiv);
#undef ARITHMETIC_CASE
    case IR::Op::Mod:
      return builder.CreateSRem(
          EmitValue(ctx, builder, cmd.args[0], regs, llvm_blocks),
          EmitValue(ctx, builder, cmd.args[1], regs, llvm_blocks));
    // TODO support fmod, or stop supporting elsewhere
    case IR::Op::Neg:
      if (cmd.type == type::Bool) {
        return builder.CreateNot(
            EmitValue(ctx, builder, cmd.args[0], regs, llvm_blocks));
      } else {
        return builder.CreateNeg(
            EmitValue(ctx, builder, cmd.args[0], regs, llvm_blocks));
      }
    case IR::Op::Or:
      return builder.CreateOr(
          EmitValue(ctx, builder, cmd.args[0], regs, llvm_blocks),
          EmitValue(ctx, builder, cmd.args[1], regs, llvm_blocks));
    case IR::Op::And:
      return builder.CreateAnd(
          EmitValue(ctx, builder, cmd.args[0], regs, llvm_blocks),
          EmitValue(ctx, builder, cmd.args[1], regs, llvm_blocks));
    case IR::Op::Xor:
      return builder.CreateXor(
          EmitValue(ctx, builder, cmd.args[0], regs, llvm_blocks),
          EmitValue(ctx, builder, cmd.args[1], regs, llvm_blocks));
    case IR::Op::UncondJump:
      // TODO use EmitValue?
      return builder.CreateBr(
          llvm_blocks[std::get<IR::BlockIndex>(cmd.args[0].value).value]);
    case IR::Op::CondJump:
      return builder.CreateCondBr(
          EmitValue(ctx, builder, cmd.args[0], regs, llvm_blocks),
          llvm_blocks[std::get<IR::BlockIndex>(cmd.args[1].value).value],
          llvm_blocks[std::get<IR::BlockIndex>(cmd.args[2].value).value]);
    case IR::Op::ReturnJump:
      return builder.CreateRetVoid();
    // TODO not always void
    case IR::Op::Trunc:
      return builder.CreateTrunc(
          EmitValue(ctx, builder, cmd.args[0], regs, llvm_blocks),
          llvm::Type::getInt8Ty(ctx), "trunc");
    case IR::Op::Extend:
      return builder.CreateTrunc(
          EmitValue(ctx, builder, cmd.args[0], regs, llvm_blocks),
          llvm::Type::getInt32Ty(ctx), "ext");
    case IR::Op::Print: {
      auto *printf_fn = module->getOrInsertFunction(
          "printf",
          llvm::FunctionType::get(
              llvm::Type::getInt32Ty(ctx),
              llvm::PointerType::get(llvm::Type::getInt8Ty(ctx), 0), true));
      for (const auto &arg : cmd.args) {
        if (arg.type == type::Bool) {
          NOT_YET();

        } else if (arg.type == type::Char) {
          return builder.CreateCall(
              printf_fn, {StringConstant(builder, "%c"),
                          EmitValue(ctx, builder, arg, regs, llvm_blocks)},
              "print");
        } else if (arg.type == type::Int) {
          return builder.CreateCall(
              printf_fn, {StringConstant(builder, "%d"),
                          EmitValue(ctx, builder, arg, regs, llvm_blocks)},
              "print");
        } else if (arg.type == type::Real) {
          return builder.CreateCall(
              printf_fn, {StringConstant(builder, "%f"),
                          EmitValue(ctx, builder, arg, regs, llvm_blocks)},
              "print");
        } else if (arg.type == type::String) {
          // TODO this is wrong because strings aren't char*
          return builder.CreateCall(
              printf_fn, {StringConstant(builder, "%s"),
                          EmitValue(ctx, builder, arg, regs, llvm_blocks)});
        } else {
          LOG << arg.type;
          NOT_YET();
        }
      }
    } break;
    case IR::Op::Malloc: {
      // TODO cast from void*?
      auto *malloc_fn = module->getOrInsertFunction(
          "malloc",
          llvm::FunctionType::get(llvm::Type::getVoidTy(ctx)->getPointerTo(0),
                                  llvm::Type::getInt32Ty(ctx), false));
      // TODO this is wrong for cross-compilation
      auto target_architecture = Architecture::CompilingMachine();
      return builder.CreateCall(
          malloc_fn,
          {llvm::ConstantInt::get(
              ctx, llvm::APInt(64, target_architecture.bytes(
                                       cmd.type->as<type::Pointer>().pointee),
                               false))});
    } break;
    case IR::Op::Free: {
      // TODO cast to void* first?
      auto *free_fn = module->getOrInsertFunction(
          "free", llvm::FunctionType::get(
                      llvm::Type::getVoidTy(ctx),
                      llvm::Type::getVoidTy(ctx)->getPointerTo(0), false));
      return builder.CreateCall(
          free_fn, {EmitValue(ctx, builder, cmd.args[0], regs, llvm_blocks)});
    } break;
    case IR::Op::Call: {
      std::vector<llvm::Value *> values;
      values.reserve(cmd.args.size());
      for (const auto& arg : cmd.args) {
        values.push_back(EmitValue(ctx, builder, arg, regs, llvm_blocks));
      }
      llvm::Value *fn = values.back();
      values.pop_back();
      return builder.CreateCall(fn, values);
    } break;
    case IR::Op::Lt: {
      auto *lhs = EmitValue(ctx, builder, cmd.args[0], regs, llvm_blocks);
      auto *rhs = EmitValue(ctx, builder, cmd.args[1], regs, llvm_blocks);
      // Correct for enum flags?
      // TODO ordered vs unordered
      return (cmd.type == type::Real) ? builder.CreateFCmpOLT(lhs, rhs)
                                      : builder.CreateICmpSLT(lhs, rhs);
    }
    case IR::Op::Le: {
      auto *lhs = EmitValue(ctx, builder, cmd.args[0], regs, llvm_blocks);
      auto *rhs = EmitValue(ctx, builder, cmd.args[1], regs, llvm_blocks);
      // Correct for enum flags?
      // TODO ordered vs unordered
      return (cmd.type == type::Real) ? builder.CreateFCmpOLE(lhs, rhs)
                                      : builder.CreateICmpSLE(lhs, rhs);
    }
    case IR::Op::Eq: {
      auto *lhs = EmitValue(ctx, builder, cmd.args[0], regs, llvm_blocks);
      auto *rhs = EmitValue(ctx, builder, cmd.args[1], regs, llvm_blocks);
      if (cmd.type == type::Int || cmd.type == type::Char ||
          cmd.type->is<type::Enum>()) {
        return builder.CreateICmpEQ(lhs, rhs);
      } else if (cmd.type == type::Real) {
        // TODO ordered vs unordered
        return builder.CreateFCmpOEQ(lhs, rhs);
      } else {
        NOT_YET(cmd.type);
      }
    }
    case IR::Op::Ne: {
      auto *lhs = EmitValue(ctx, builder, cmd.args[0], regs, llvm_blocks);
      auto *rhs = EmitValue(ctx, builder, cmd.args[1], regs, llvm_blocks);
      if (cmd.type == type::Int || cmd.type == type::Char ||
          cmd.type->is<type::Enum>()) {
        return builder.CreateICmpNE(lhs, rhs);
      } else if (cmd.type == type::Real) {
        // TODO ordered vs unordered
        return builder.CreateFCmpONE(lhs, rhs);
      } else {
        NOT_YET(cmd.type);
      }
    }
    case IR::Op::Ge: {
      auto *lhs = EmitValue(ctx, builder, cmd.args[0], regs, llvm_blocks);
      auto *rhs = EmitValue(ctx, builder, cmd.args[1], regs, llvm_blocks);
      // Correct for enum flags?
      // TODO ordered vs unordered
      return (cmd.type == type::Real) ? builder.CreateFCmpOGE(lhs, rhs)
                                      : builder.CreateICmpSGE(lhs, rhs);
    }
    case IR::Op::Gt: {
      auto *lhs = EmitValue(ctx, builder, cmd.args[0], regs, llvm_blocks);
      auto *rhs = EmitValue(ctx, builder, cmd.args[1], regs, llvm_blocks);
      // Correct for enum flags?
      // TODO ordered vs unordered
      return (cmd.type == type::Real) ? builder.CreateFCmpOGT(lhs, rhs)
                                      : builder.CreateICmpSGT(lhs, rhs);
    }
    case IR::Op::ArrayLength:
      // TODO use a struct gep on a generic array llvm struct type holding an
      // int and a void*.
      return builder.CreatePointerCast(
          EmitValue(ctx, builder, cmd.args[0], regs, llvm_blocks),
          llvm::Type::getInt32Ty(ctx)->getPointerTo(0));
    case IR::Op::ArrayData:
      // TODO use a struct gep on a generic array llvm struct type holding an
      // int and a void*. Then cast to actual type.
      return builder.CreatePointerCast(
          builder.CreateGEP(
              builder.CreatePointerCast(
                  EmitValue(ctx, builder, cmd.args[0], regs, llvm_blocks),
                  llvm::Type::getInt32Ty(ctx)->getPointerTo(0)),
              llvm::ConstantInt::get(ctx, llvm::APInt(32, 1, false))),
          cmd.type->llvm(ctx));
    case IR::Op::VariantType:
      // TODO 64-bit int for type? Maybe more type-safety would be nice.
      return builder.CreatePointerCast(
          EmitValue(ctx, builder, cmd.args[0], regs, llvm_blocks),
          llvm::Type::getInt64Ty(ctx)->getPointerTo(0));
    case IR::Op::VariantValue:
      // TODO use a struct gep on a generic array llvm struct type holding an
      // int and a void*. Then cast to actual type.
      return builder.CreatePointerCast(
          builder.CreateGEP(
              builder.CreatePointerCast(
                  EmitValue(ctx, builder, cmd.args[0], regs, llvm_blocks),
                  llvm::Type::getInt64Ty(ctx)->getPointerTo(0)),
              llvm::ConstantInt::get(ctx, llvm::APInt(64, 1, false))),
          cmd.type->llvm(ctx));
    case IR::Op::Phi: {
      auto *phi = builder.CreatePHI(cmd.type->llvm(ctx), cmd.args.size() / 2);
      for (size_t i = 0; i < cmd.args.size(); i += 2) {
        phi->addIncoming(
            EmitValue(ctx, builder, cmd.args[i], regs, llvm_blocks),
            llvm_blocks[std::get<IR::BlockIndex>(cmd.args[i + 1].value).value]);
      }
      return phi;
    } break;
    case IR::Op::Field:
      return builder.CreateStructGEP(
          cmd.args[0]
              .type->as<type::Pointer>()
              .pointee->as<type::Struct>()
              .llvm(ctx),
          EmitValue(ctx, builder, cmd.args[0], regs, llvm_blocks),
          static_cast<u32>(std::get<i32>(cmd.args[1].value)));
    case IR::Op::PtrIncr:
      return builder.CreateGEP(
          EmitValue(ctx, builder, cmd.args[0], regs, llvm_blocks),
          EmitValue(ctx, builder, cmd.args[1], regs, llvm_blocks));
    case IR::Op::SetReturn: NOT_YET();
    case IR::Op::Cast:
      NOT_YET();  // TODO this isn't even usable yet, so perhaps we just want to
                  // gut it?
    case IR::Op::CreateStruct: UNREACHABLE();
    case IR::Op::InsertField: UNREACHABLE();
    case IR::Op::FinalizeStruct: UNREACHABLE();
    case IR::Op::Variant: UNREACHABLE();
    case IR::Op::Arrow: UNREACHABLE();
    case IR::Op::Array: UNREACHABLE();
    case IR::Op::Ptr: UNREACHABLE();
    case IR::Op::Err: UNREACHABLE();
    case IR::Op::Contextualize: UNREACHABLE();
  }
  UNREACHABLE();
}

void EmitAll(const std::vector<std::unique_ptr<IR::Func>> &fns,
             llvm::Module *module) {
  auto &ctx = module->getContext();
  for (auto &fn : fns) {
    fn->llvm_fn_ = llvm::Function::Create(
        fn->type_->llvm_fn(ctx), llvm::Function::PrivateLinkage, "", module);
  }

  llvm::IRBuilder<> builder(ctx);
  for (auto &fn : fns) {
    std::vector<llvm::BasicBlock *> llvm_blocks;
    llvm_blocks.reserve(fn->blocks_.size());
    for (const auto &block : fn->blocks_) {
      llvm_blocks.push_back(
          llvm::BasicBlock::Create(ctx, "block", fn->llvm_fn_));
    }

    std::unordered_map<IR::Register, llvm::Value *> regs;
    for (size_t i = 0; i < fn->blocks_.size(); ++i) {
      builder.SetInsertPoint(llvm_blocks[i]);
      for (const auto &cmd : fn->blocks_[i].cmds_) {
        regs[cmd.result] = EmitCmd(module, builder, cmd, regs, llvm_blocks);
      }
    }
  }
}
}  // namespace backend

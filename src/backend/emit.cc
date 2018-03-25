#include "backend/emit.h"

#include <string>
#include <unordered_map>
#include <vector>

#include "ir/func.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "type/all.h"

// TODO remove dependence on printf
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
          [&](IR::EnumVal e) -> llvm::Value * { NOT_YET(); },
          [&](const type::Type *t) -> llvm::Value * { NOT_YET(); },
          [&](IR::Func *f) -> llvm::Value * { NOT_YET(); },
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
    case IR::Op::ReturnJump:
      return builder.CreateRetVoid();
    // TODO not always void
    case IR::Op::Print:
      for (const auto &arg : cmd.args) {
        auto *printf_fn = module->getOrInsertFunction(
            "printf",
            llvm::FunctionType::get(
                llvm::IntegerType::getInt32Ty(ctx),
                llvm::PointerType::get(llvm::Type::getInt8Ty(ctx), 0), true));
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
                          EmitValue(ctx, builder, arg, regs, llvm_blocks)},
              "print");

        } else {
          LOG << arg.type;
          NOT_YET();
        }
      }
      break;
    case IR::Op::Lt: {
      auto *lhs = EmitValue(ctx, builder, cmd.args[0], regs, llvm_blocks);
      auto *rhs = EmitValue(ctx, builder, cmd.args[1], regs, llvm_blocks);
      if (cmd.type == type::Int) {
        return builder.CreateICmpSLT(lhs, rhs);
      } else if (cmd.type == type::Real) {
        // TODO ordered vs unordered
        return builder.CreateFCmpOLT(lhs, rhs);
      } else {
        NOT_YET();
      }
    }
    case IR::Op::Le: {
      auto *lhs = EmitValue(ctx, builder, cmd.args[0], regs, llvm_blocks);
      auto *rhs = EmitValue(ctx, builder, cmd.args[1], regs, llvm_blocks);
      if (cmd.type == type::Int) {
        return builder.CreateICmpSLE(lhs, rhs);
      } else if (cmd.type == type::Real) {
        // TODO ordered vs unordered
        return builder.CreateFCmpOLE(lhs, rhs);
      } else {
        NOT_YET();
      }
    }
    case IR::Op::Eq: {
      auto *lhs = EmitValue(ctx, builder, cmd.args[0], regs, llvm_blocks);
      auto *rhs = EmitValue(ctx, builder, cmd.args[1], regs, llvm_blocks);
      if (cmd.type == type::Int) {
        return builder.CreateICmpEQ(lhs, rhs);
      } else if (cmd.type == type::Real) {
        // TODO ordered vs unordered
        return builder.CreateFCmpOEQ(lhs, rhs);
      } else {
        NOT_YET();
      }
    }
    case IR::Op::Ne: {
      auto *lhs = EmitValue(ctx, builder, cmd.args[0], regs, llvm_blocks);
      auto *rhs = EmitValue(ctx, builder, cmd.args[1], regs, llvm_blocks);
      if (cmd.type == type::Int) {
        return builder.CreateICmpNE(lhs, rhs);
      } else if (cmd.type == type::Real) {
        // TODO ordered vs unordered
        return builder.CreateFCmpONE(lhs, rhs);
      } else {
        NOT_YET();
      }
    }
    case IR::Op::Ge: {
      auto *lhs = EmitValue(ctx, builder, cmd.args[0], regs, llvm_blocks);
      auto *rhs = EmitValue(ctx, builder, cmd.args[1], regs, llvm_blocks);
      if (cmd.type == type::Int) {
        return builder.CreateICmpSGE(lhs, rhs);
      } else if (cmd.type == type::Real) {
        // TODO ordered vs unordered
        return builder.CreateFCmpOGE(lhs, rhs);
      } else {
        NOT_YET();
      }
    }
    case IR::Op::Gt: {
      auto *lhs = EmitValue(ctx, builder, cmd.args[0], regs, llvm_blocks);
      auto *rhs = EmitValue(ctx, builder, cmd.args[1], regs, llvm_blocks);
      if (cmd.type == type::Int) {
        return builder.CreateICmpSGT(lhs, rhs);
      } else if (cmd.type == type::Real) {
        // TODO ordered vs unordered
        return builder.CreateFCmpOGT(lhs, rhs);
      } else {
        NOT_YET();
      }
    }
    case IR::Op::CreateStruct: UNREACHABLE();
    case IR::Op::InsertField: UNREACHABLE();
    case IR::Op::FinalizeStruct: UNREACHABLE();
    case IR::Op::Variant: UNREACHABLE();
    case IR::Op::Arrow: UNREACHABLE();
    case IR::Op::Array: UNREACHABLE();
    case IR::Op::Ptr: UNREACHABLE();
    case IR::Op::Err: UNREACHABLE();
    case IR::Op::Contextualize: UNREACHABLE();
    default: cmd.dump(0); NOT_YET();
  }
  UNREACHABLE();
}

void Emit(const IR::Func &fn, llvm::Module *module) {
  auto &ctx = module->getContext();
  auto *llvm_fn = llvm::Function::Create(
      llvm::FunctionType::get(llvm::Type::getVoidTy(ctx), {}, false),
      llvm::Function::InternalLinkage, "f", module);

  llvm::IRBuilder<> builder(ctx);

  std::vector<llvm::BasicBlock *> llvm_blocks;
  llvm_blocks.reserve(fn.blocks_.size());
  for (const auto &block : fn.blocks_) {
    llvm_blocks.push_back(llvm::BasicBlock::Create(ctx, "block", llvm_fn));
  }

  std::unordered_map<IR::Register, llvm::Value *> regs;
  for (size_t i = 0; i < fn.blocks_.size(); ++i) {
    builder.SetInsertPoint(llvm_blocks[i]);
    for (const auto &cmd : fn.blocks_[i].cmds_) {
      regs[cmd.result] = EmitCmd(module, builder, cmd, regs, llvm_blocks);
    }
  }
}
}  // namespace backend

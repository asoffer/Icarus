#include "backend/emit.h"

#include <string>
#include <unordered_map>
#include <vector>

#include "ir/func.h"
#include "type/all.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"

// TODO remove dependence on printf
//
// TODO worry about concurrent access as well as modularity (global? per
// module?)
static llvm::Value *StringConstant(llvm::IRBuilder<> &builder,
                                      const std::string &str) {
  static std::unordered_map<std::string, llvm::Value *> global_strs;
  auto& result = global_strs[str];
  if (!result) {
    result = builder.CreateGlobalStringPtr(str);
  }
  return result;
}

namespace backend {
static llvm::Value *
EmitValue(llvm::LLVMContext &ctx, llvm::IRBuilder<> &builder,
          const IR::Val &val,
          const std::vector<llvm::BasicBlock *> &llvm_blocks) {
  return std::visit(
      base::overloaded{
          [&](IR::Register reg) -> llvm::Value * { NOT_YET(); },
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

static void EmitCmd(llvm::Module* module, llvm::IRBuilder<> &builder,
                    const IR::Cmd &cmd,
                    const std::vector<llvm::BasicBlock *> &llvm_blocks) {
  switch (cmd.op_code_) {
  case IR::Op::UncondJump:
    builder.CreateBr(
        llvm_blocks[std::get<IR::BlockIndex>(cmd.args[0].value).value]);
    break;
  case IR::Op::ReturnJump:
    builder.CreateRetVoid();
    // TODO not always void
    break;
  case IR::Op::Print:
    for (const auto &arg : cmd.args) {
      auto &ctx = module->getContext();
      auto *printf_fn = module->getOrInsertFunction(
          "printf",
          llvm::FunctionType::get(
              llvm::IntegerType::getInt32Ty(ctx),
              llvm::PointerType::get(llvm::Type::getInt8Ty(ctx), 0), true));
      if (arg.type == type::Bool) {
        NOT_YET();

      } else if (arg.type == type::Char) {
        builder.CreateCall(printf_fn,
                           {StringConstant(builder, "%c"),
                            EmitValue(ctx, builder, arg, llvm_blocks)},
                           "print");
      } else if (arg.type == type::Int) {
        builder.CreateCall(printf_fn,
                           {StringConstant(builder, "%d"),
                            EmitValue(ctx, builder, arg, llvm_blocks)},
                           "print");
      } else if (arg.type == type::Real) {
        builder.CreateCall(printf_fn,
                           {StringConstant(builder, "%f"),
                            EmitValue(ctx, builder, arg, llvm_blocks)},
                           "print");
      } else if (arg.type == type::String) {
        // TODO this is wrong because strings aren't char*
        builder.CreateCall(printf_fn,
                           {StringConstant(builder, "%s"),
                            EmitValue(ctx, builder, arg, llvm_blocks)},
                           "print");

      } else {
        LOG << arg.type;
        NOT_YET();
      }
    }
    break;
  default:
    cmd.dump(0);
    NOT_YET();
  }
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

  for (size_t i = 0; i < fn.blocks_.size(); ++i) {
    builder.SetInsertPoint(llvm_blocks[i]);
    for (const auto &cmd : fn.blocks_[i].cmds_) {
      EmitCmd(module, builder, cmd, llvm_blocks);
    }
  }
}
} // namespace backend

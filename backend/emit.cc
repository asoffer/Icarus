#ifdef ICARUS_USE_LLVM
#include "backend/emit.h"

#include <string>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "architecture.h"
#include "ast/function_literal.h"
#include "base/check.h"
#include "ir/compiled_fn.h"
#include "llvm/ir/DerivedTypes.h"
#include "llvm/ir/IRBuilder.h"
#include "llvm/ir/Module.h"

// TODO remove dependence on printf, malloc, free
//
// TODO worry about concurrent access as well as modularity (global? per
// module?)
static llvm::Value *StringConstant(llvm::IRBuilder<> *builder,
                                   std::string_view str) {
  static absl::flat_hash_map<std::string, llvm::Value *> global_strs;
  auto &result = global_strs[std::string(str)];
  if (!result) {
    result =
        builder->CreateGlobalStringPtr(llvm::StringRef{str.data(), str.size()});
  }
  return result;
}

namespace backend {
namespace {
using ::matcher::InheritsFrom;

struct LlvmData {
  llvm::Function *fn;
  llvm::Module *module;
  llvm::IRBuilder<> *builder;
  absl::flat_hash_map<ir::Reg, llvm::Value *> regs;
  std::vector<llvm::BasicBlock *> blocks;
  std::vector<llvm::Value *> rets;
};
}  // namespace

static llvm::Value *EmitValue(size_t num_args, LlvmData *llvm_data,
                              const ir::Val &val) {
  return std::visit(
      base::overloaded{
          [&](ir::Reg reg) -> llvm::Value * {
            if (static_cast<size_t>(reg.value) < num_args) {
              return llvm_data->fn->arg_begin() + reg.value;
            }

            // TODO still need to be sure these are read in the correct order
            return llvm_data->regs.at(reg);
          },
          [&](ir::Addr addr) -> llvm::Value * {
            switch (addr.kind) {
              case ir::Addr::Kind::Null:
                ASSERT(val.type, InheritsFrom<type::Pointer>());
                return llvm::ConstantPointerNull::get(
                    val.type->as<type::Pointer>().llvm_ptr(
                        llvm_data->module->getContext()));
              case ir::Addr::Kind::Stack: NOT_YET();
              case ir::Addr::Kind::Heap: NOT_YET();
            }
            UNREACHABLE();
          },
          [&](bool b) -> llvm::Value * {
            return llvm::ConstantInt::get(llvm_data->module->getContext(),
                                          llvm::APInt(1, b ? 1 : 0, false));
          },
          [&](char c) -> llvm::Value * {
            return llvm::ConstantInt::get(llvm_data->module->getContext(),
                                          llvm::APInt(8, c, false));
          },
          [&](double d) -> llvm::Value * {
            return llvm::ConstantFP::get(
                llvm::Type::getDoubleTy(llvm_data->module->getContext()), d);
          },
          [&](int32_t n) -> llvm::Value * {
            return llvm::ConstantInt::get(llvm_data->module->getContext(),
                                          llvm::APInt(32, n, true));
          },
          [&](ir::EnumVal e) -> llvm::Value * {
            return llvm::ConstantInt::get(llvm_data->module->getContext(),
                                          llvm::APInt(32, e.value, true));
          },
          [&](ir::FlagsVal e) -> llvm::Value * {
            return llvm::ConstantInt::get(llvm_data->module->getContext(),
                                          llvm::APInt(32, e.value, true));
          },
          [&](const type::Type *t) -> llvm::Value * {
            // TODO this is probably a bad idea?
            return llvm::ConstantInt::get(
                llvm_data->module->getContext(),
                llvm::APInt(64, reinterpret_cast<uintptr_t>(t), false));
          },
          [&](ir::CompiledFn *f) -> llvm::Value * {
            return llvm_data->module->getOrInsertFunction(
                f->name(), f->type_->llvm_fn(llvm_data->module->getContext()));
          },
          [&](ast::ScopeLiteral *s) -> llvm::Value * { NOT_YET(); },
          [&](ast::Expression *e) -> llvm::Value * { NOT_YET(); },
          [&](ir::Interface b) -> llvm::Value * { UNREACHABLE(); },
          [&](ir::BlockIndex b) -> llvm::Value * { NOT_YET(); },
          [&](std::string_view s) -> llvm::Value * {
            // TODO this is wrong because strings aren't char*, but maybe it's
            // okay and we can do promotion later?
            return StringConstant(llvm_data->builder, s);
          },
          [&](const ir::BlockSequence &bs) -> llvm::Value * { NOT_YET(); },
            NOT_YET();
          },
          [&](const ir::Foreign &f) -> llvm::Value * { NOT_YET(); }
          /*
          [&](ast::FunctionLiteral *fn) -> llvm::Value * {
            return llvm_data->module->getOrInsertFunction(
                fn->ir_func_->name(), fn->ir_func_->type_->llvm_fn(
                                          llvm_data->module->getContext()));
          }*/
      },
      val.value);
}

static llvm::Value *EmitCmd(const type::Function *fn_type, LlvmData *llvm_data,
                            const ir::Cmd &cmd) {
  size_t num_args = fn_type->input.size();
  size_t num_rets = fn_type->output.size();

  auto &ctx = llvm_data->module->getContext();
  switch (cmd.op_code_) {
    case ir::Op::Alloca:
      return llvm_data->builder->CreateAlloca(cmd.alloca_.type_->llvm(ctx));
    case ir::Op::Store:
      // TODO in the case of a function, we are given the declaration but we
      // actually need to extract the corresponding function pointer.
      return llvm_data->builder->CreateStore(
          EmitValue(num_args, llvm_data, cmd.args[0]),
          EmitValue(num_args, llvm_data, cmd.args[1]));
    case ir::Op::Load:
      return llvm_data->builder->CreateLoad(
          EmitValue(num_args, llvm_data, cmd.args[0]));
#define ARITHMETIC_CASE(op, llvm_float, llvm_int)                              \
  case ir::Op::op: {                                                           \
    auto *lhs = EmitValue(num_args, llvm_data, cmd.args[0]);                   \
    auto *rhs = EmitValue(num_args, llvm_data, cmd.args[1]);                   \
    if (cmd.type == type::Float32 || cmd.type == type::Float64) {              \
      return llvm_data->builder->llvm_float(lhs, rhs);                         \
    } else {                                                                   \
      return llvm_data->builder->llvm_int(lhs, rhs);                           \
    }                                                                          \
  } break
      ARITHMETIC_CASE(Add, CreateFAdd, CreateAdd);
      ARITHMETIC_CASE(Sub, CreateFSub, CreateSub);
      ARITHMETIC_CASE(Mul, CreateFMul, CreateMul);
      ARITHMETIC_CASE(Div, CreateFDiv, CreateSDiv);
#undef ARITHMETIC_CASE
    case ir::Op::Mod:
      return llvm_data->builder->CreateSRem(
          EmitValue(num_args, llvm_data, cmd.args[0]),
          EmitValue(num_args, llvm_data, cmd.args[1]));
    // TODO support fmod, or stop supporting elsewhere
    case ir::Op::Not:
      return llvm_data->builder->CreateNot(
          EmitValue(num_args, llvm_data, cmd.args[0]));
    case ir::Op::NegInt:
    case ir::Op::NegFloat32:
    case ir::Op::NegFloat64:
      return llvm_data->builder->CreateNeg(
          EmitValue(num_args, llvm_data, cmd.args[0]));
    case ir::Op::Or:
      return llvm_data->builder->CreateOr(
          EmitValue(num_args, llvm_data, cmd.args[0]),
          EmitValue(num_args, llvm_data, cmd.args[1]));
    case ir::Op::And:
      return llvm_data->builder->CreateAnd(
          EmitValue(num_args, llvm_data, cmd.args[0]),
          EmitValue(num_args, llvm_data, cmd.args[1]));
    case ir::Op::Xor:
      return llvm_data->builder->CreateXor(
          EmitValue(num_args, llvm_data, cmd.args[0]),
          EmitValue(num_args, llvm_data, cmd.args[1]));
    case ir::Op::UncondJump:
      // TODO use EmitValue?
      return llvm_data->builder->CreateBr(
          llvm_data->blocks[std::get<ir::BlockIndex>(cmd.args[0].value).value]);
    case ir::Op::CondJump:
      return llvm_data->builder->CreateCondBr(
          EmitValue(num_args, llvm_data, cmd.args[0]),
          llvm_data->blocks[std::get<ir::BlockIndex>(cmd.args[1].value).value],
          llvm_data->blocks[std::get<ir::BlockIndex>(cmd.args[2].value).value]);
    case ir::Op::ReturnJump:
      if (num_rets == 1 && !fn_type->output.at(0)->is_big()) {
        llvm_data->builder->CreateRet(
            llvm_data->builder->CreateLoad(llvm_data->rets.at(0)));
      } else {
        llvm_data->builder->CreateRetVoid();
      }
      return nullptr;
    case ir::Op::Print: {
      auto *printf_fn = llvm_data->module->getOrInsertFunction(
          "printf",
          llvm::FunctionType::get(
              llvm::Type::getInt32Ty(ctx),
              llvm::PointerType::get(llvm::Type::getInt8Ty(ctx), 0), true));
      for (const auto &arg : cmd.args) {
        if (arg.type == type::Bool) {
          return llvm_data->builder->CreateCall(
              printf_fn,
              {StringConstant(llvm_data->builder, "%s"),
               llvm_data->builder->CreateSelect(
                   EmitValue(num_args, llvm_data, arg),
                   StringConstant(llvm_data->builder, "true"),
                   StringConstant(llvm_data->builder, "false"))},
              "print");
        } else if (arg.type == type::Int32) {
          return llvm_data->builder->CreateCall(
              printf_fn,
              {StringConstant(llvm_data->builder, "%d"),
               EmitValue(num_args, llvm_data, arg)},
              "print");
        } else if (arg.type == type::Float32 || arg.type == type::Float64) {
          return llvm_data->builder->CreateCall(
              printf_fn,
              {StringConstant(llvm_data->builder, "%f"),
               EmitValue(num_args, llvm_data, arg)},
              "print");
        } else if (arg.type == type::ByteView) {
          // TODO this is wrong because strings aren't char*
          return llvm_data->builder->CreateCall(
              printf_fn, {StringConstant(llvm_data->builder, "%s"),
                          EmitValue(num_args, llvm_data, arg)});
        } else if (arg.type->is<type::Pointer>()) {
          return llvm_data->builder->CreateCall(
              printf_fn, {StringConstant(llvm_data->builder, "0x%016x"),
                          EmitValue(num_args, llvm_data, arg)});
        } else {
          NOT_YET(arg.type);
        }
      }
    } break;
    case ir::Op::Call: {
      std::vector<llvm::Value *> values;
      values.reserve(cmd.args.size());
      for (const auto &arg : cmd.args) {
        values.push_back(EmitValue(num_args, llvm_data, arg));
      }
      llvm::Value *fn = values.back();
      values.pop_back();
      return llvm_data->builder->CreateCall(fn, values);
    } break;
    case ir::Op::Lt: {
      auto *lhs = EmitValue(num_args, llvm_data, cmd.args[0]);
      auto *rhs = EmitValue(num_args, llvm_data, cmd.args[1]);
      // Correct for enum flags?
      // TODO ordered vs unordered
      return (cmd.args[0].type == type::Real)
                 ? llvm_data->builder->CreateFCmpOLT(lhs, rhs)
                 : llvm_data->builder->CreateICmpSLT(lhs, rhs);
    }
    case ir::Op::Le: {
      auto *lhs = EmitValue(num_args, llvm_data, cmd.args[0]);
      auto *rhs = EmitValue(num_args, llvm_data, cmd.args[1]);
      // Correct for enum flags?
      // TODO ordered vs unordered
      return (cmd.args[0].type == type::Real)
                 ? llvm_data->builder->CreateFCmpOLE(lhs, rhs)
                 : llvm_data->builder->CreateICmpSLE(lhs, rhs);
    }
    case ir::Op::Eq: {
      auto *lhs = EmitValue(num_args, llvm_data, cmd.args[0]);
      auto *rhs = EmitValue(num_args, llvm_data, cmd.args[1]);
      if (cmd.args[0].type == type::Int8 cmd.args[0].type == type::Int16 ||
          cmd.args[0].type == type::Int32 || cmd.args[0].type == type::Int64 ||
          cmd.args[0].type == type::Nat8 || cmd.args[0].type == type::Nat16 ||
          cmd.args[0].type == type::Nat32 || cmd.args[0].type == type::Nat64 ||
          cmd.args[0].type->is<type::Enum>() ||
          cmd.args[0].type->is<type::Flags>() ||
          cmd.args[0].type->is<type::Pointer>() ||
          cmd.args[0].type == type::Type_) {
        return llvm_data->builder->CreateICmpEQ(lhs, rhs);
      } else if (cmd.args[0].type == type::Real) {
        // TODO ordered vs unordered
        return llvm_data->builder->CreateFCmpOEQ(lhs, rhs);
      } else {
        NOT_YET(cmd.args[0].type, " vs ", cmd.args[1].type);
      }
    }
    case ir::Op::Ne: {
      auto *lhs = EmitValue(num_args, llvm_data, cmd.args[0]);
      auto *rhs = EmitValue(num_args, llvm_data, cmd.args[1]);
      if (cmd.args[0].type == type::Int8 cmd.args[0].type == type::Int16 ||
          cmd.args[0].type == type::Int32 || cmd.args[0].type == type::Int64 ||
          cmd.args[0].type == type::Nat8 || cmd.args[0].type == type::Nat16 ||
          cmd.args[0].type == type::Nat32 || cmd.args[0].type == type::Nat64 ||
          cmd.args[0].type->is<type::Enum>() ||
          cmd.args[0].type->is<type::Flags>() ||
          cmd.args[0].type->is<type::Pointer>() ||
          cmd.args[0].type == type::Type_) {
        return llvm_data->builder->CreateICmpNE(lhs, rhs);
      } else if (cmd.args[0].type == type::Real) {
        // TODO ordered vs unordered
        return llvm_data->builder->CreateFCmpONE(lhs, rhs);
      } else {
        NOT_YET(cmd.args[0].type, " vs ", cmd.args[1].type);
      }
    }
    case ir::Op::Ge: {
      auto *lhs = EmitValue(num_args, llvm_data, cmd.args[0]);
      auto *rhs = EmitValue(num_args, llvm_data, cmd.args[1]);
      // Correct for enum flags?
      // TODO ordered vs unordered
      return (cmd.args[0].type == type::Real)
                 ? llvm_data->builder->CreateFCmpOGE(lhs, rhs)
                 : llvm_data->builder->CreateICmpSGE(lhs, rhs);
    }
    case ir::Op::Gt: {
      auto *lhs = EmitValue(num_args, llvm_data, cmd.args[0]);
      auto *rhs = EmitValue(num_args, llvm_data, cmd.args[1]);
      // Correct for enum flags?
      // TODO ordered vs unordered
      return (cmd.args[0].type == type::Real)
                 ? llvm_data->builder->CreateFCmpOGT(lhs, rhs)
                 : llvm_data->builder->CreateICmpSGT(lhs, rhs);
    }
    case ir::Op::VariantType:
      // TODO 64-bit int for type? Maybe more type-safety would be nice.
      return llvm_data->builder->CreatePointerCast(
          EmitValue(num_args, llvm_data, cmd.args[0]),
          llvm::Type::getInt64Ty(ctx)->getPointerTo(0));
    case ir::Op::VariantValue:
      // TODO use a struct gep on a generic array llvm struct type holding an
      // int and a void*. Then cast to actual type.
      return llvm_data->builder->CreatePointerCast(
          llvm_data->builder->CreateGEP(
              llvm_data->builder->CreatePointerCast(
                  EmitValue(num_args, llvm_data, cmd.args[0]),
                  llvm::Type::getInt64Ty(ctx)->getPointerTo(0)),
              llvm::ConstantInt::get(ctx, llvm::APInt(64, 1, false))),
          cmd.type->llvm(ctx));
    case ir::Op::Phi:
      // We have to skip the phi->addIncoming here because the values may not
      // yet have been seen from a future basic block.
      return llvm_data->builder->CreatePHI(cmd.type->llvm(ctx),
                                           cmd.args.size() / 2);
    case ir::Op::Field:
      return llvm_data->builder->CreateStructGEP(
          cmd.args[0]
              .type->as<type::Pointer>()
              .pointee->as<type::Struct>()
              .llvm(ctx),
          EmitValue(num_args, llvm_data, cmd.args[0]),
          static_cast<uint32_t>(std::get<int32_t>(cmd.args[1].value)));
    case ir::Op::PtrIncr:
      return llvm_data->builder->CreateGEP(
          EmitValue(num_args, llvm_data, cmd.args[0]),
          EmitValue(num_args, llvm_data, cmd.args[1]));
    case ir::Op::SetReturn:
      return llvm_data->builder->CreateStore(
          EmitValue(num_args, llvm_data, cmd.args[1]),
          EmitValue(num_args, llvm_data, cmd.args[0]));
    case ir::Op::Cast: {
      if (cmd.args[0].type == cmd.type) {
        return EmitValue(num_args, llvm_data, cmd.args[0]);
      } else if (cmd.args[0].type == type::Int32 && cmd.type == type::Real) { // TODO other sizes
        return llvm_data->builder->CreateSIToFP(
            EmitValue(num_args, llvm_data, cmd.args[0]), type::Real->llvm(ctx));
      } else if (cmd.args[0].type->is<type::Pointer>() &&
                 cmd.args[0]
                     .type->as<type::Pointer>()
                     .pointee->is<type::Array>() &&
                 cmd.type->is<type::Pointer>()) {
        // TODO check that it's an array of the same type as the pointed to
        // thing?
        auto *zero = llvm::ConstantInt::get(llvm_data->module->getContext(),
                                            llvm::APInt(32, 0, false));
        return llvm_data->builder->CreateGEP(
            EmitValue(num_args, llvm_data, cmd.args[0]), {zero, zero});
      }
      UNREACHABLE(cmd);
    } break;
    case ir::Op::Align: UNREACHABLE();
    case ir::Op::Bytes: UNREACHABLE();
    case ir::Op::Tup: UNREACHABLE();
    case ir::Op::CreateStruct: UNREACHABLE();
    case ir::Op::CreateStructField: UNREACHABLE();
    case ir::Op::SetStructFieldName: UNREACHABLE();
    case ir::Op::FinalizeStruct: UNREACHABLE();
    case ir::Op::Variant: UNREACHABLE();
    case ir::Op::Arrow: UNREACHABLE();
    case ir::Op::Array: UNREACHABLE();
    case ir::Op::Ptr: UNREACHABLE();
  }
  UNREACHABLE();
}

void EmitAll(const std::vector<std::unique_ptr<ir::CompiledFn>> &fns,
             llvm::Module *module) {
  auto &ctx = module->getContext();
  llvm::IRBuilder<> builder(ctx);

  for (auto &fn : fns) {
    LlvmData llvm_data;
    llvm_data.module  = module;
    llvm_data.builder = &builder;
    llvm_data.fn      = fn->llvm_fn_;

    llvm_data.blocks.reserve(fn->blocks_.size());

    for (const auto &block : fn->blocks_) {
      llvm_data.blocks.push_back(
          llvm::BasicBlock::Create(ctx, "block", fn->llvm_fn_));
    }

    builder.SetInsertPoint(llvm_data.blocks[0]);
    if (fn->type_->output.size() == 1 && !fn->type_->output[0]->is_big()) {
      // TODO Something feels super duper fishy here. Why do we need an
      // allocation?
      llvm_data.rets.push_back(
          builder.CreateAlloca(fn->type_->output[0]->llvm(ctx)));
    } else {
      llvm_data.rets.reserve(fn->type_->output.size());
      auto arg_iter = llvm_data.fn->arg_begin() + fn->type_->input.size();
      for (size_t i = 0; i < fn->type_->output.size(); ++i) {
        llvm_data.rets.push_back(arg_iter);
        ++arg_iter;
      }
    }

    for (size_t i = 0; i < fn->blocks_.size(); ++i) {
      builder.SetInsertPoint(llvm_data.blocks[i]);
      for (const auto &cmd : fn->blocks_.at(i).cmds_) {
        auto cmd_result            = EmitCmd(fn->type_, &llvm_data, cmd);
        llvm_data.regs[cmd.result] = cmd_result;
        if (cmd_result == nullptr) { break; }
      }
    }

    for (size_t i = 0; i < fn->blocks_.size(); ++i) {
      for (const auto &cmd : fn->blocks_[i].cmds_) {
        if (cmd.op_code_ != ir::Op::Phi) { continue; }
        llvm::Value *phi = llvm_data.regs[cmd.result];
        for (size_t i = 0; i < cmd.args.size(); i += 2) {
          llvm::cast<llvm::PHINode>(phi)->addIncoming(
              EmitValue(fn->type_->output.size(), &llvm_data, cmd.args[i + 1]),
              llvm_data
                  .blocks[std::get<ir::BlockIndex>(cmd.args[i].value).value]);
        }
      }
    }
  }
}

}  // namespace backend
#endif  // ICARUS_USE_LLVM

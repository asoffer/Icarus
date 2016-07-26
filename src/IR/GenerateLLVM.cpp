#include "IR/IR.h"
#include "Type/Type.h"
#include "Stack.h"

extern llvm::IRBuilder<> builder;
extern llvm::BasicBlock *make_block(const std::string &name,
                                    llvm::Function *fn);

extern const char *GetGlobalStringNumbered(size_t index);

namespace cstdlib {
extern llvm::Constant *printf();
extern llvm::Constant *malloc();
extern llvm::Constant *free();
extern llvm::Constant *memcpy();
} // namespace cstdlib

namespace data {
extern llvm::Constant *null(const Type *t);
extern llvm::ConstantInt *const_bool(bool b);
extern llvm::ConstantInt *const_uint(size_t n);
extern llvm::ConstantInt *const_uint32(size_t n);
extern llvm::ConstantInt *const_int(long n);
extern llvm::ConstantInt *const_char(char c);
extern llvm::ConstantFP *const_real(double d);
extern llvm::Value *global_string(const std::string &s);
} // namespace data

namespace IR {
extern std::vector<llvm::Constant *> LLVMGlobals;
static llvm::Value *IR_to_LLVM(IR::Func *ir_fn, IR::Value cmd_arg,
                               const std::vector<llvm::Value *> &registers) {

  switch (cmd_arg.flag) {
  case ValType::B: return data::const_bool(cmd_arg.as_bool);
  case ValType::C: return data::const_char(cmd_arg.as_char);
  case ValType::I: return data::const_int(cmd_arg.as_int);
  case ValType::R: return data::const_real(cmd_arg.as_real);
  case ValType::U: return data::const_uint(cmd_arg.as_uint);
  case ValType::Null: return data::null(cmd_arg.as_null);
  case ValType::T:
    // TODO is this what I want? Used for just a few ops like print.
    return reinterpret_cast<llvm::Value *>(cmd_arg.as_type);
  case ValType::F:
    return cmd_arg.as_func->llvm_fn;
  case ValType::CStr:
    return data::global_string(std::string(cmd_arg.as_cstr));
  case ValType::Block:
    return reinterpret_cast<llvm::Value *>(cmd_arg.as_block);
  case ValType::Reg:
    return registers[cmd_arg.as_reg];
  case ValType::Arg: {
    auto arg_num = (long)cmd_arg.as_arg;
    auto iter = ir_fn->llvm_fn->args().begin();
    while (arg_num --> 0) { iter++; }
    return iter;
  } break;
  case ValType::StackAddr: ir_fn->dump(); UNREACHABLE;
  case ValType::FrameAddr:
    assert(ir_fn->frame_map.find(cmd_arg.as_frame_addr) != ir_fn->frame_map.end());
    return ir_fn->frame_map[cmd_arg.as_frame_addr];
  case ValType::GlobalAddr:
    return LLVMGlobals[cmd_arg.as_global_addr];
  case ValType::HeapAddr: NOT_YET;
  case ValType::GlobalCStr:
    return data::global_string(GetGlobalStringNumbered(cmd_arg.as_global_cstr));
  }
}

static void CompletePhiDefinition(IR::Func *ir_fn, IR::Block *block,
                                  size_t cmd_index,
                                  std::vector<llvm::Value *> &registers) {
  auto &cmd = block->cmds[cmd_index];
  auto reg = (llvm::PHINode *)registers[cmd.result.reg];
  for (size_t i = 0; i < cmd.args.size(); i += 2) {
    assert(cmd.args[i].flag == ValType::Block);
    assert(cmd.args[i].as_block);
    auto val = IR_to_LLVM(ir_fn, cmd.args[i + 1], registers);
    assert(val);
    reg->addIncoming(val, cmd.args[i].as_block->llvm_block);
  }
}

void Func::GenerateLLVM() {
  if (generated != Gen::NotYet) { return; }
  std::vector<llvm::BasicBlock *> llvm_blocks(blocks.size(), nullptr);
  llvm_fn->setName(name);

  for (const auto &b : blocks) {
    if (!b) { continue; }
    b->llvm_block = make_block(b->block_name, llvm_fn);
  }

  std::vector<llvm::Value *> registers(num_cmds, nullptr);
  std::vector<std::pair<IR::Block *, size_t>> phis;
  for (size_t i = 0; i < blocks.size(); ++i) {
    llvm_blocks[i] = blocks[i]->GenerateLLVM(this, registers, phis);
  }

  for (auto phi_loc_pair : phis) {
    CompletePhiDefinition(this, phi_loc_pair.first, phi_loc_pair.second,
                          registers);
  }

  builder.SetInsertPoint(alloc_block);
  assert(!llvm_blocks.empty());
  builder.CreateBr(llvm_blocks[0]);
  generated = Gen::Done;
}

llvm::BasicBlock *
Block::GenerateLLVM(IR::Func *ir_fn, std::vector<llvm::Value *> &registers,
                    std::vector<std::pair<IR::Block *, size_t>> &phis) {
  assert(llvm_block);

  builder.SetInsertPoint(llvm_block);

  // NOTE this is larger than necessary but definitely big enough.
  // TODO get the size just right when we know it.
  {
    size_t cmd_counter = ~0ul;
    for (const auto &cmd : cmds) {
      ++cmd_counter;
      switch (cmd.op_code) {
      case IR::Op::Field: {
        assert(cmd.args[2].flag == IR::ValType::U);
        registers[cmd.result.reg] = builder.CreateGEP(
            IR_to_LLVM(ir_fn, cmd.args[1], registers),
            {data::const_uint32(0), data::const_uint32(cmd.args[2].as_uint)});
      } continue;
      case IR::Op::Phi: {
        registers[cmd.result.reg] = builder.CreatePHI(
            *cmd.result.type, (unsigned int)cmd.args.size() >> 1, "phi");
        phis.emplace_back(this, cmd_counter);
      } continue;
      case IR::Op::TC_Ptr: {
        Type *pointee = nullptr;
        if (cmd.args[0].flag == ValType::T) {
          pointee = cmd.args[0].as_type;
        } else if (cmd.args[0].flag == ValType::Reg) {
          pointee = reinterpret_cast<Type *>(registers[cmd.args[0].as_reg]);
        } else {
          UNREACHABLE;
        }
        assert(pointee);

        registers[cmd.result.reg] =
            reinterpret_cast<llvm::Value *>(Ptr(pointee));
      } continue;
      case IR::Op::TC_Arrow: {
        Type *from = nullptr;
        if (cmd.args[0].flag == ValType::T) {
          from = cmd.args[0].as_type;
        } else if (cmd.args[0].flag == ValType::Reg) {
          from = reinterpret_cast<Type *>(registers[cmd.args[0].as_reg]);
        } else {
          UNREACHABLE;
        }
        assert(from);

        Type *to = nullptr;
        if (cmd.args[1].flag == ValType::T) {
          to = cmd.args[1].as_type;
        } else if (cmd.args[1].flag == ValType::Reg) {
          to = reinterpret_cast<Type *>(registers[cmd.args[1].as_reg]);
        } else {
          UNREACHABLE;
        }
        assert(to);

        registers[cmd.result.reg] =
            reinterpret_cast<llvm::Value *>(::Func(from, to));

      } continue;
      case IR::Op::TC_Tup: {
                             UNREACHABLE;

      } continue;
      case IR::Op::TC_Arr1: {
        Type *data_type = nullptr;
        if (cmd.args[0].flag == ValType::T) {
          data_type = cmd.args[0].as_type;
        } else if (cmd.args[0].flag == ValType::Reg) {
          data_type = reinterpret_cast<Type *>(registers[cmd.args[0].as_reg]);
        } else {
          dump();
          UNREACHABLE;
        }
        assert(data_type);

        registers[cmd.result.reg] =
            reinterpret_cast<llvm::Value *>(::Arr(data_type));
      } continue;
      case IR::Op::TC_Arr2: {
        size_t length = 0;
        if (cmd.args[0].flag == ValType::I) {
          length = (size_t)cmd.args[0].as_int;
        } else if (cmd.args[0].flag == ValType::U) {
          length = cmd.args[0].as_uint;
        } else {
          UNREACHABLE;
        }

        Type *data_type = nullptr;
        if (cmd.args[1].flag == ValType::T) {
          data_type = cmd.args[1].as_type;
        } else if (cmd.args[1].flag == ValType::Reg) {
          data_type = reinterpret_cast<Type *>(registers[cmd.args[1].as_reg]);

        } else {
          UNREACHABLE;
        }
        assert(data_type);

        registers[cmd.result.reg] =
            reinterpret_cast<llvm::Value *>(Arr(data_type, length));
      } continue;
      case IR::Op::Bytes: 
      case IR::Op::Alignment: {
        Type *t = nullptr;
        if (cmd.args[0].flag == ValType::T) {
          t = cmd.args[0].as_type;
        } else if (cmd.args[0].flag == ValType::Reg) {
          t = reinterpret_cast<Type *>(registers[cmd.args[0].as_reg]);

        } else {
          const_cast<Cmd &>(cmd).dump(0);
          UNREACHABLE;
        }
        assert(t);
        registers[cmd.result.reg] = data::const_uint(
            cmd.op_code == IR::Op::Bytes ? t->bytes() : t->alignment());
      } continue;
      case IR::Op::Call: {
        if (cmd.result.type == Type_) {
          auto local_stack = new IR::LocalStack;

          IR::Func *fn;
          if (cmd.args[0].flag == IR::ValType::T) {
            assert(cmd.args[0].as_type->is_parametric_struct());
            auto param_struct = ((ParametricStructure *)cmd.args[0].as_type);
            assert(param_struct->ast_expression);
            assert(param_struct->ast_expression->ir_func);
            fn = param_struct->ast_expression->ir_func;
          } else {
            assert(cmd.args[0].flag == IR::ValType::F);
            fn = cmd.args[0].as_func;
          }

          std::vector<IR::Value> cmd_args;
          for (size_t i = 1; i < cmd.args.size(); ++i) {
            cmd_args.push_back(cmd.args[i]);
          }
          auto result = fn->Call(local_stack, cmd_args);
          delete local_stack;
          registers[cmd.result.reg] = IR_to_LLVM(ir_fn, result, registers);

          continue;
        } else {
          break;
        }

      } UNREACHABLE;
      default:;
      }

      std::vector<llvm::Value *> args(cmd.args.size(), nullptr);
      for (size_t i = 0; i < cmd.args.size(); ++i) {
        args[i] = IR_to_LLVM(ir_fn, cmd.args[i], registers);
      }

      switch (cmd.op_code) {
      case IR::Op::BNot:
        registers[cmd.result.reg] = builder.CreateNot(args[0]);
        break;
      case IR::Op::INeg:
        registers[cmd.result.reg] = builder.CreateNeg(args[0]);
        break;
      case IR::Op::FNeg:
        registers[cmd.result.reg] = builder.CreateFNeg(args[0]);
        break;

      case IR::Op::CAdd:
      case IR::Op::IAdd:
      case IR::Op::UAdd:
        registers[cmd.result.reg] = builder.CreateAdd(args[0], args[1]);
        break;

      case IR::Op::FAdd:
        registers[cmd.result.reg] = builder.CreateFAdd(args[0], args[1]);
        break;

      case IR::Op::ISub:
      case IR::Op::USub:
        registers[cmd.result.reg] = builder.CreateSub(args[0], args[1]);
        break;
      case IR::Op::FSub:
        registers[cmd.result.reg] = builder.CreateFSub(args[0], args[1]);
        break;

      case IR::Op::IMul:
      case IR::Op::UMul:
        registers[cmd.result.reg] = builder.CreateMul(args[0], args[1]);
        break;
      case IR::Op::FMul:
        registers[cmd.result.reg] = builder.CreateFMul(args[0], args[1]);
        break;

      case IR::Op::IDiv:
        registers[cmd.result.reg] = builder.CreateSDiv(args[0], args[1]);
        break;
      case IR::Op::UDiv:
        registers[cmd.result.reg] = builder.CreateUDiv(args[0], args[1]);
        break;
      case IR::Op::FDiv:
        registers[cmd.result.reg] = builder.CreateFDiv(args[0], args[1]);
        break;

      case IR::Op::IMod:
        registers[cmd.result.reg] = builder.CreateSRem(args[0], args[1]);
        break;
      case IR::Op::UMod:
        registers[cmd.result.reg] = builder.CreateURem(args[0], args[1]);
        break;
      case IR::Op::FMod:
        registers[cmd.result.reg] = builder.CreateFRem(args[0], args[1]);
        break;

      case IR::Op::BOr:
        registers[cmd.result.reg] = builder.CreateOr(args[0], args[1]);
        break;
      case IR::Op::BXor:
        registers[cmd.result.reg] = builder.CreateXor(args[0], args[1]);
        break;

      case IR::Op::ILT:
        registers[cmd.result.reg] = builder.CreateICmpSLT(args[0], args[1]);
        break;
      case IR::Op::ULT:
        registers[cmd.result.reg] = builder.CreateICmpULT(args[0], args[1]);
        break;
      case IR::Op::FLT:
        registers[cmd.result.reg] = builder.CreateFCmpOLT(args[0], args[1]);
        break;
      case IR::Op::ILE:
        registers[cmd.result.reg] = builder.CreateICmpSLE(args[0], args[1]);
        break;
      case IR::Op::ULE:
        registers[cmd.result.reg] = builder.CreateICmpULE(args[0], args[1]);
        break;
      case IR::Op::FLE:
        registers[cmd.result.reg] = builder.CreateFCmpOLE(args[0], args[1]);
        break;

      case IR::Op::TEQ:
        registers[cmd.result.reg] =
            data::const_bool(reinterpret_cast<Type *>(args[0]) ==
                             reinterpret_cast<Type *>(args[1]));
        break;
      case IR::Op::FnEQ: NOT_YET;

      case IR::Op::UEQ:
      case IR::Op::IEQ:
      case IR::Op::CEQ:
      case IR::Op::BEQ:
      case IR::Op::PtrEQ:
        registers[cmd.result.reg] = builder.CreateICmpEQ(args[0], args[1]);
        break;
      case IR::Op::FEQ:
        registers[cmd.result.reg] = builder.CreateFCmpOEQ(args[0], args[1]);
        break;

      case IR::Op::TNE:
        registers[cmd.result.reg] =
            data::const_bool(reinterpret_cast<Type *>(args[0]) !=
                             reinterpret_cast<Type *>(args[1]));
        break;
      case IR::Op::FnNE: NOT_YET;

      case IR::Op::BNE:
      case IR::Op::CNE:
      case IR::Op::INE:
      case IR::Op::UNE:
        registers[cmd.result.reg] = builder.CreateICmpNE(args[0], args[1]);
        break;
      case IR::Op::FNE:
        registers[cmd.result.reg] = builder.CreateFCmpONE(args[0], args[1]);
        break;

      case IR::Op::IGE:
        registers[cmd.result.reg] = builder.CreateICmpSGE(args[0], args[1]);
        break;
      case IR::Op::UGE:
        registers[cmd.result.reg] = builder.CreateICmpUGE(args[0], args[1]);
        break;
      case IR::Op::FGE:
        registers[cmd.result.reg] = builder.CreateFCmpOGE(args[0], args[1]);
        break;
      case IR::Op::CGT:
      case IR::Op::IGT:
        registers[cmd.result.reg] = builder.CreateICmpSGT(args[0], args[1]);
        break;
      case IR::Op::UGT:
        registers[cmd.result.reg] = builder.CreateICmpUGT(args[0], args[1]);
        break;
      case IR::Op::FGT:
        registers[cmd.result.reg] = builder.CreateFCmpOGT(args[0], args[1]);
        break;

      case IR::Op::Malloc:
        registers[cmd.result.reg] = builder.CreateBitCast(
            builder.CreateCall(cstdlib::malloc(), args[0]), *cmd.result.type);
        break;
      case IR::Op::Free:
        builder.CreateCall(cstdlib::free(),
                           builder.CreateBitCast(args[0], *RawPtr));
        break;
      case IR::Op::ArrayLength:
        registers[cmd.result.reg] = builder.CreateGEP(
            args[0], {data::const_uint32(0), data::const_uint32(0)});
        break;

      case IR::Op::ArrayData:
        registers[cmd.result.reg] = builder.CreateGEP(
            args[1], {data::const_uint32(0), data::const_uint32(1)});
        break;

      case IR::Op::Print: {
        auto print_type = reinterpret_cast<Type *>(args[0]);
        if (print_type == Bool) {
          auto to_show =
              builder.CreateSelect(args[1], data::global_string("true"),
                                   data::global_string("false"));
          builder.CreateCall(cstdlib::printf(),
                             {data::global_string("%s"), to_show});
        } else if (print_type == Char) {
          builder.CreateCall(cstdlib::printf(),
                             {data::global_string("%c"), args[1]});
        } else if (print_type == Int) {
          builder.CreateCall(cstdlib::printf(),
                             {data::global_string("%ld"), args[1]});
        } else if (print_type == Real) {
          builder.CreateCall(cstdlib::printf(),
                             {data::global_string("%f"), args[1]});
        } else if (print_type == Uint) {
          builder.CreateCall(cstdlib::printf(),
                             {data::global_string("%lu"), args[1]});
        } else if (print_type == Type_) {
          auto type_to_print = reinterpret_cast<Type *>(args[1]);
          builder.CreateCall(
              cstdlib::printf(),
              {data::global_string("%s"),
               data::global_string(type_to_print->to_string().c_str())});
        } else if (print_type->is_pointer()) {
          builder.CreateCall(cstdlib::printf(),
                             {data::global_string("0x%zx"), args[1]});
        } else if (print_type->is_enum()) {
          auto enum_type = (Enum *)print_type;
          // TODO show names not numbers
          builder.CreateCall(
              cstdlib::printf(),
              {data::global_string(enum_type->to_string() + ".%d"), args[1]});
        } else {
          UNREACHABLE;
        }
      } break;
      case IR::Op::Call: {
        auto fn = args[0];
        args.erase(args.begin());

        auto ret_type = cmd.result.type;
        if (ret_type == Void) {
          builder.CreateCall(fn, args);
        } else if (ret_type->is_primitive() || ret_type->is_pointer() ||
                   ret_type->is_enum()) {
          registers[cmd.result.reg] = builder.CreateCall(fn, args);
        } else if (ret_type->is_tuple()) {
            NOT_YET;
        } else {
          auto ret_val = builder.CreateAlloca(*ret_type);
          args.push_back(ret_val);
          registers[cmd.result.reg] = ret_val;
        }

      } break;
      case IR::Op::Load:
        if (cmd.result.type->is_function()) {
          registers[cmd.result.reg] = args[0];
        } else {
          registers[cmd.result.reg] = builder.CreateLoad(args[0]);
        }
        break;
      case IR::Op::Store: {
        // TODO or do we want to actually do the store (it'll be easily
        // optimized out)
        if (reinterpret_cast<Type *>(args[0]) == Type_) { break; }
        builder.CreateStore(args[1], args[2]);
      } break;
      case IR::Op::Cast: {
        auto from_type = reinterpret_cast<Type *>(args[0]);
        auto to_type   = reinterpret_cast<Type *>(args[1]);

        if (from_type == to_type) { registers[cmd.result.reg] = args[2]; }

        if (from_type == Bool) {
          registers[cmd.result.reg] =
              (to_type == Real)
                  ? builder.CreateUIToFP(args[2], *to_type, "ext.val")
                  : builder.CreateZExt(args[2], *to_type, "ext.val");
        } else if (from_type == Int) {
          registers[cmd.result.reg] =
              (to_type == Real)
                  ? builder.CreateSIToFP(args[2], *to_type, "fp.val")
                  : args[2];
        } else if (from_type == Uint) {
          registers[cmd.result.reg] =
              (to_type == Real)
                  ? builder.CreateUIToFP(args[2], *to_type, "fp.val")
                  : args[2];
        } else if (from_type->is_pointer() && to_type->is_pointer()) {
          registers[cmd.result.reg] = builder.CreateBitCast(args[2], *to_type);
        }
      } break;
      case IR::Op::NOp: break;
      case IR::Op::PtrIncr:
        registers[cmd.result.reg] = builder.CreateGEP(args[1], args[2]);
        break;
      case IR::Op::Access: {
        registers[cmd.result.reg] = builder.CreateGEP(
            builder.CreateBitCast(args[2], *cmd.result.type), args[1]);
      } break;
      case IR::Op::InitFieldVec: UNREACHABLE;
      case IR::Op::PushField: UNREACHABLE;
      case IR::Op::CreateStruct: UNREACHABLE;
      case IR::Op::GetFromCache: UNREACHABLE;
      case IR::Op::Field: UNREACHABLE;
      case IR::Op::Phi: UNREACHABLE;
      case IR::Op::TC_Tup: UNREACHABLE;
      case IR::Op::TC_Ptr: UNREACHABLE;
      case IR::Op::TC_Arrow: UNREACHABLE;
      case IR::Op::TC_Arr1: UNREACHABLE;
      case IR::Op::TC_Arr2: UNREACHABLE;
      case IR::Op::Bytes: UNREACHABLE;
      case IR::Op::Alignment: UNREACHABLE;

      case IR::Op::Trunc:
        registers[cmd.result.reg] = builder.CreateTrunc(args[0], *Char);
        break;
      case IR::Op::ZExt:
        registers[cmd.result.reg] = builder.CreateZExt(args[0], *Uint);
        break;
      case IR::Op::Memcpy: builder.CreateCall(cstdlib::memcpy(), args); break;
     }
    }
  }

  exit->GenerateLLVM(ir_fn, registers);
  return llvm_block;
}

static llvm::ConstantInt *LLVMConstant(Value val) {
  switch (val.flag) {
    case ValType::B: return data::const_bool(val.as_bool);
    case ValType::C: return data::const_char(val.as_char);
    case ValType::I: return data::const_int(val.as_int);
    case ValType::U: return data::const_uint(val.as_uint);
    default: UNREACHABLE;
  }
}

void Exit::Switch::GenerateLLVM(
    Func *fn, const std::vector<llvm::Value *> &registers) {
  auto switch_stmt = builder.CreateSwitch(IR_to_LLVM(fn, cond, registers),
                                          default_block->llvm_block,
                                          (unsigned int)table.size());

  for (auto entry : table) {
    switch_stmt->addCase(LLVMConstant(entry.first), entry.second->llvm_block);
  }
}

void Exit::Unconditional::GenerateLLVM(
    Func *fn, const std::vector<llvm::Value *> &registers) {
  builder.CreateBr(block->llvm_block);
}

void Exit::Conditional::GenerateLLVM(
    Func *fn, const std::vector<llvm::Value *> &registers) {
  builder.CreateCondBr(IR_to_LLVM(fn, cond, registers), true_block->llvm_block,
                       false_block->llvm_block);
}
void Exit::Return::GenerateLLVM(Func *fn,
                                const std::vector<llvm::Value *> &registers) {
  if (fn->fn_type->output->is_primitive() ||
      fn->fn_type->output->is_pointer() || fn->fn_type->output->is_enum()) {
    assert(fn->fn_type->output != Void);
    builder.CreateRet(IR_to_LLVM(fn, ret_val, registers));
  } else {
    // TODO return some other value
    builder.CreateRetVoid();
  }
}

void Exit::ReturnVoid::GenerateLLVM(
    Func *fn, const std::vector<llvm::Value *> &registers) {
  builder.CreateRetVoid();
}
} // nmaespace IR

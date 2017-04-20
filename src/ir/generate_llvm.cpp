#include "ir.h"
#include "../type/type.h"
#include "stack.h"

extern llvm::IRBuilder<> builder;
extern llvm::BasicBlock *make_block(const std::string &name,
                                    llvm::Function *fn);
extern llvm::Module *global_module;
extern const char *GetGlobalStringNumbered(size_t index);

namespace cstdlib {
extern llvm::Constant *malloc();

static llvm::Constant *printf() {
  static llvm::Constant *func_ = global_module->getOrInsertFunction(
      "printf", llvm::FunctionType::get(*Int, {*Ptr(Char)}, true));
  return func_;
}

static llvm::Constant *free() {
  static llvm::Constant *func_ = global_module->getOrInsertFunction(
      "free", llvm::FunctionType::get(*Void, {*Ptr(Char)}, false));
  return func_;
}

static llvm::Constant *memcpy() {
  static llvm::Constant *func_ = global_module->getOrInsertFunction(
      "memcpy", llvm::FunctionType::get(
                    *Ptr(Char), {*Ptr(Char), *Ptr(Char), *Uint}, false));
  return func_;
}
} // namespace cstdlib

namespace data {
extern llvm::ConstantInt *const_bool(bool b);
extern llvm::ConstantInt *const_uint(size_t n);
extern llvm::ConstantInt *const_u16(uint16_t n);
extern llvm::ConstantInt *const_u32(uint32_t n);
extern llvm::ConstantInt *const_int(long n);
extern llvm::ConstantInt *const_char(char c);
extern llvm::ConstantFP *const_real(double d);
extern llvm::Value *global_string(const std::string &s);
extern llvm::Constant *null_pointer(Type *t);
} // namespace data

namespace IR {
static llvm::Value *IR_to_LLVM(IR::Func *ir_fn, IR::Value cmd_arg,
                               const std::vector<llvm::Value *> &registers) {
  switch (cmd_arg.flag) {
  case ValType::Val: return cmd_arg.as_val->llvm();
  case ValType::Loc: return cmd_arg.as_loc->llvm(ir_fn, registers);
  case ValType::ExtFn: return global_module->getFunction(cmd_arg.as_ext_fn);
  case ValType::CStr: return data::global_string(std::string(cmd_arg.as_cstr));
  case ValType::Block: return reinterpret_cast<llvm::Value *>(cmd_arg.as_block);
  case ValType::None: UNREACHABLE;
  case ValType::Error: UNREACHABLE;
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
        assert(cmd.args[2].flag == IR::ValType::Val);
        llvm::Value *val = nullptr;
        if (cmd.args[2].as_val->is_uint()) {
          val = data::const_u32((uint32_t)cmd.args[2].as_val->GetUint());
        } else if (cmd.args[2].as_val->is_u32()) {
          val = cmd.args[2].as_val->llvm();
        } else {
          UNREACHABLE;
        }

        registers[cmd.result.reg] =
            builder.CreateGEP(IR_to_LLVM(ir_fn, cmd.args[1], registers),
                              {data::const_u32(0), val});
      } continue;
      case IR::Op::Phi: {
        registers[cmd.result.reg] = builder.CreatePHI(
            *cmd.result.type, (unsigned int)cmd.args.size() >> 1, "phi");
        phis.emplace_back(this, cmd_counter);
      } continue;
      case IR::Op::TC_Ptr: {
        Type *pointee = nullptr;

        if (cmd.args[0].flag == ValType::Val) {
          pointee = cmd.args[0].as_val->GetType();
        } else if (cmd.args[0].flag == ValType::Loc) {
          // TODO WHAT?
          pointee =
              reinterpret_cast<Type *>(registers[cmd.args[0].as_loc->GetReg()]);
        } else {
          UNREACHABLE;
        }
        assert(pointee);

        registers[cmd.result.reg] =
            reinterpret_cast<llvm::Value *>(Ptr(pointee));
      } continue;
      case IR::Op::TC_Arrow: {
        Type *from = nullptr;

        if (cmd.args[0].flag == ValType::Val) {
          from = cmd.args[0].as_val->GetType();
        } else if (cmd.args[0].flag == ValType::Loc) {
          // TODO WHAT?
          from =
              reinterpret_cast<Type *>(registers[cmd.args[0].as_loc->GetReg()]);
        } else {
          UNREACHABLE;
        }
        assert(from);

        Type *to = nullptr;
        if (cmd.args[1].flag == ValType::Val) {
          to = cmd.args[1].as_val->GetType();
        } else if (cmd.args[1].flag == ValType::Loc) {
          // TODO WHAT?
          to =
              reinterpret_cast<Type *>(registers[cmd.args[1].as_loc->GetReg()]);
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

        if (cmd.args[0].flag == ValType::Val) {
          data_type = cmd.args[0].as_val->GetType();
        } else if (cmd.args[1].flag == ValType::Loc) {
          data_type =
              reinterpret_cast<Type *>(registers[cmd.args[0].as_loc->GetReg()]);
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

        if (cmd.args[0].flag == IR::ValType::Val) {
          length = (size_t)cmd.args[0].as_val->GetInt();
        } else if (cmd.args[0].flag == IR::ValType::Val) {
          length = cmd.args[0].as_val->GetUint();
        } else {
          UNREACHABLE;
        }

        Type *data_type = nullptr;
        if (cmd.args[1].flag == ValType::Val) {
          data_type = cmd.args[1].as_val->GetType();
        } else if (cmd.args[1].flag == ValType::Loc) {
          data_type = 
              reinterpret_cast<Type *>(registers[cmd.args[1].as_loc->GetReg()]);

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
        if (cmd.args[0].flag == ValType::Val) {
          t = cmd.args[0].as_val->GetType();
        } else if (cmd.args[0].flag == ValType::Loc) {
          t = reinterpret_cast<Type *>(registers[cmd.args[0].as_loc->GetReg()]);
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

          assert(cmd.args[0].flag == IR::ValType::Val &&
                 cmd.args[0].as_val->is_function());

          std::vector<IR::Value> cmd_args;
          for (size_t i = 1; i < cmd.args.size(); ++i) {
            cmd_args.push_back(cmd.args[i]);
          }
          auto result = cmd.args[0].as_val->GetFunc()->Call(local_stack, cmd_args);
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
        assert(args[i]);
      }

      switch (cmd.op_code) {
      case IR::Op::Not: {
        registers[cmd.result.reg] = builder.CreateNot(args[0]);
      } break;
      case IR::Op::Neg: {
        registers[cmd.result.reg] = (cmd.result.type == Real)
                                        ? builder.CreateFNeg(args[0])
                                        : builder.CreateNeg(args[0]);
      } break;
      case IR::Op::Add: {
        registers[cmd.result.reg] = (cmd.result.type == Real)
                                        ? builder.CreateFAdd(args[0], args[1])
                                        : builder.CreateAdd(args[0], args[1]);
      } break;
      case IR::Op::Sub: {
        registers[cmd.result.reg] = (cmd.result.type == Real)
                                        ? builder.CreateFSub(args[0], args[1])
                                        : builder.CreateSub(args[0], args[1]);
      } break;
      case IR::Op::Mul: {
        registers[cmd.result.reg] = (cmd.result.type == Real)
                                        ? builder.CreateFMul(args[0], args[1])
                                        : builder.CreateMul(args[0], args[1]);
      } break;
      case IR::Op::Div: {
        registers[cmd.result.reg] =
            (cmd.result.type == Int)  ? builder.CreateSDiv(args[0], args[1])
          : (cmd.result.type == Uint) ? builder.CreateUDiv(args[0], args[1])
          : /* else */                  builder.CreateFDiv(args[0], args[1]);
      } break;
      case IR::Op::Mod: {
        registers[cmd.result.reg] =
            (cmd.result.type == Int)  ? builder.CreateSRem(args[0], args[1])
          : (cmd.result.type == Uint) ? builder.CreateURem(args[0], args[1])
          : /* else */                  builder.CreateFRem(args[0], args[1]);
      } break;
      case IR::Op::Or: {
        registers[cmd.result.reg] = builder.CreateOr(args[0], args[1]);
      } break;
      case IR::Op::Xor: {
        registers[cmd.result.reg] = builder.CreateXor(args[0], args[1]);
      } break;
      case IR::Op::LT: {
        auto type = reinterpret_cast<Type*>(args[0]);
        registers[cmd.result.reg] =
            (type == Int)  ? builder.CreateICmpSLT(args[1], args[2])
          : (type == Uint) ? builder.CreateICmpULT(args[1], args[2])
          : /* else */       builder.CreateFCmpOLT(args[1], args[2]);
      } break;
      case IR::Op::LE: {
        auto type = reinterpret_cast<Type*>(args[0]);
        registers[cmd.result.reg] =
            (type == Int)  ? builder.CreateICmpSLE(args[1], args[2])
          : (type == Uint) ? builder.CreateICmpULE(args[1], args[2])
          : /* else */       builder.CreateFCmpOLE(args[1], args[2]);
      } break;
      case IR::Op::EQ: {
        auto type = reinterpret_cast<Type *>(args[0]);
        if (type->is_function()) { NOT_YET; }
        if (type == Real) {
          registers[cmd.result.reg] = builder.CreateFCmpOEQ(args[1], args[2]);
        } else if (type == Type_) {
          registers[cmd.result.reg] =
              data::const_bool(reinterpret_cast<Type *>(args[1]) ==
                               reinterpret_cast<Type *>(args[2]));
        } else {
          registers[cmd.result.reg] = builder.CreateICmpEQ(args[1], args[2]);
        }
      } break;
      case IR::Op::NE: {
        auto type = reinterpret_cast<Type *>(args[0]);
        if (type->is_function()) { NOT_YET; }
        if (type == Real) {
          registers[cmd.result.reg] = builder.CreateFCmpONE(args[1], args[2]);
        } else if (type == Type_) {
          registers[cmd.result.reg] =
              data::const_bool(reinterpret_cast<Type *>(args[1]) !=
                               reinterpret_cast<Type *>(args[2]));
        } else {
          registers[cmd.result.reg] = builder.CreateICmpNE(args[1], args[2]);
        }
      } break;
      case IR::Op::GE: {
        auto type = reinterpret_cast<Type *>(args[0]);
        registers[cmd.result.reg] =
            (type == Int)  ? builder.CreateICmpSGE(args[1], args[2])
          : (type == Uint) ? builder.CreateICmpUGE(args[1], args[2])
          : /* else */       builder.CreateFCmpOGE(args[1], args[2]);
      } break;
      case IR::Op::GT: {
        auto type = reinterpret_cast<Type *>(args[0]);
        registers[cmd.result.reg] =
            (type == Int || type == Char) ? builder.CreateICmpSGT(args[1], args[2])
          : (type == Uint)                ? builder.CreateICmpUGT(args[1], args[2])
          :                                 builder.CreateFCmpOGT(args[1], args[2]);
      } break;
      case IR::Op::Malloc:
        registers[cmd.result.reg] = builder.CreateBitCast(
            builder.CreateCall(cstdlib::malloc(), args[0]), *cmd.result.type);
        break;
      case IR::Op::Free:
        builder.CreateCall(cstdlib::free(),
                           builder.CreateBitCast(args[0], *Ptr(Char)));
        break;
      case IR::Op::ArrayLength:
        registers[cmd.result.reg] = builder.CreateGEP(
            args[0], {data::const_u32(0), data::const_u32(0)});
        break;
      case IR::Op::ArrayData:
        registers[cmd.result.reg] = builder.CreateGEP(
            args[1], {data::const_u32(0), data::const_u32(1)});
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
        } else if (print_type == String) {
          builder.CreateCall(cstdlib::printf(),
                             {data::global_string("%s"), args[1]});
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
          auto val_str = builder.CreateLoad(builder.CreateGEP(
              enum_type->string_data, {data::const_u32(0), args[1]}));
          builder.CreateCall(
              cstdlib::printf(),
              {data::global_string(enum_type->to_string() + ".%s"), val_str});
        } else {
          UNREACHABLE;
        }
      } break;
      case IR::Op::Call: {
        auto fn_obj = args[0];
        args[0] =
            builder.CreateGEP(fn_obj, {data::const_u32(0), data::const_u32(0)});
        auto fn_ptr =
            builder.CreateGEP(fn_obj, {data::const_u32(0), data::const_u32(1)});
        auto ret_type = cmd.result.type;

        if (ret_type == Void) {
          builder.CreateCall(fn_ptr, args);
        } else if (ret_type->is_primitive() || ret_type->is_pointer() ||
                   ret_type->is_enum() || ret_type->is_function()) {
          registers[cmd.result.reg] = builder.CreateCall(fn_ptr, args);
        } else if (ret_type->is_tuple()) {
            NOT_YET;
        } else {
          // TODO move this to the correct location
          auto ret_val = builder.CreateAlloca(*ret_type);
          args.push_back(ret_val);
          registers[cmd.result.reg] = ret_val;
        }

      } break;
      case IR::Op::Load:
        // TODO this may be a hack. If it's a function, don't load it.
        if (cmd.args[0].flag == ValType::Val && cmd.args[0].as_val->is_function()) {
          registers[cmd.result.reg] = args[0];
        } else {
          registers[cmd.result.reg] = builder.CreateLoad(args[0]);
        }
        break;
      case IR::Op::Store: {
        // TODO or do we want to actually do the store (it'll be easily
        // optimized out)
        if (reinterpret_cast<Type *>(args[0]) == Type_) { break; }
        if (reinterpret_cast<Type *>(args[0])->is_function()) {
          if (args[1] == data::null_pointer(Char)) {
            args[1] = data::null_pointer(reinterpret_cast<Type *>(args[0]));
          }

          auto raw_fn_ptr = builder.CreateGEP(
              args[2], {data::const_u32(0), data::const_u32(1)});
          builder.CreateStore(args[1], raw_fn_ptr);
        } else {
          builder.CreateStore(args[1], args[2]);
        }
      } break;
      case IR::Op::Cast: {
        auto from_type = reinterpret_cast<Type *>(args[0]);
        auto to_type   = reinterpret_cast<Type *>(args[1]);

        if (from_type == to_type ||
            (from_type == String && to_type == Ptr(Char)) ||
            (from_type == Ptr(Char) && to_type == String)) {
          registers[cmd.result.reg] = args[2];

        } else if (from_type == Bool) {
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
        } else {
          UNREACHABLE;
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
      case IR::Op::WriteErr: UNREACHABLE;

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

void Exit::Switch::GenerateLLVM(
    Func *fn, const std::vector<llvm::Value *> &registers) {
  auto switch_stmt = builder.CreateSwitch(IR_to_LLVM(fn, cond, registers),
                                          default_block->llvm_block,
                                          (unsigned int)table.size());

  for (auto entry : table) {
    assert(entry.first.flag == ValType::Val &&
           (entry.first.as_val->is_bool() || entry.first.as_val->is_char() ||
            entry.first.as_val->is_int() || entry.first.as_val->is_uint()));

    // NOTE: This assertion guarantees that we can cast from llvm::Constant* to
    // llvm::ConstantInt*.
    switch_stmt->addCase((llvm::ConstantInt *)entry.first.as_val->llvm(),
                         entry.second->llvm_block);
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
      fn->fn_type->output->is_function() || fn->fn_type->output->is_pointer() ||
      fn->fn_type->output->is_enum()) {
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

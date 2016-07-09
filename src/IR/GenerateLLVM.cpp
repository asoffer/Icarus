#include "IR/IR.h"
#include "Type/Type.h"

extern llvm::IRBuilder<> builder;
extern llvm::BasicBlock *make_block(const std::string &name,
                                    llvm::Function *fn);

namespace cstdlib {
extern llvm::Constant *printf();
} // namespace cstdlib

namespace data {
extern llvm::Constant *null_pointer(Type *t);
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
static llvm::Value *IR_to_LLVM(IR::Func *ir_fn, IR::Value cmd_arg,
                               const std::vector<llvm::Value *> &registers) {
  switch (cmd_arg.flag) {
  case ValType::B: return data::const_bool(cmd_arg.as_bool);
  case ValType::C: return data::const_char(cmd_arg.as_char);
  case ValType::I: return data::const_int(cmd_arg.as_int);
  case ValType::R: return data::const_real(cmd_arg.as_real);
  case ValType::U: return data::const_uint(cmd_arg.as_uint);
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
  case ValType::StackAddr: NOT_YET;
  case ValType::FrameAddr:
    assert(ir_fn->frame_map.find(cmd_arg.as_frame_addr) != ir_fn->frame_map.end());
    return ir_fn->frame_map[cmd_arg.as_frame_addr];
  case ValType::HeapAddr: NOT_YET;
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
  std::vector<llvm::BasicBlock *> llvm_blocks(blocks.size(), nullptr);
  llvm_fn->setName(name);

  for (const auto &b : blocks) {
    b->llvm_block = make_block(b->block_name, llvm_fn);
  }

  std::vector<llvm::Value *> registers(num_cmds, nullptr);
  std::vector<std::pair<IR::Block *, size_t>> phis;
  for (const auto &b : blocks) {
    llvm_blocks AT(b->block_num) = b->GenerateLLVM(this, registers, phis);
  }

  for (auto phi_loc_pair : phis) {
    CompletePhiDefinition(this, phi_loc_pair.first, phi_loc_pair.second,
                          registers);
  }



  builder.SetInsertPoint(alloc_block);
  assert(!llvm_blocks.empty());
  builder.CreateBr(llvm_blocks[0]);
}

llvm::BasicBlock *
Block::GenerateLLVM(IR::Func *ir_fn, std::vector<llvm::Value *> &registers,
                    std::vector<std::pair<IR::Block *, size_t>> &phis) {
  assert(llvm_block);
  builder.SetInsertPoint(llvm_block);

  // NOTE this is larger than necessary but definitely big enough.
  // TODO get the size just right when we know it.
  {
    size_t cmd_counter = 0;
    for (const auto &cmd : cmds) {
      switch (cmd.op_code) {
      case IR::Op::Field: {
        assert(cmd.args[2].flag == IR::ValType::U);
        registers[cmd.result.reg] = builder.CreateGEP(
            IR_to_LLVM(ir_fn, cmd.args[1], registers),
            {data::const_uint(0), data::const_uint32(cmd.args[2].as_uint)});
      }
        continue;
      case IR::Op::Phi: {
        registers[cmd.result.reg] = builder.CreatePHI(
            *cmd.result.type, (unsigned int)cmd.args.size() >> 1, "phi");
        phis.emplace_back(this, cmd_counter);
      }
        continue;
      case IR::Op::TC_Ptr: NOT_YET;
      case IR::Op::TC_Arrow: NOT_YET;
      case IR::Op::TC_Arr1: NOT_YET;
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
          const_cast<Cmd &>(cmd).dump(10);
          UNREACHABLE;
        }

        registers[cmd.result.reg] =
            reinterpret_cast<llvm::Value *>(Arr(data_type, length));
      }
        continue;
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
        registers[cmd.result.reg] = builder.CreateSub(args[0], args[1]);
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

      case IR::Op::TEQ: NOT_YET;
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

      case IR::Op::TNE: NOT_YET;
      case IR::Op::FnNE: NOT_YET;

      case IR::Op::BNE:
      case IR::Op::CNE:
      case IR::Op::INE:
      case IR::Op::UNE:
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
        registers[cmd.result.reg] = builder.CreateICmpSGT(args[0], args[1]);
        break;
      case IR::Op::IGT:
        registers[cmd.result.reg] = builder.CreateICmpSGT(args[0], args[1]);
        break;
      case IR::Op::UGT:
        registers[cmd.result.reg] = builder.CreateICmpUGT(args[0], args[1]);
        break;
      case IR::Op::FGT:
        registers[cmd.result.reg] = builder.CreateFCmpOGT(args[0], args[1]);
        break;

      case IR::Op::ArrayLength: NOT_YET;
      case IR::Op::ArrayData: NOT_YET;
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
        } else {
          UNREACHABLE;
        }
      } break;
      case IR::Op::Call: {
        auto fn = args[0];
        args.erase(args.begin());

        auto call_fn_type = cmd.args[0].as_func->fn_type;
        auto ret_type = call_fn_type->output;
        if (ret_type->is_primitive()) {
          if (ret_type == Void) {
            builder.CreateCall(fn, args);
          } else {
            registers[cmd.result.reg] = builder.CreateCall(fn, args);
          }
        } else if (!ret_type->is_primitive()) {
          if (ret_type->is_tuple()) {
            NOT_YET;
          } else {
            auto ret_val = builder.CreateAlloca(*ret_type);
            args.push_back(ret_val);
            registers[cmd.result.reg] = ret_val;
          }
        }

      } break;
      case IR::Op::Load:
        registers[cmd.result.reg] = builder.CreateLoad(args[0]);
        break;
      case IR::Op::Store: builder.CreateStore(args[1], args[2]); break;
      case IR::Op::Cast: NOT_YET;
      case IR::Op::NOp: NOT_YET;
      case IR::Op::PtrIncr:
        registers[cmd.result.reg] = builder.CreateGEP(args[1], args[2]);
        break;
      case IR::Op::Access:
        registers[cmd.result.reg] =
            builder.CreateGEP(args[2], {data::const_uint(0), args[1]});
        break;
      case IR::Op::Field: UNREACHABLE;
      case IR::Op::Phi: UNREACHABLE;
      case IR::Op::TC_Ptr: UNREACHABLE;
      case IR::Op::TC_Arrow: UNREACHABLE;
      case IR::Op::TC_Arr1: UNREACHABLE;
      case IR::Op::TC_Arr2: UNREACHABLE;

      case IR::Op::Bytes: NOT_YET;
      case IR::Op::Alignment: NOT_YET;
      }
      ++cmd_counter;
    }
  }

  switch (exit.flag) {
  case Exit::Strategy::Unset: ir_fn->dump(); UNREACHABLE;
  case Exit::Strategy::Uncond:
    builder.CreateBr(exit.true_block->llvm_block);
    break;
  case Exit::Strategy::Cond:
    builder.CreateCondBr(IR_to_LLVM(ir_fn, exit.val, registers),
                         exit.true_block->llvm_block,
                         exit.false_block->llvm_block);
    break;
  case Exit::Strategy::Return:
    if (ir_fn->fn_type->output->is_primitive()) {
      assert(ir_fn->fn_type->output != Void);
      builder.CreateRet(IR_to_LLVM(ir_fn, exit.val, registers));
    } else {
      // TODO return some other value
      builder.CreateRetVoid();
    }
    break;
  case Exit::Strategy::ReturnVoid: builder.CreateRetVoid(); break;
  }
  return llvm_block;
}

} // nmaespace IR

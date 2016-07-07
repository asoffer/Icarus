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
extern llvm::ConstantInt *const_int(long n);
extern llvm::ConstantInt *const_char(char c);
extern llvm::ConstantFP *const_real(double d);
extern llvm::Value *global_string(const std::string &s);
} // namespace data

namespace IR {
void Func::GenerateLLVM() {
  std::vector<llvm::BasicBlock *> llvm_blocks(blocks.size(), nullptr);
  auto alloc_block = make_block("entry", llvm_fn);
  builder.SetInsertPoint(alloc_block);
  for (auto kv : frame_map) { kv.second->alloc = kv.second->type->allocate(); }

  for (const auto &b : blocks) {
    b->llvm_block = make_block(b->block_name, llvm_fn);
  }


  for (const auto &b : blocks) {
    llvm_blocks AT(b->block_num) = b->GenerateLLVM(this);
  }

  builder.SetInsertPoint(alloc_block);
  assert(!llvm_blocks.empty());
  builder.CreateBr(llvm_blocks[0]);
}

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
  case ValType::Arg: NOT_YET;
  case ValType::StackAddr: NOT_YET;
  case ValType::FrameAddr:
    return ir_fn->frame_map[cmd_arg.as_frame_addr]->alloc;
  case ValType::HeapAddr: NOT_YET;
  }
}

llvm::BasicBlock *Block::GenerateLLVM(IR::Func *ir_fn) {
  assert(llvm_block);
  builder.SetInsertPoint(llvm_block);

  // NOTE this is larger than necessary but definitely big enough.
  // TODO get the size just right when we know it.
  std::vector<llvm::Value *> registers(cmds.size(), nullptr);

  for (const auto &cmd : cmds) {
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
      registers[cmd.result.reg] = builder.CreateMul(args[0], args[1]);
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

    case IR::Op::PtrEQ: NOT_YET;
    case IR::Op::TEQ: NOT_YET;
    case IR::Op::FnEQ: NOT_YET;

    case IR::Op::BEQ:
      registers[cmd.result.reg] = builder.CreateICmpEQ(args[0], args[1]);
      break;
    case IR::Op::CEQ:
      registers[cmd.result.reg] = builder.CreateICmpEQ(args[0], args[1]);
      break;
    case IR::Op::IEQ:
      registers[cmd.result.reg] = builder.CreateICmpEQ(args[0], args[1]);
      break;
    case IR::Op::UEQ:
      registers[cmd.result.reg] = builder.CreateICmpEQ(args[0], args[1]);
      break;
    case IR::Op::FEQ:
      registers[cmd.result.reg] = builder.CreateFCmpOEQ(args[0], args[1]);
      break;

    case IR::Op::TNE: NOT_YET;
    case IR::Op::FnNE: NOT_YET;

    case IR::Op::BNE:
      registers[cmd.result.reg] = builder.CreateICmpNE(args[0], args[1]);
      break;
    case IR::Op::CNE:
      registers[cmd.result.reg] = builder.CreateICmpNE(args[0], args[1]);
      break;
    case IR::Op::INE:
      registers[cmd.result.reg] = builder.CreateICmpNE(args[0], args[1]);
      break;
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
        NOT_YET;
      } else if (print_type == Bool) {
        auto to_show = builder.CreateSelect(
            args[1], data::global_string("true"), data::global_string("false"));
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
      }
    } break;
    case IR::Op::Call: {
      auto fn = args[0];
      args.erase(args.begin());
      builder.CreateCall(fn, args);
    } break;
    case IR::Op::Load:
      registers[cmd.result.reg] = builder.CreateLoad(args[0]);
      break;
    case IR::Op::Store: builder.CreateStore(args[1], args[2]); break;
    case IR::Op::Cast: NOT_YET;
    case IR::Op::NOp: NOT_YET;

    case IR::Op::Phi: {
      llvm::PHINode *phi = builder.CreatePHI(
          *cmd.result.type, (unsigned int)args.size() >> 1, "phi");

      for (size_t i = 0; i < args.size(); i += 2) {
        phi->addIncoming(args[i + 1], ((Block *)args[i])->llvm_block);
      }
      registers[cmd.result.reg] = phi;
    } break;
    case IR::Op::Field: NOT_YET;
    case IR::Op::Access: NOT_YET;
    case IR::Op::TC_Ptr: NOT_YET;
    case IR::Op::TC_Arrow: NOT_YET;
    case IR::Op::TC_Arr1: NOT_YET;
    case IR::Op::TC_Arr2: NOT_YET;
    case IR::Op::Bytes: NOT_YET;
    case IR::Op::Alignment: NOT_YET;
    }
  }

  switch (exit.flag) {
  case Exit::Strategy::Unset: UNREACHABLE;
  case Exit::Strategy::Uncond:
    builder.CreateBr(exit.true_block->llvm_block);
    break;
  case Exit::Strategy::Cond:
    builder.CreateCondBr(IR_to_LLVM(ir_fn, exit.val, registers),
                         exit.true_block->llvm_block,
                         exit.false_block->llvm_block);
    break;
  case Exit::Strategy::Return: NOT_YET;
  case Exit::Strategy::ReturnVoid: builder.CreateRetVoid(); break;
  }
  return llvm_block;
}

} // nmaespace IR

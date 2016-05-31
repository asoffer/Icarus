#include "IR.h"
#include <cmath>

// TODO what Value number to return????
namespace IR {
Func *Func::Current;
Block *Block::Current;

Value::Value() : flag(ValType::Ref) {
  val.as_arg = Func::Current->num_cmds;
  Func::Current->num_cmds++;
}

static std::string OpCodeString(Op op_code) {
  switch (op_code) {
  case Op::BNot:  return "not  ";
  case Op::INeg:  return "ineg ";
  case Op::FNeg:  return "fneg ";
  case Op::Load:  return "load ";
  case Op::Store: return "store";
  case Op::Phi:   return "phi  ";
  case Op::IAdd:  return "iadd ";
  case Op::UAdd:  return "uadd ";
  case Op::FAdd:  return "fadd ";
  case Op::ISub:  return "isub ";
  case Op::USub:  return "usub ";
  case Op::FSub:  return "fsub ";
  case Op::IMul:  return "imul ";
  case Op::UMul:  return "umul ";
  case Op::FMul:  return "fmul ";
  case Op::IDiv:  return "idiv ";
  case Op::UDiv:  return "udiv ";
  case Op::FDiv:  return "fdiv ";
  case Op::IMod:  return "imod ";
  case Op::UMod:  return "umod ";
  case Op::FMod:  return "fmod ";
  case Op::BXor:  return "bxor ";
  case Op::ILT:   return "ilt  ";
  case Op::ULT:   return "ult  ";
  case Op::FLT:   return "flt  ";
  case Op::ILE:   return "ile  ";
  case Op::ULE:   return "ule  ";
  case Op::FLE:   return "fle  ";
  case Op::IEQ:   return "ieq  ";
  case Op::UEQ:   return "ueq  ";
  case Op::FEQ:   return "feq  ";
  case Op::BEQ:   return "beq  ";
  case Op::CEQ:   return "ceq  ";
  case Op::FnEQ:  return "fneq ";
  case Op::TEQ:   return "teq  ";
  case Op::INE:   return "ine  ";
  case Op::UNE:   return "une  ";
  case Op::FNE:   return "fne  ";
  case Op::BNE:   return "bne  ";
  case Op::CNE:   return "cne  ";
  case Op::FnNE:  return "fnne ";
  case Op::TNE:   return "tne  ";
  case Op::IGT:   return "igt  ";
  case Op::UGT:   return "ugt  ";
  case Op::FGT:   return "fgt  ";
  case Op::IGE:   return "ige  ";
  case Op::UGE:   return "uge  ";
  case Op::FGE:   return "fge  ";
  }
}

std::ostream &operator<<(std::ostream& os, const Value& value) {
  switch(value.flag) {
  case ValType::B: return os << (value.val.as_bool ? "true" : "false");
  case ValType::C: return os << value.val.as_char;
  case ValType::I: return os << value.val.as_int;
  case ValType::R: return os << value.val.as_real;
  case ValType::U: return os << value.val.as_uint;
  case ValType::T: return os << value.val.as_type;
  case ValType::F: return os << "f_" << value.val.as_func->name;
  case ValType::Ref: return os << "%" << value.val.as_ref;
  case ValType::Arg: return os << "$" << value.val.as_arg;
  }
}

void Cmd::dump(size_t indent) {
  std::cout << std::string(indent, ' ') << result << "\t= ";

  if (op_code == Op::Phi) {
    std::cout << "phi (" << incoming_blocks.size() << ")";
    for (size_t i = 0; i < incoming_blocks.size(); ++i) {
      std::cout << "\n" << std::string(indent + 2, ' ') << "block-"
                << incoming_blocks[i]->block_num << " => " << args[i];
    }
    std::cout << std::endl;
  } else {
    std::cout << OpCodeString(op_code);
    auto iter = args.begin();

    std::cout << " " << *iter;

    ++iter;
    for (; iter != args.end(); ++iter) { std::cout << ", " << *iter; }
    std::cout << std::endl;
  }
}

void Block::dump() {
  if (block_num == 0) {
    std::cout << "  entry:\n";
  } else {
    std::cout << "  block-" << block_num << ":\n";
  }
  for (auto c : cmds) { c.dump(4); }

  exit.dump(4);
}

void Exit::dump(size_t indent) {
  std::cout << std::string(indent, ' ');

  switch (flag) {
  case Strategy::Uncond:
    std::cout << "jmp block-" << true_block->block_num << "\n\n";
    return;

  case Strategy::Cond:
    std::cout << "cond br " << val << " [T: block-" << true_block->block_num
              << "] [F: block-" << false_block->block_num << "]\n\n";
    return;

  case Strategy::Return:
    std::cout << "ret " << val << "\n\n";
    return;
  }
}

void Func::dump() {
  std::cout << "func " << name;
  if (args.empty()) {
    std::cout << "():" << std::endl;
  } else {
    std::cout << "(#$" << args.size() << "):" << std::endl;
  }

  for (auto b : blocks) { b->dump(); }
}

} // namespace IR

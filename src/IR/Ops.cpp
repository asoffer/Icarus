#include "IR.h"
#include <cmath>

#define ONE_ARG_CMD(name)                                                      \
  Cmd name(Value v) {                                                          \
    Cmd cmd(Op::name, v);                                                      \
    Block::Current->cmds.push_back(cmd);                                       \
    return cmd;                                                                \
  }

#define TWO_ARG_CMD(name)                                                      \
  Cmd name(Value arg1, Value arg2) {                                           \
    Cmd cmd(Op::name, arg1);                                                   \
    cmd.args.push_back(arg2); /* Put this in the constructor */                \
    Block::Current->cmds.push_back(cmd);                                       \
    return cmd;                                                                \
  }

// TODO what Value number to return????
namespace IR {
Func *Func::Current;
Block *Block::Current;

Value::Value() : flag(ValType::Ref) {
  val.as_arg = Func::Current->num_cmds;
  Func::Current->num_cmds++;
}

ONE_ARG_CMD(BNot)

ONE_ARG_CMD(INeg)
ONE_ARG_CMD(FNeg)

ONE_ARG_CMD(Load)
TWO_ARG_CMD(Store)

Cmd Phi() {
  Cmd phi;
  phi.op_code = Op::Phi;
  // Block::Current->cmds.push_back(phi);
  return phi;
}

TWO_ARG_CMD(IAdd)
TWO_ARG_CMD(UAdd)
TWO_ARG_CMD(FAdd)

TWO_ARG_CMD(ISub)
TWO_ARG_CMD(USub)
TWO_ARG_CMD(FSub)

TWO_ARG_CMD(IMul)
TWO_ARG_CMD(UMul)
TWO_ARG_CMD(FMul)

TWO_ARG_CMD(IDiv)
TWO_ARG_CMD(UDiv)
TWO_ARG_CMD(FDiv)

TWO_ARG_CMD(IMod)
TWO_ARG_CMD(UMod)
TWO_ARG_CMD(FMod)

TWO_ARG_CMD(BXor)

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

#undef TWO_ARG_CMD
#undef ONE_ARG_CMD

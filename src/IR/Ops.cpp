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
ONE_ARG_CMD(Ret)

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

static std::string OpCodeString(Op op_code) {
  switch (op_code) {
  case Op::BNot:  return "not  ";
  case Op::INeg:  return "ineg ";
  case Op::FNeg:  return "fneg ";
  case Op::Load:  return "load ";
  case Op::Store: return "store";
  case Op::Ret:   return "ret  ";
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
  std::cout << std::string(indent, ' ') << result << "\t= "
            << OpCodeString(op_code);

  auto iter = args.begin();

  std::cout << " " << *iter;

  ++iter;
  for (; iter != args.end(); ++iter) {
    std::cout << ", " << *iter;
  }
  std::cout << std::endl;
}

void Block::dump() {
  if (block_num == 0) {
    std::cout << "  entry:\n";
  } else {
    std::cout << "  block-" << block_num << ":\n";
  }
  for (auto c : cmds) { c.dump(4); }
}

void Func::dump() {
  std::cout << "func " << name;
  if (args.empty()) {
    std::cout << "():" << std::endl;
  } else {
    std::cout << "(#$" << args.size() << "):" << std::endl;
  }

  entry->dump();
  for (auto b : blocks) { b->dump(); }
}

} // namespace IR

#undef TWO_ARG_CMD
#undef ONE_ARG_CMD

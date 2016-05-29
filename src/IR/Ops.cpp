#include "IR.h"
#define BLOCK Block::Current()

#define ONE_ARG_CMD(name)                                                      \
  Value name(Value v) {                                                        \
    Cmd cmd(Op::name, v);                                                      \
    cmd.dump();                                                                \
    BLOCK->cmds.push_back(cmd);                                                \
    return cmd.result;                                                         \
  }

#define TWO_ARG_CMD(name)                                                      \
  Value name(Value arg1, Value arg2) {                                         \
    Cmd cmd(Op::name, arg1);                                                   \
    cmd.dump();                                                                \
    cmd.args.push_back(arg2); /* Put this in the constructor */                \
    BLOCK->cmds.push_back(cmd);                                                \
    return cmd.result;                                                         \
  }

// TODO what Value number to return????
namespace IR {
std::stack<Block *> Block::Stack;

ONE_ARG_CMD(BNot)

ONE_ARG_CMD(INeg)
ONE_ARG_CMD(FNeg)

ONE_ARG_CMD(Load)
TWO_ARG_CMD(Store)

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

#define STREAMIFY(stream, value)                                               \
  if ((value).flag == B) {                                                     \
    stream << ((value).val.as_bool ? "true" : "false");                        \
  } else if ((value).flag == C) {                                              \
    stream << (value).val.as_char;                                             \
  } else if ((value).flag == I) {                                              \
    stream << (value).val.as_int;                                              \
  } else if ((value).flag == R) {                                              \
    stream << (value).val.as_real;                                             \
  } else if ((value).flag == U) {                                              \
    stream << (value).val.as_uint;                                             \
  } else if ((value).flag == T) {                                              \
    stream << (value).val.as_type;                                             \
  } else if ((value).flag == Ref) {                                            \
    stream << "%" << (value).val.as_ref;                                       \
  } else {                                                                     \
    assert(false);                                                             \
  }

void Cmd::dump(size_t indent) {
  std::cout << std::string(indent, ' ') << "%";
  STREAMIFY(std::cout, result);
  std::cout << " = " << OpCodeString(op_code);

  auto iter = args.begin();

  std::cout << " ";
  STREAMIFY(std::cout, *iter);

  ++iter;
  for (; iter != args.end(); ++iter) {
    std::cout << ", ";
    STREAMIFY(std::cout, *iter);
  }
  std::cout << std::endl;
}

void Block::dump() {
  std::cout << "block-" << block_num << ":\n";
  for (auto c : cmds) { c.dump(4); }
}

} // namespace IR
#undef STREAMIFY

#undef TWO_ARG_CMD
#undef ONE_ARG_CMD
#undef BLOCK

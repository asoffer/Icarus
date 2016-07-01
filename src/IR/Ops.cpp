#include "IR.h"
#include <cmath>
#include "Type/Type.h"

// TODO what Value number to return????
namespace IR {
#define CMD_WITH_1_ARGS(name, out_type)                                        \
  Value name(Value v) {                                                        \
    Cmd cmd(Op::name, out_type != Void);                                       \
    cmd.args.push_back(v);                                                     \
    cmd.result.type = out_type;                                                \
    Block::Current->push(cmd);                                                 \
    return Value::Reg(cmd.result.reg);                                         \
  }

#define CMD_WITH_2_ARGS(name, out_type)                                        \
  Value name(Value arg1, Value arg2) {                                         \
    Cmd cmd(Op::name, out_type != Void);                                       \
    cmd.args.push_back(arg1);                                                  \
    cmd.args.push_back(arg2);                                                  \
    cmd.result.type = out_type;                                                \
    Block::Current->push(cmd);                                                 \
    return Value::Reg(cmd.result.reg);                                         \
  }

// Intentionally empty. Must be hand implemented
#define CMD_WITH_NA_ARGS(name, out_type)

#define IR_MACRO(OpCode, op_code_str, num_args, out_type)                      \
  CMD_WITH_##num_args##_ARGS(OpCode, out_type)
#include "../config/IR.conf"
#undef IR_MACRO

#undef CMD_WITH_V_ARGS
#undef CMD_WITH_1_ARGS
#undef CMD_WITH_2_ARGS

Value Access(Type *type, Value index, Value ptr) {
  Cmd cmd(Op::Access, true);
  cmd.args        = {Value(type), index, ptr};
  cmd.result.type = Ptr(type);
  Block::Current->push(cmd);
  return Value::Reg(cmd.result.reg);
}

Value Field(Structure *struct_type, Value ptr, size_t field_num) {
  Cmd cmd(Op::Field, true);
  cmd.args        = {Value(struct_type), ptr, Value(field_num)};
  cmd.result.type = Ptr(struct_type->field_type AT(field_num));
  Block::Current->push(cmd);
  return Value::Reg(cmd.result.reg);
}

Value Cast(Type *in, Type *out, Value arg) {
  Cmd cmd(Op::Cast, true);
  cmd.args        = {Value(in), Value(out), arg};
  cmd.result.type = out;
  Block::Current->push(cmd);
  return Value::Reg(cmd.result.reg);
}

Value Call(Type *out, Value fn, const std::vector<Value> &args) {
  Cmd cmd(Op::Call, out != Void);
  cmd.args.push_back(fn);
  for (const auto elem : args) { cmd.args.push_back(elem); }
  cmd.result.type = out;
  Block::Current->push(cmd);
  return Value::Reg(cmd.result.reg);
}

Cmd Phi(Type *ret_type) {
  Cmd cmd(Op::Phi, true);
  cmd.result.type = ret_type;
  return cmd;
}

Value Store(Type *rhs_type, Value lhs, Value rhs) {
  Cmd cmd(Op::Store, false);
  cmd.args        = {Value(rhs_type), lhs, rhs};
  cmd.result.type = Void;
  Block::Current->push(cmd);
  return Value();
}

Value Load(Type *load_type, Value v) {
  Cmd cmd(Op::Load, true);
  cmd.args        = {v};
  cmd.result.type = load_type;
  Block::Current->push(cmd);
  return Value::Reg(cmd.result.reg);
}

Func *Func::Current;
Block *Block::Current;

Cmd::Cmd(Op o, bool has_ret) : op_code(o) {
  result.reg = has_ret ? Func::Current->num_cmds : ~0u;
  if (has_ret) { Func::Current->num_cmds++; }
}

std::string OpCodeString(Op op_code) {
  switch (op_code) {
#define IR_MACRO(OpCode, op_code_str, num_args, out_type)                      \
  case Op::OpCode:                                                             \
    return op_code_str;
#include "config/IR.conf"
#undef IR_MACRO
  }
}

static inline std::string Escape(char c) {
  if (c == '\n') { return "\\n"; }
  if (c == '\r') { return "\\r"; }
  if (c == '\t') { return "\\t"; }
  return std::string(1, c);
}

std::ostream &operator<<(std::ostream &os, const Value &value) {
  switch (value.flag) {
  case ValType::B: return os << (value.as_bool ? "true" : "false");
  case ValType::C: return os << "'" << Escape(value.as_char) << "'";
  case ValType::I: return os << value.as_int;
  case ValType::R: return os << value.as_real;
  case ValType::U: return os << value.as_uint << 'u';
  case ValType::T: return os << *value.as_type;
  case ValType::F: {
    os << "fn.";
    if (value.as_func->name != "") {
      os << value.as_func->name;
    } else {
      os << value.as_func;
    }
    return os;
  }
  case ValType::Ptr: return os << "ptr " << value.as_ptr;
  case ValType::Reg: return os << "%" << value.as_reg;
  case ValType::Arg: return os << "#" << value.as_arg;
  case ValType::Alloc: return os << "$" << value.as_alloc;
  case ValType::RelAlloc: return os << "~$" << value.as_rel_alloc;
  case ValType::Block: return os << "block-" << value.as_block->block_num;
  }
}

void Cmd::dump(size_t indent) {
  std::cerr << std::string(indent, ' ') << result.type->to_string();
  if (result.type != Void) {
    std::cerr << " %" << result.reg << "\t= ";
  } else {
    std::cerr << "  \t  ";
  }
  assert(!args.empty());
  std::cerr << OpCodeString(op_code) << ' ' << args[0];
  for (size_t i = 1; i < args.size(); ++i) { std::cerr << ", " << args[i]; }
  std::cerr << '\n';
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
  std::cerr << std::string(indent, ' ');
  switch (flag) {
  case Strategy::Uncond:
    std::cerr << "jmp block-" << true_block->block_num << "\n\n";
    break;
  case Strategy::Cond:
    std::cerr << "cond br " << val << " [T: block-" << true_block->block_num
              << "] [F: block-" << false_block->block_num << "]\n\n";
    break;
  case Strategy::Return: std::cerr << "ret " << val << "\n\n"; break;
  case Strategy::ReturnVoid: std::cerr << "ret\n\n"; break;
  case Strategy::Unset: UNREACHABLE;
  }
}

void Func::dump() {
  std::cout << "func ";
  if (name != "") {
    std::cout << name;
  } else {
    std::cout << this;
  }

  if (args.empty()) {
    std::cout << "():" << std::endl;
  } else {
    std::cout << "(#$" << args.size() << "):" << std::endl;
  }

  for (auto b : blocks) { b->dump(); }
}

Block *Func::AddBlock() {
  auto result = new IR::Block(blocks.size());
  blocks.push_back(result);
  return result;
}

} // namespace IR

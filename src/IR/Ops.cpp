#include "IR.h"
#include <cmath>
#include "Type/Type.h"

// TODO what Value number to return????
namespace IR {
Func *Func::Current;
Block *Block::Current;

Cmd::Cmd(Op o, Value v) : op_code(o), args(1, v) {
  result.flag       = ValType::Ref;
  result.val.as_ref = Func::Current->num_cmds;
  Func::Current->num_cmds++;
}

Cmd::Cmd() {
  result.flag       = ValType::Ref;
  result.val.as_ref = Func::Current->num_cmds;
  Func::Current->num_cmds++;
}

static std::string OpCodeString(Op op_code) {
  switch (op_code) {
#define IR_MACRO(OpCode, op_code_str, num_args)                                \
  case Op::OpCode:                                                             \
    return op_code_str;
#include "config/IR.conf"
#undef IR_MACRO
  }
}
static std::string Escape(char c) {
  if (c == '\n') { return "\\n"; }
  if (c == '\r') { return "\\r"; }
  if (c == '\t') { return "\\t"; }
  return std::string(1, c);
}

std::ostream &operator<<(std::ostream& os, const Value& value) {
  switch(value.flag) {
  case ValType::B: return os << (value.val.as_bool ? "true" : "false");
  case ValType::C: return os << "'" << Escape(value.val.as_char) << "'";
  case ValType::I: return os << value.val.as_int;
  case ValType::R: return os << value.val.as_real;
  case ValType::U: return os << value.val.as_uint;
  case ValType::T: return os << *value.val.as_type;
  case ValType::F: {
    os << "fn.";
    if (value.val.as_func->name != "") {
      os << value.val.as_func->name;
    } else {
      os << value.val.as_func;
    }
    return os;
  }

  case ValType::Ptr: return os << value.val.as_ptr;
  case ValType::Ref: return os << "%" << value.val.as_ref;
  case ValType::Arg: return os << "#" << value.val.as_arg;
  case ValType::Alloc: return os << "$" << value.val.as_alloc;
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

  case Strategy::ReturnVoid:
    std::cout << "ret\n\n";
    return;

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

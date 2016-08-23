#include "IR.h"
#include <cmath>
#include "Type/Type.h"

// TODO rename "implicit_functions"
extern FileType file_type;
extern llvm::Module *global_module;
extern std::vector<IR::Func *> implicit_functions;

llvm::BasicBlock* make_block(const std::string& name, llvm::Function* fn) {
  return llvm::BasicBlock::Create(llvm::getGlobalContext(), name, fn);
}

static std::vector<IR::Value> initial_globals = {};

// NOTE: This looks like an unnecessary function, but it serves a purpose.
// Simply assigning to the vector can be dangerous, because sometimes we want to
// assign the result of Evaluate() to it. The problem is Evaluate might make a
// call to a generic that then increases the size of the global vector. Because
// C++ doesn't define an order in which the the LHS and RHS are computed, if the
// LHS is computed first, that address can become invalid if the vector needs to
// grow. This function guarantees the call order in a way that is easy to use at
// the call site.
void AddInitialGlobal(size_t global_addr, IR::Value initial_val) {
  assert(global_addr < initial_globals.size());
  // NOTE: Either the value was null, or we are resetting it with the same
  // value. This can happen because we may be defining a recursive function.
  assert(initial_globals[global_addr] == IR::Value::None() ||
         initial_globals[global_addr] == initial_val);
  initial_globals[global_addr] = initial_val;
}

IR::Value GetInitialGlobal(size_t global_addr) {
  assert(global_addr < initial_globals.size());
  return initial_globals[global_addr];
}


namespace IR {
#define STATIC_VALUE(name, flag_name, as_name, param_type)                     \
  Value Value::name(param_type x) {                                            \
    Value v;                                                                   \
    v.flag         = ValType::flag_name;                                       \
    v.as_##as_name = x;                                                        \
    return v;                                                                  \
  }

STATIC_VALUE(GlobalCStr, GlobalCStr, global_cstr, size_t)
STATIC_VALUE(Arg, Arg, arg, size_t)
STATIC_VALUE(Null, Null, null, Type *)
STATIC_VALUE(Reg, Reg, reg, size_t)
STATIC_VALUE(StackAddr, StackAddr, stack_addr, size_t)
STATIC_VALUE(HeapAddr, HeapAddr, heap_addr, void *)
STATIC_VALUE(FrameAddr, FrameAddr, frame_addr, size_t)
STATIC_VALUE(ExtFn, ExtFn, ext_fn, const char *)
#undef STATIC_VALUE

Value Value::None() {
  Value v;
  v.flag    = ValType::T;
  v.as_type = nullptr;
  return v;
}

Value Value::Error() {
  Value v;
  v.flag    = ValType::Error;
  v.as_type = nullptr;
  return v;
}

std::vector<llvm::Value *> LLVMGlobals;

Value Value::CreateGlobal() {
  static size_t global_num_ = 0;
  Value v;
  v.flag          = ValType::GlobalAddr;
  v.as_stack_addr = global_num_++;
  initial_globals.emplace_back(IR::Value::None());
  LLVMGlobals.emplace_back(nullptr);
  return v;
}

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

Func::Func(Function *fn_type, bool should_gen)
    : fn_type(fn_type), llvm_fn(nullptr), alloc_block(nullptr), num_cmds(0),
      frame_size(0), generated(Gen::NotYet) {

  if (!fn_type->has_vars()) {
    should_gen &=
        (fn_type->time() == Time::run || fn_type->time() == Time::either);

    if (should_gen && file_type != FileType::None) {
      llvm::FunctionType *llvm_fn_type = *fn_type;
      llvm_fn = (llvm::Function *)global_module->getOrInsertFunction(
          name, llvm_fn_type);
      implicit_functions.push_back(this);
    }

    alloc_block = make_block("entry", llvm_fn);

    blocks.push_back(new Block());
    blocks.back()->block_name = "fn-entry";
    blocks.push_back(new Block());
    blocks.back()->block_name = "fn-exit";
  }

}

Value Access(Type *type, Value index, Value ptr) {
  Cmd cmd(Op::Access, true);
  cmd.args        = {Value(type), index, ptr};
  cmd.result.type = Ptr(type);
  Block::Current->push(cmd);
  return Value::Reg(cmd.result.reg);
}

Value Field(Struct *struct_type, Value ptr, size_t field_num) {
  Cmd cmd(Op::Field, true);
  cmd.args        = {Value(struct_type), ptr, Value(field_num)};
  cmd.result.type = Ptr(struct_type->field_type AT(field_num));
  Block::Current->push(cmd);
  return Value::Reg(cmd.result.reg);
}

Value PushField(Value fields, const char *name, Value ty, Value init) {
  Cmd cmd(Op::PushField, false);
  cmd.args        = {fields, Value(const_cast<char *>(name)), ty, init};
  cmd.result.type = Void;
  Block::Current->push(cmd);
  return Value();
}

Value InitFieldVec(size_t num_decls) {
  Cmd cmd(Op::InitFieldVec, true);
  cmd.args        = {Value(num_decls)};
  cmd.result.type = Ptr(Char);
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

Value TC_Tup(const std::vector<IR::Value>& vals) {
  Cmd cmd(Op::TC_Tup, true);
  cmd.args = vals;
  cmd.result.type = Type_;
  Block::Current->push(cmd);
  return Value::Reg(cmd.result.reg);
}

Value Memcpy(Value dest, Value source, Value num_bytes) {
  Cmd cmd(Op::Memcpy, false);
  cmd.args = {dest, source, num_bytes};
  cmd.result.type = Void;
  Block::Current->push(cmd);
  return Value();
}

Value ArrayData(Array *type, Value array_ptr) {
  Cmd cmd(Op::ArrayData, true);
  cmd.args        = {IR::Value(type), array_ptr};
  cmd.result.type = Ptr(Ptr(type->data_type));
  Block::Current->push(cmd);
  return Value::Reg(cmd.result.reg);
}

Value Malloc(Type *type, Value num) {
  Cmd cmd(Op::Malloc, true);
  cmd.args        = {num};
  cmd.result.type = Ptr(type);
  Block::Current->push(cmd);
  return Value::Reg(cmd.result.reg);
}

Value Trunc(Value val) {
  Cmd cmd(Op::Trunc, true);
  cmd.args        = {val};
  cmd.result.type = Char;
  Block::Current->push(cmd);
  return Value::Reg(cmd.result.reg);
}

Value ZExt(Value val) {
  Cmd cmd(Op::ZExt, true);
  cmd.args        = {val};
  cmd.result.type = Uint;
  Block::Current->push(cmd);
  return Value::Reg(cmd.result.reg);
}

Value PtrIncr(Pointer *ptr_type, Value ptr, Value incr) {
  Cmd cmd(Op::PtrIncr, true);
  cmd.args = {IR::Value(ptr_type), ptr, incr};
  cmd.result.type = ptr_type;
  Block::Current->push(cmd);
  return Value::Reg(cmd.result.reg);
}

Cmd NOp() {
  Cmd cmd(Op::NOp, false);
  return cmd;
}

Cmd Phi(Type *ret_type) {
  Cmd cmd(Op::Phi, true);
  cmd.result.type = ret_type;
  return cmd;
}

// NOTE: Void rhs-type means stack address
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

Func *Func::Current   = nullptr;
Block *Block::Current = nullptr;

Cmd::Cmd(Op o, bool has_ret) : op_code(o) {
  assert(Func::Current);
  result.reg = has_ret ? Func::Current->num_cmds : FAIL;
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

std::string Escape(char c) {
  if (c == '\n') { return "\\n"; }
  if (c == '\r') { return "\\r"; }
  if (c == '\t') { return "\\t"; }
  if (c < 32) { return "\\" + std::to_string(c); }
  return std::string(1, c);
}

std::ostream &operator<<(std::ostream &os, const Value &value) {
  switch (value.flag) {
  case ValType::B: return os << (value.as_bool ? "true" : "false");
  case ValType::C: return os << "'" << Escape(value.as_char) << "'";
  case ValType::I: return os << value.as_int;
  case ValType::R: return os << value.as_real;
  case ValType::U: return os << value.as_uint << 'u';
  case ValType::U16: return os << value.as_uint16 << "u16";
  case ValType::U32: return os << value.as_uint32 << "u32";
  case ValType::T:
    if (value.as_type) {
      return os << *value.as_type;
    } else {
      return os << "--";
    }
  case ValType::F: {
    os << "fn.";
    if (!value.as_func) {
      os << "null";
    } else if (value.as_func->GetName() != "") {
      os << value.as_func->GetName();
    } else {
      os << value.as_func;
    }
    return os;
  }
  case ValType::ExtFn: return os << "fn:" << value.as_ext_fn;
  case ValType::Null: return os << "null";
  case ValType::CStr: return os << "\"" << (void *)value.as_cstr;
  case ValType::Reg: return os << "%" << value.as_reg;
  case ValType::Arg: return os << "#" << value.as_arg;
  case ValType::StackAddr: return os << "$" << value.as_stack_addr;
  case ValType::FrameAddr: return os << "`$`" << value.as_frame_addr;
  case ValType::HeapAddr: return os << "&" << value.as_heap_addr;
  case ValType::GlobalAddr: return os << "g." << value.as_global_addr;
  case ValType::GlobalCStr: return os << "c\"#" << value.as_global_cstr;
  case ValType::Error: return os << "[[Error]]";
  case ValType::Block:
    if (value.as_block) {
      return os << value.as_block->block_name;
    } else {
      return os << "0x0";
    }
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
  std::cerr << "  " << block_name << ":\n";
  for (auto c : cmds) { c.dump(4); }

  exit->dump(4);
}

namespace Exit {
void Unconditional::dump(size_t indent) {
  std::cerr << std::string(indent, ' ') << "jmp " << block->block_name
            << "\n\n";
}

void Conditional::dump(size_t indent) {
  std::cerr << std::string(indent, ' ');
  std::cerr << "cond br " << cond;
  if (true_block) {
    std::cerr << " [T: " << true_block->block_name << "]";
  } else {
    std::cerr << " [T: 0x0]";
  }

  if (false_block) {
    std::cerr << " [F: " << false_block->block_name << "]\n\n";
  } else {
    std::cerr << " [F: 0x0]\n\n";
  }
}

void Switch::dump(size_t indent) {
  std::cerr << std::string(indent, ' ') << "switch " << cond << " ("
            << table.size() << ") " << cond << "\n";
  for (auto row : table) {
    std::cerr << std::string(indent + 2, ' ') << "[" << row.first << " => "
              << row.second->block_name << "]\n";
  }
  std::cerr << std::string(indent + 2, ' ') << "=> "
            << default_block->block_name << '\n';
}


void ReturnVoid::dump(size_t indent) {
  std::cerr << std::string(indent, ' ') << "ret\n\n";
}

void Return::dump(size_t indent) {
  std::cerr << std::string(indent, ' ') << "ret " << ret_val << "\n\n";
}
} // namespace Exit

void Func::dump() {
  std::cout << "func ";
  if (name != "") {
    std::cout << name;
  } else {
    std::cout << this;
  }

  if (args.empty()) {
    std::cout << ":" << std::endl;
  } else {
    std::cout << "(#$" << args.size() << "):" << std::endl;
  }

  for (auto b : blocks) {
    if (!b) { continue; }
    b->dump();
  }
}

Block *Func::AddBlock(const char *block_name) {
  auto result        = new IR::Block();
  result->block_name = block_name;
  blocks.push_back(result);
  return result;
}

} // namespace IR

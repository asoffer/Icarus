#ifndef ICARUS_IR_H
#define ICARUS_IR_H

// Not to be confused with LLVM's IR!

namespace IR {
enum class ValType : char {
  B, C, I, R, U, T, F, CStr, Block, Reg, Arg, StackAddr, FrameAddr, HeapAddr, GlobalAddr,
  GlobalCStr, Null
};

struct Func;
struct Block;
struct LocalStack;
struct StackFrame;
struct Value;

struct Value {
  union {
    bool as_bool;
    char as_char;
    long as_int;
    double as_real;
    size_t as_uint;
    Type *as_type;
    Func *as_func;
    char *as_cstr;
    size_t as_reg;
    size_t as_arg;
    Block *as_block;
    size_t as_stack_addr, as_frame_addr, as_global_addr;
    void *as_heap_addr;
    Type *as_null;
    size_t as_global_cstr;
  };

  ValType flag;

  explicit Value(bool b) : as_bool(b), flag(ValType::B) {}
  explicit Value(char c) : as_char(c), flag(ValType::C) {}
  explicit Value(long n) : as_int(n), flag(ValType::I) {}
  explicit Value(double d) : as_real(d), flag(ValType::R) {}
  explicit Value(size_t n) : as_uint(n), flag(ValType::U) {}
  explicit Value(Type *t) : as_type(t), flag(ValType::T) {}
  explicit Value(Func *f) : as_func(f), flag(ValType::F) {}
  explicit Value(char *p) : as_cstr(p), flag(ValType::CStr) {}
  explicit Value(Block *b) : as_block(b), flag(ValType::Block) {}

  Value() : flag(ValType::Reg) {}

  static Value GlobalCStr(size_t n) {
    Value v;
    v.flag           = ValType::GlobalCStr;
    v.as_global_cstr = n;
    return v;
  }

  static Value None() {
    Value v;
    v.flag    = ValType::T;
    v.as_type = nullptr;
    return v;
  }

  static Value Null(Type *t) {
    Value v;
    v.flag    = ValType::Null;
    v.as_null = t;
    return v;
  }

  static Value Reg(size_t n) {
    Value v;
    v.flag   = ValType::Reg;
    v.as_reg = n;
    return v;
  }

  static Value FrameAddr(size_t n) {
    Value v;
    v.flag          = ValType::FrameAddr;
    v.as_frame_addr = n;
    return v;
  }

  static Value StackAddr(size_t n) {
    Value v;
    v.flag          = ValType::StackAddr;
    v.as_stack_addr = n;
    return v;
  }

  static Value CreateGlobal();

  static Value HeapAddr(void *ptr) {
    Value v;
    v.flag         = ValType::HeapAddr;
    v.as_heap_addr = ptr;
    return v;
  }

  
  static Value Arg(size_t n) {
    Value v;
    v.flag       = IR::ValType::Arg;
    v.as_arg = n;
    return v;
  }
};

std::ostream &operator<<(std::ostream &os, const Value &value);

enum class Op : char {
#define IR_MACRO(OpCode, op_code_str, num_args, out_type) OpCode,
#include "../config/IR.conf"
#undef IR_MACRO
};

struct Cmd {
  std::vector<Value> args;
  Op op_code;

  struct Result {
    Type *type;
    size_t reg;
  } result;

  Cmd(Op op_code, bool has_ret);

  void dump(size_t indent);

  void Execute(StackFrame &frame);
};

namespace Exit {
struct Strategy {
  virtual void dump(size_t indent) = 0;
  virtual void ShowExit(int &row) = 0;
  virtual Block *JumpBlock(StackFrame &fr) = 0;
  virtual void GenerateLLVM(Func *fn,
                            const std::vector<llvm::Value *> &registers) = 0;
  virtual ~Strategy() {}

  virtual bool is_conditional() { return false; }
  virtual bool is_return() { return false; }
};

struct Switch : public Strategy {
  Switch(Value val, Block *default_block)
      : cond(val), default_block(default_block) {}
  Switch(const Switch &) = delete;
  ~Switch() {}

  void GenerateLLVM(Func *fn, const std::vector<llvm::Value *> &registers);
  Block *JumpBlock(StackFrame &fr);
  void ShowExit(int &row);
  void dump(size_t indent);
  inline void AddEntry(Value v, Block *b) { table.emplace_back(v, b); }

  Value cond;
  Block *default_block;
  std::vector<std::pair<Value, Block *>> table;
};

struct ReturnVoid : public Strategy {
  ReturnVoid() {}
  ReturnVoid(const ReturnVoid &) = delete;
  ~ReturnVoid() {}

  void GenerateLLVM(Func *fn, const std::vector<llvm::Value *> &registers);
  Block *JumpBlock(StackFrame &fr);
  void ShowExit(int &row);
  void dump(size_t indent);
};

struct Return : public Strategy {
  Return() = delete;
  Return(const Return &) = delete;
  ~Return() {}
  Return(Value v) : ret_val(v) {}

  bool is_return() { return true; }

  void GenerateLLVM(Func *fn, const std::vector<llvm::Value *> &registers);
  Block *JumpBlock(StackFrame &fr);
  void ShowExit(int &row);
  void dump(size_t indent);
  Value ret_val;
};

struct Conditional : public Strategy {
  bool is_conditional() { return true; }
  Conditional() = delete;
  Conditional(const Conditional &) = delete;
  ~Conditional() {}
  Conditional(Value v, Block *t, Block *f)
      : cond(v), true_block(t), false_block(f) {}

  void GenerateLLVM(Func *fn, const std::vector<llvm::Value *> &registers);
  Block *JumpBlock(StackFrame &fr);
  void ShowExit(int &row);
  void dump(size_t indent);
  Value cond;
  Block *true_block, *false_block;
};

struct Unconditional : public Strategy {
  Unconditional() = delete;
  Unconditional(const Unconditional &) = delete;
  ~Unconditional() {}
  Unconditional(Block *b) : block(b) {}

  void GenerateLLVM(Func *fn, const std::vector<llvm::Value *> &registers);
  Block *JumpBlock(StackFrame &fr);
  void ShowExit(int &row);
  void dump(size_t indent);
  Block *block;
};
} // namespace Exit

struct Block {
  static Block *Current;

  // Passing a char into the condition to trigger it's type to be C. We don't
  // care that it's C specifically, so long as it isn't B, Arg, or Ref.
  Block() : block_name("unnamed-block"), exit(nullptr) {}
  ~Block() {}

  void push(const Cmd &cmd) { cmds.push_back(cmd); }

  llvm::BasicBlock *
  GenerateLLVM(IR::Func *ir_fn, std::vector<llvm::Value *> &registers,
               std::vector<std::pair<IR::Block *, size_t>> &phis);

  const char *block_name;
  std::vector<Cmd> cmds;

  llvm::BasicBlock *llvm_block;

  Exit::Strategy *exit;

  inline void SetReturnVoid() {
    delete exit;
    exit = new Exit::ReturnVoid;
  }
  inline void SetReturn(Value v) {
    delete exit;
    exit = new Exit::Return(v);
  }
  inline void SetUnconditional(Block *b) {
    delete exit;
    exit = new Exit::Unconditional(b);
  }
  inline void SetConditional(Value v, Block *t, Block *f) {
    delete exit;
    exit = new Exit::Conditional(v, t, f);
  }
  inline void SetSwitch(Value cond, Block *default_block) {
    delete exit;
    exit = new Exit::Switch(cond, default_block);
  }

  void dump();
};

struct Func {
  static Func *Current;
  std::map<size_t, llvm::Value *> frame_map;

  std::vector<Block *> blocks;
  std::vector<Value *> args;
  Function *fn_type;
  llvm::Function *llvm_fn;
  llvm::BasicBlock *alloc_block;

  Block *entry() { return blocks.front(); }
  Block *exit() { return blocks AT(1); }

  void SetName(const std::string &new_name) {
    name = new_name;
    if (llvm_fn) { llvm_fn->setName(name); }
  }
  const std::string &GetName() const { return name; }

  Value Call(LocalStack *, const std::vector<Value> &);

  size_t num_cmds, frame_size;

  size_t PushSpace(Type *t);

  // NotYet - Should generate the code to this function but have not yet.
  // ToLink - This is just a stub. It should never have it's code generated
  //   Done - Code has been generated
  enum class Gen : char { NotYet, ToLink, Done };
  Gen generated;

  void GenerateLLVM();

  void PushLocal(AST::Declaration *decl);
  Block *AddBlock(const char *block_name);

  Func(Function *fn_type, bool should_gen = true);

  ~Func() {
    for (auto b : blocks) { delete b; }
  }

  void dump();

private:
  std::string name;
};

#define CMD_WITH_1_ARGS(name, out_type) Value name(Value);

#define CMD_WITH_2_ARGS(name, out_type) Value name(Value, Value);

// Intentionally empty. Must be hand implemented
#define CMD_WITH_NA_ARGS(name, out_type)

#define IR_MACRO(OpCode, op_code_str, num_args, out_type)                      \
  CMD_WITH_##num_args##_ARGS(OpCode, out_type)
#include "../config/IR.conf"
#undef IR_MACRO

Value Call(Type *out, Value, const std::vector<Value> &);
Value Store(Type *rhs_type, Value, Value);
Value Load(Type *load_type, Value);
Value Cast(Type *in, Type *out, Value);
Value Field(Structure *struct_type, Value ptr, size_t field_num);
Value Access(Type *type, Value index, Value ptr);
Value ArrayData(Array *type, Value array_ptr);
Value Malloc(Type *type, Value num);
Value Free(Value val);
Value Memcpy(Value dest, Value source, Value num_bytes);
Value PtrIncr(Pointer *type, Value ptr, Value incr);
Value Trunc(Value val);
Value ZExt(Value val);
Value PushField(Value fields, const char *name, Value ty, Value init);
Value InitFieldVec(size_t num_decls);
Value TC_Tup(const std::vector<IR::Value> &vals);

Cmd Phi(Type *ret_type);
Cmd NOp();

#undef CMD_WITH_V_ARGS
#undef CMD_WITH_1_ARGS
#undef CMD_WITH_2_ARGS

} // namespace IR

#endif // ICARUS_IR_H

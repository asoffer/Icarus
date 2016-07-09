#ifndef ICARUS_IR_H
#define ICARUS_IR_H

// Not to be confused with LLVM's IR!

namespace IR {
enum class ValType : char {
  B, C, I, R, U, T, F, CStr, Block, Reg, Arg, StackAddr, FrameAddr, HeapAddr
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
    size_t as_stack_addr;
    size_t as_frame_addr;
    size_t as_heap_addr;
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

  static Value Reg(size_t n) {
    Value v;
    v.flag       = ValType::Reg;
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

  static Value HeapAddr(size_t n) {
    Value v;
    v.flag         = ValType::HeapAddr;
    v.as_heap_addr = n;
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

// Models we should exit the block. There are a few options:
// 1. An unconditional jump to another block
// 2. A conditional jump to one of two branches
// 3. Return from the current function.
struct Exit {
  friend struct Block;
  friend Value Call(Func *, LocalStack *, const std::vector<Value> &);
  friend void RefreshDisplay(const StackFrame &, LocalStack *);

  enum class Strategy { Unset, Uncond, Cond, Return, ReturnVoid } flag;

  Value val; // This is the return value in the case of a return, and the value
             // to branch on in the case of a conditional branch.
  Block *true_block; // This is the branch to take in the case of an
                     // unconditional jump, or in the case of a conditional jump
                     // when the condition is true
  Block *false_block; // This is the branch to take in the case of a conditional
                      // jump when that condition is false.

  Exit()
      : flag(Strategy::Unset), val(false), true_block(nullptr),
        false_block(nullptr) {}

  void SetReturnVoid() { flag = Strategy::ReturnVoid; }

  void SetReturn(Value v) {
    flag = Strategy::Return;
    val  = v;
  }

  void SetTrueBranch(Block *b) {
    assert(flag == Strategy::Cond);
    true_block = b;
  }

  void SetFalseBranch(Block *b) {
    assert(flag == Strategy::Cond);
    false_block = b;
  }

  void SetUnconditional(Block *b) {
    flag       = Strategy::Uncond;
    true_block = b;
  }

  void SetConditional(Value v, Block *t, Block *f) {
    flag        = Strategy::Cond;
    val         = v;
    true_block  = t;
    false_block = f;
  }

  void dump(size_t indent);
};

struct Block {
  static Block *Current;

  // Passing a char into the condition to trigger it's type to be C. We don't
  // care that it's C specifically, so long as it isn't B, Arg, or Ref.
  Block(size_t n) : block_num(n), block_name("unnamed-block") {}
  ~Block() {}

  void push(const Cmd &cmd) { cmds.push_back(cmd); }

  llvm::BasicBlock *
  GenerateLLVM(IR::Func *ir_fn, std::vector<llvm::Value *> &registers,
               std::vector<std::pair<IR::Block *, size_t>> &phis);

  Block *ExecuteJump(StackFrame &frame);

  size_t block_num;
  const char *block_name;
  std::vector<Cmd> cmds;

  llvm::BasicBlock *llvm_block;

  Exit exit;

  void dump();
};

struct Func {
  static Func *Current;
  std::map<size_t, llvm::Value *> frame_map;

  std::vector<Block *> blocks;
  std::vector<Value *> args;
  std::string name;
  Function *fn_type;
  llvm::Function *llvm_fn;
  llvm::BasicBlock *alloc_block;

  Block *entry() { return blocks.front(); }
  size_t num_cmds, frame_size;

  size_t PushSpace(Type *t);

  void GenerateLLVM();

  void PushLocal(AST::Declaration *decl);
  Block *AddBlock(const char *block_name);

  Func(Function *fn_type, bool should_gen = true);

  ~Func() {
    for (auto b : blocks) { delete b; }
  }

  void dump();
};

Value Call(Func *f, LocalStack *local_stack,
           const std::vector<Value> &arg_vals);

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
Value PtrIncr(Pointer *type, Value ptr, Value incr);

Cmd Phi(Type *ret_type);
Cmd NOp();

#undef CMD_WITH_V_ARGS
#undef CMD_WITH_1_ARGS
#undef CMD_WITH_2_ARGS

} // namespace IR

#endif // ICARUS_IR_H

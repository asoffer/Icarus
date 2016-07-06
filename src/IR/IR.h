#ifndef ICARUS_IR_H
#define ICARUS_IR_H

// Not to be confused with LLVM's IR!

namespace IR {
enum class ValType : char {
  B, C, I, R, U, T, F, CStr, Ptr, Block, Reg, Arg, Alloc, RelAlloc
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
    void *as_ptr;
    size_t as_reg;
    size_t as_arg;
    Block *as_block;
    size_t as_alloc; // Distance from head of entire stack
    size_t as_rel_alloc; // Distance from current stack frame head
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
  explicit Value(void *p) : as_ptr(p), flag(ValType::Ptr) {}
  explicit Value(Block *b) : as_block(b), flag(ValType::Block) {}
  explicit Value(std::nullptr_t) : as_ptr(nullptr), flag(ValType::Ptr) {}

  Value() : flag(ValType::Reg) {}

  static Value Reg(size_t n) {
    Value v;
    v.flag       = ValType::Reg;
    v.as_reg = n;
    return v;
  }

  static Value RelAlloc(size_t n) {
    Value v;
    v.flag         = ValType::RelAlloc;
    v.as_rel_alloc = n;
    return v;
  }

  static Value Alloc(size_t n) {
    Value v;
    v.flag     = ValType::Alloc;
    v.as_alloc = n;
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

public:
  enum class Strategy { Unset, Uncond, Cond, Return, ReturnVoid } flag;

private:

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

public:
  void SetReturnVoid() { flag = Strategy::ReturnVoid; }

  void SetReturn(Value v) {
    flag = Strategy::Return;
    val  = v;
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

  Block *ExecuteJump(StackFrame &frame);

  size_t block_num;
  const char *block_name;
  std::vector<Cmd> cmds;

  Exit exit;

  void dump();
};

struct Func {
  static Func *Current;

  std::vector<Block *> blocks;
  std::vector<Value *> args;
  std::string name;

  Block *entry() { return blocks.front(); }
  size_t num_cmds, frame_size;

  size_t PushSpace(size_t bytes, size_t alignment);
  size_t PushSpace(Type *t);

  void PushLocal(AST::Declaration *decl);
  Block *AddBlock(const char *block_name);

  Func() : num_cmds(0), frame_size(0) {
    blocks.push_back(new Block(0));
    blocks.back()->block_name = "entry";
  }
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
Cmd Phi(Type *ret_type);
Cmd NOp();

#undef CMD_WITH_V_ARGS
#undef CMD_WITH_1_ARGS
#undef CMD_WITH_2_ARGS

} // namespace IR

#endif // ICARUS_IR_H

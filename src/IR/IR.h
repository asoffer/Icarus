#ifndef ICARUS_IR_H
#define ICARUS_IR_H

// Not to be confused with LLVM's IR!

namespace IR {
enum class ValType { B, C, I, R, U, T, F, Ptr, Ref, Arg, Alloc, RelAlloc };

struct Func;
struct Block;
struct LocalStack;
struct StackFrame;

struct Value {
  ValType flag;

  union {
    void *as_ptr;
    bool as_bool;
    char as_char;
    int as_int;
    double as_real;
    size_t as_uint;
    Type *as_type;
    Func *as_func;
    size_t as_ref;
    size_t as_arg;
    size_t as_alloc;
    size_t as_rel_alloc;
  } val;

  explicit Value(bool b) : flag(ValType::B) { val.as_bool = b; }
  explicit Value(char c) : flag(ValType::C) { val.as_char = c; }
  explicit Value(int n) : flag(ValType::I) { val.as_int = n; }
  explicit Value(double d) : flag(ValType::R) { val.as_real = d; }
  explicit Value(size_t n) : flag(ValType::U) { val.as_uint = n; }
  explicit Value(Func *f) : flag(ValType::F) { val.as_func = f; }
  explicit Value(Type *t) : flag(ValType::T) { val.as_type = t; }
  explicit Value(void *p) : flag(ValType::Ptr) { val.as_ptr = p; }
  explicit Value(std::nullptr_t) : flag(ValType::Ptr) { val.as_ptr = nullptr; }

  Value() : flag(ValType::Ref) {}

  static Value RelAlloc(size_t n) {
    Value v;
    v.flag = ValType::RelAlloc;
    v.val.as_alloc = n;
    return v;
  }

  static Value Alloc(size_t n) {
    Value v;
    v.flag = ValType::Alloc;
    v.val.as_alloc = n;
    return v;
  }
  
  static Value Arg(size_t n) {
    Value v;
    v.flag       = IR::ValType::Arg;
    v.val.as_arg = n;
    return v;
  }
};

std::ostream &operator<<(std::ostream &os, const Value &value);

enum class Op {
#define IR_MACRO(OpCode, op_code_str, num_args) OpCode,
#include "../config/IR.conf"
#undef IR_MACRO
};

struct Cmd {
  Op op_code;
  std::vector<Block *> incoming_blocks; // Only used for phi cmds
  std::vector<Value> args;

  operator Value() { return result; }
  Value result;

  Cmd(Op o, Value v);

  void dump(size_t indent = 0);

  friend Cmd CallCmd(Value);
  friend Cmd GEP(Type *t, Value lhs, std::vector<int> indices);

  // Only used for phi cmds
  friend Cmd Phi();
  void AddIncoming(Block *block, Value output_val) {
    incoming_blocks.push_back(block);
    args.push_back(output_val);
  }

  void Execute(StackFrame &frame);

private:
  Cmd();
};

// Models we should exit the block. There are a few options:
// 1. An unconditional jump to another block
// 2. A conditional jump to one of two branches
// 3. Return from the current function.
struct Exit {
  friend struct Block;
  friend Value Call(Func *, LocalStack *, const std::vector<Value> &);

private:
  enum class Strategy { Uncond, Cond, Return, ReturnVoid } flag;

  Value val; // This is the return value in the case of a return, and the value
             // to branch on in the case of a conditional branch.
  Block *true_block; // This is the branch to take in the case of an
                     // unconditional jump, or in the case of a conditional jump
                     // when the condition is true
  Block *false_block; // This is the branch to take in the case of a conditional
                      // jump when that condition is false.

  Exit()
      : flag(Strategy::Return), val(false), true_block(nullptr),
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
  Block(size_t n) : block_num(n) {}
  ~Block() {}

  void push(const Cmd &cmd) { cmds.push_back(cmd); }

  Block *ExecuteJump(StackFrame &frame);

  size_t block_num;
  std::vector<Cmd> cmds;

  Exit exit;

  void dump();
};

struct Func {
  static Func *Current;

  std::vector<Block *> blocks;
  std::vector<Value *> args;
  std::map<AST::Identifier *, size_t> frame_map;
  std::string name;

  Block *entry() { return blocks.front(); }
  size_t num_cmds, frame_size;

  void PushLocal(AST::Declaration *decl);
  Block *AddBlock();

  Func() : num_cmds(0), frame_size(0) { blocks.push_back(new Block(0)); }
  ~Func() {
    for (auto b : blocks) { delete b; }
  }

  void dump();
};

Value Call(Func *f, LocalStack *local_stack,
           const std::vector<Value> &arg_vals);

#define CMD_WITH_1_ARGS(name)                                                  \
  inline Cmd name(Value v) {                                                   \
    Cmd cmd(Op::name, v);                                                      \
    Block::Current->push(cmd);                                                 \
    return cmd;                                                                \
  }

#define CMD_WITH_2_ARGS(name)                                                  \
  inline Cmd name(Value arg1, Value arg2) {                                    \
    Cmd cmd(Op::name, arg1);                                                   \
    cmd.args.push_back(arg2); /* TODO Put this in the constructor */           \
    Block::Current->push(cmd);                                                 \
    return cmd;                                                                \
  }

inline Cmd Store(Type *t, Value arg1, Value arg2) {
  Cmd cmd(Op::Store, Value(t));
  cmd.args.push_back(arg1); /* TODO Put this in the constructor */
  cmd.args.push_back(arg2); /* TODO Put this in the constructor */
  Block::Current->push(cmd);
  return cmd;
}

inline Cmd Load(Type *t, Value arg1) {
  Cmd cmd(Op::Load, Value(t));
  cmd.args.push_back(arg1); /* TODO Put this in the constructor */
  Block::Current->push(cmd);
  return cmd;
}


// Intentionally empty. Must be hand implemented
#define CMD_WITH_NA_ARGS(name)

#define IR_MACRO(OpCode, op_code_str, num_args)                                \
  CMD_WITH_##num_args##_ARGS(OpCode)
#include "../config/IR.conf"
#undef IR_MACRO

inline Cmd Phi() {
  Cmd phi;
  phi.op_code = Op::Phi;
  // Block::Current->cmds.push_back(phi);
  return phi;
}

inline Cmd CallCmd(Value lhs) {
  Cmd call;
  call.op_code = Op::Call;
  call.args.push_back(lhs);
  return call;
}

inline Cmd GEP(Type *t, Value lhs, std::vector<int> indices) {
  Cmd call;
  call.op_code = Op::GEP;
  call.args.emplace_back(t);
  call.args.push_back(lhs);
  for (auto i : indices) { call.args.push_back(Value(i)); }
  return call;
}

#undef CMD_WITH_V_ARGS
#undef CMD_WITH_1_ARGS
#undef CMD_WITH_2_ARGS

} // namespace IR

#endif // ICARUS_IR_H

#ifndef ICARUS_IR_H
#define ICARUS_IR_H

// Not to be confused with LLVM's IR!

namespace IR {
enum class ValType { B, C, I, R, U, T, F, Ref, Arg };

struct Func;
struct Block;

struct Value {
  ValType flag;

  union {
    bool as_bool;
    char as_char;
    long as_int;
    double as_real;
    size_t as_uint;
    Type *as_type;
    Func *as_func;
    size_t as_ref;
    size_t as_arg;
  } val;

  explicit Value(bool b) : flag(ValType::B) { val.as_bool = b; }
  explicit Value(char c) : flag(ValType::C) { val.as_char = c; }
  explicit Value(long n) : flag(ValType::I) { val.as_int = n; }
  explicit Value(double d) : flag(ValType::R) { val.as_real = d; }
  explicit Value(size_t n) : flag(ValType::U) { val.as_uint = n; }
  explicit Value(Func *f) : flag(ValType::F) { val.as_func = f; }
  explicit Value(Type *t) : flag(ValType::T) { val.as_type = t; }

  Value();
};

std::ostream &operator<<(std::ostream &os, const Value &value);

enum class Op {
  BNot,
  INeg, FNeg,

  Load, Store,
  Phi,

  IAdd, UAdd, FAdd,
  ISub, USub, FSub,
  IMul, UMul, FMul,
  IDiv, UDiv, FDiv,
  IMod, UMod, FMod,

  BXor,

  ILT, ULT, FLT,
  ILE, ULE, FLE,
  IEQ, UEQ, FEQ, BEQ, CEQ, TEQ, FnEQ,
  INE, UNE, FNE, BNE, CNE, TNE, FnNE,
  IGE, UGE, FGE,
  IGT, UGT, FGT,

};

struct StackFrame {
  std::vector<Value> reg;
  std::vector<Value> allocs;
  const std::vector<Value>& args;

  size_t inst_ptr;
  Block *curr_block, *prev_block;

  StackFrame(Func *f, const std::vector<Value> &args);
};

struct Cmd {
  Op op_code;
  std::vector<Block *> incoming_blocks; // Only used for phi cmds
  std::vector<Value> args;

  operator Value() { return result; }
  Value result;

  Cmd(Op o, Value v) : op_code(o), args(1, v) {}
  void dump(size_t indent = 0);

  // Only used for phi cmds
  friend Cmd Phi();
  void AddIncoming(Block *block, Value output_val) {
    incoming_blocks.push_back(block);
    args.push_back(output_val);
  }

  void Execute(StackFrame &frame);

private:
  Cmd() {}
};

// Models we should exit the block. There are a few options:
// 1. An unconditional jump to another block
// 2. A conditional jump to one of two branches
// 3. Return from the current function.
struct Exit {
  friend struct Block;
  friend Value Call(Func *, const std::vector<Value> &);

private:
  enum class Strategy { Uncond, Cond, Return } flag;

  Value val; // This is the return value in the case of a return, and the value
             // to branch on in the case of a conditional branch.
  Block *true_block; // This is the branch to take in the case of an
                     // unconditional jump, or in the case of a conditional jump
                     // when the condition is true
  Block *false_block; // This is the branch to take in the case of a conditional
                      // jump when that condition is false.

  Exit() : flag(Strategy::Return), val(false), true_block(nullptr), false_block(nullptr) {}

public:
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
  std::string name;

  Block *entry() { return blocks.front(); }
  size_t num_cmds;

  Func() : num_cmds(0) { blocks.push_back(new Block(0)); }
  ~Func() {
    for (auto b : blocks) { delete b; }
  }

  void dump();
};

Value Call(Func *f, const std::vector<Value>& arg_vals);

#define ONE_ARG_CMD(name)                                                      \
  inline Cmd name(Value v) {                                                   \
    Cmd cmd(Op::name, v);                                                      \
    Block::Current->cmds.push_back(cmd);                                       \
    return cmd;                                                                \
  }

#define TWO_ARG_CMD(name)                                                      \
  inline Cmd name(Value arg1, Value arg2) {                                    \
    Cmd cmd(Op::name, arg1);                                                   \
    cmd.args.push_back(arg2); /* Put this in the constructor */                \
    Block::Current->cmds.push_back(cmd);                                       \
    return cmd;                                                                \
  }

ONE_ARG_CMD(BNot)

ONE_ARG_CMD(INeg)
ONE_ARG_CMD(FNeg)

ONE_ARG_CMD(Load)
TWO_ARG_CMD(Store)

inline Cmd Phi() {
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

TWO_ARG_CMD(ILT)
TWO_ARG_CMD(ULT)
TWO_ARG_CMD(FLT)

TWO_ARG_CMD(ILE)
TWO_ARG_CMD(ULE)
TWO_ARG_CMD(FLE)

TWO_ARG_CMD(BEQ)
TWO_ARG_CMD(CEQ)
TWO_ARG_CMD(IEQ)
TWO_ARG_CMD(UEQ)
TWO_ARG_CMD(FEQ)
TWO_ARG_CMD(TEQ)
TWO_ARG_CMD(FnEQ)

TWO_ARG_CMD(BNE)
TWO_ARG_CMD(CNE)
TWO_ARG_CMD(INE)
TWO_ARG_CMD(UNE)
TWO_ARG_CMD(FNE)
TWO_ARG_CMD(TNE)
TWO_ARG_CMD(FnNE)

TWO_ARG_CMD(IGE)
TWO_ARG_CMD(UGE)
TWO_ARG_CMD(FGE)

TWO_ARG_CMD(IGT)
TWO_ARG_CMD(UGT)
TWO_ARG_CMD(FGT)
} // namespace IR
#undef TWO_ARG_CMD
#undef ONE_ARG_CMD

#endif // ICARUS_IR_H

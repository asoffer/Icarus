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
  Ret,

  IAdd, UAdd, FAdd,
  ISub, USub, FSub,
  IMul, UMul, FMul,
  IDiv, UDiv, FDiv,
  IMod, UMod, FMod,

  BXor,
  /*
  ILT, ULT, FLT,
  ILE, ULE, FLE,
  IEQ, UEQ, FEQ,
  INE, UNE, FNE,
  IGE, UGE, FGE,
  IGT, UGT, FGT,
  */
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

  Value eval(const std::vector<Value> &vals, const std::vector<Value> &fn_args);

private:
  Cmd() {}
};

struct Block {
  static Block *Current;

  // Passing a char into the condition to trigger it's type to be C. We don't
  // care that it's C specifically, so long as it isn't B, Arg, or Ref.
  Block(size_t n)
      : block_num(n), true_block(nullptr), false_block(nullptr), cond('x') {}
  ~Block() {}

  void push(const Cmd &cmd) { cmds.push_back(cmd); }

  void set_conditional_jump(Value val, Block *t_block, Block *f_block) {
    cond        = val;
    true_block  = t_block;
    false_block = f_block;
  }

  void set_unconditional_jump(Block *b) {
    cond       = Value('x');
    true_block = b;
  }

  Block *execute_jump(const std::vector<Value> &vals,
                              const std::vector<Value> &fn_args);

  size_t block_num;
  std::vector<Cmd> cmds;

  // This is wacky: If the block is conditional, then you jump based on the
  // value cond. If not, then we just take true_block. In order to determine
  // which case you are in, you need to check the type of cond. If it's an Arg,
  // a Ref, or a B, then the block is conditional. Otherwise, it's
  // unconditional.
  //
  // TODO there's probably a way to use unions to simplify this, but my first
  // guess raised warnings, so I'll figure it out later.
  Block *true_block, *false_block;
  Value cond;

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

Cmd BNot(Value);

Cmd INeg(Value);
Cmd FNeg(Value);

Cmd Load(Value);
Cmd Store(Value, Value);
Cmd Phi();
Cmd Ret(Value);

Cmd IAdd(Value, Value);
Cmd UAdd(Value, Value);
Cmd FAdd(Value, Value);

Cmd ISub(Value, Value);
Cmd USub(Value, Value);
Cmd FSub(Value, Value);

Cmd IMul(Value, Value);
Cmd UMul(Value, Value);
Cmd FMul(Value, Value);

Cmd IDiv(Value, Value);
Cmd UDiv(Value, Value);
Cmd FDiv(Value, Value);

Cmd IMod(Value, Value);
Cmd UMod(Value, Value);
Cmd FMod(Value, Value);


Cmd BXor(Value, Value);
} // namespace IR

#endif // ICARUS_IR_H

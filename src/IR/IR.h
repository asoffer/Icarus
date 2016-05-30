#ifndef ICARUS_IR_H
#define ICARUS_IR_H

// Not to be confused with LLVM's IR!

namespace IR {
enum class ValType { B, C, I, R, U, T, F, Ref, Arg };

struct Func;

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
  Ret,

  IAdd, UAdd, FAdd,
  ISub, USub, FSub,
  IMul, UMul, FMul,
  IDiv, UDiv, FDiv,
  IMod, UMod, FMod
};

struct Cmd {
  Op op_code;
  std::vector<Value> args;

  operator Value() { return result; }

  Value result;
  Cmd(Op o, Value v) : op_code(o), args(1, v) {}
  void dump(size_t indent = 0);

  Value eval(const std::vector<Value> &vals, const std::vector<Value> &fn_args);
};

struct Block {
  static Block *Current;

  Block(size_t n) : block_num(n) {}
  virtual ~Block() {}

  void push(const Cmd &cmd) { cmds.push_back(cmd); }

  size_t block_num;
  std::vector<Cmd> cmds;

  void dump();
};

struct UncondBlock : public Block {
  UncondBlock(size_t n) : Block(n) {}
  virtual ~UncondBlock() {}
  Block *next_block;
};

struct CondBlock : public Block {
  CondBlock(size_t n) : Block(n) {}
  virtual ~CondBlock() {}
  Block *true_block, *false_block;
};

struct Func {
  static Func *Current;


  std::vector<Block *> blocks;
  std::vector<Value *> args;
  std::string name;
  Block *entry;
  size_t num_cmds;

  Func() : entry(new UncondBlock(0)), num_cmds(0) {}
  ~Func() {
    delete entry;
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
} // namespace IR

#endif // ICARUS_IR_H

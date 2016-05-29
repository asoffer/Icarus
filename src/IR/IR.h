#ifndef ICARUS_IR_H
#define ICARUS_IR_H

// Not to be confused with LLVM's IR!

namespace IR {
enum ValueType { X, B, C, I, R, U, T, Ref };

struct Value {
  ValueType flag;

  union {
    bool as_bool;
    char as_char;
    long as_int;
    double as_real;
    size_t as_uint;
    size_t as_ref;
    Type *as_type;
  } val;

  explicit Value(bool b) : flag(B) { val.as_bool = b; }
  explicit Value(char c) : flag(C) { val.as_char = c; }
  explicit Value(long n) : flag(I) { val.as_int = n; }
  explicit Value(double d) : flag(R) { val.as_real = d; }
  explicit Value(size_t n) : flag(U) { val.as_uint = n; }
  explicit Value(Type *t) : flag(T) { val.as_type = t; }

  Value() : flag(X) {}
};

enum class Op {
  BNot,
  INeg, FNeg,

  Load, Store,

  IAdd, UAdd, FAdd,
  ISub, USub, FSub,
  IMul, UMul, FMul,
  IDiv, UDiv, FDiv,
  IMod, UMod, FMod
};

struct Cmd {
  Op op_code;
  std::vector<Value> args;
  Value result;
  Cmd(Op o, Value v) : op_code(o), args(1, v) {}
  void dump(size_t indent = 0);
};

struct Block {
  static void Push(Block *b) { Stack.push(b); }

  static void Pop() {
    assert(!Stack.empty());
    Stack.pop();
  }
  static Block *Current() { return Stack.top(); }
  static std::stack<Block *> Stack;

  size_t block_num;
  std::vector<Cmd> cmds;

  void dump();
};

struct UncondBlock : public Block {
  Block *next_block;
};

struct CondBlock : public Block {
  Block *true_block, *false_block;
};

Value BNot(Value);

Value INeg(Value);
Value FNeg(Value);

Value Load(Value);
Value Store(Value, Value);

Value IAdd(Value, Value);
Value UAdd(Value, Value);
Value FAdd(Value, Value);

Value ISub(Value, Value);
Value USub(Value, Value);
Value FSub(Value, Value);

Value IMul(Value, Value);
Value UMul(Value, Value);
Value FMul(Value, Value);

Value IDiv(Value, Value);
Value UDiv(Value, Value);
Value FDiv(Value, Value);

Value IMod(Value, Value);
Value UMod(Value, Value);
Value FMod(Value, Value);
} // namespace IR

#endif // ICARUS_IR_H

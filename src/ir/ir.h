#ifndef ICARUS_IR_IR_H
#define ICARUS_IR_IR_H

#include <string>
#include <vector>

#include "../base/debug.h"
#include "../base/types.h"

struct Type;

namespace AST {
struct CodeBlock;
} // namespace AST

namespace IR {
struct Func;

struct BlockIndex {
  i32 value = -2;
  bool is_none() { return value == -1; }
};
inline bool operator==(BlockIndex lhs, BlockIndex rhs) {
  return lhs.value == rhs.value;
}

struct RegIndex {
  BlockIndex block_index;
  size_t instr_index;
};

struct Val {
  enum class Kind : u8 { None, Arg, Reg, Frame, Heap, Global, Const } kind;
  ::Type *type;
  union {
    u64 as_arg;
    RegIndex as_reg;
    u64 as_frame_addr;
    u64 as_heap_addr;
    u64 as_global_addr;
    bool as_bool;
    char as_char;
    double as_real;
    u16 as_u16;
    u32 as_u32;
    i64 as_int;  // TODO pick the right type here
    u64 as_uint; // TODO pick the right type here
    ::Type *as_type;
    ::IR::Func *as_func;
    AST::CodeBlock *as_code;
    BlockIndex as_block;
    char *as_cstr;
  };

  static Val Arg(Type *t, u64 n);
  static Val Reg(RegIndex r, Type *t);
  static Val FrameAddr(u64 n, Type *t);
  static Val HeapAddr(u64 n, Type *t);
  static Val GlobalAddr(u64 n, Type *t);
  static Val Bool(bool b);
  static Val Char(char c);
  static Val Real(double r);
  static Val U16(u16 n);
  static Val U32(u32 n);
  static Val Int(i64 n);
  static Val Uint(u64 n);
  static Val Type(::Type *t);
  static Val CodeBlock(AST::CodeBlock *block);
  static Val Func(::IR::Func *fn);
  static Val Void();
  static Val Block(BlockIndex bi);
  static Val Null(::Type *t);
  static Val StrLit(const char *cstr);
  static Val None() { return Val(); }

  std::string to_string() const;

  Val() : kind(Kind::Const), type(nullptr), as_bool(false) {}
};

inline bool operator==(const Val& lhs, const Val& rhs) {
  return lhs.kind == rhs.kind && lhs.type == rhs.type; // TODO check the right thing.
}
inline bool operator!= (const Val& lhs, const Val& rhs) { return !(lhs == rhs); }

enum class Op : char {
  Trunc, Extend,
  Neg, // ! for bool, - for numeric types
  Add, Sub, Mul, Div, Mod, // numeric types only
  Lt, Le, Eq, Ne, Gt, Ge, // numeric types only
  And, Or, Xor, // bool only
  Print, Malloc, Free,
  Load, Store,
  ArrayLength, ArrayData,
  PtrIncr,
  Phi, Field, Access,
  Call, Cast,
  Nop, SetReturn,
  Arrow, Array,
  Alloca,
};

struct Block;
struct Cmd;

struct StackEntry {
  StackEntry(Type *t);
  StackEntry(const StackEntry&) = delete;
  StackEntry(StackEntry &&entry) : type(entry.type), data(entry.data) {
    entry.data = nullptr;
  }
  ~StackEntry();
  Type *type; 
  void *data = nullptr;
};

struct StackFrame {
  std::vector<StackEntry> locals_;
};

struct ExecContext {
  ExecContext() = delete;
  ExecContext(const IR::Func *fn);

  const Func *current_fn;
  BlockIndex current_block;
  BlockIndex prev_block;
  u64 frame_offset = 0;

  BlockIndex ExecuteBlock();
  Val ExecuteCmd(const Cmd&);
  void Resolve(Val* v) const;

  Val reg(RegIndex index) const {
    return regs_[index.block_index.value][index.instr_index];
  }


  Val &reg(RegIndex index) {
    return regs_[index.block_index.value][index.instr_index];
  }

  Val arg(u64 n) const { return args_[n]; }

  Val stack_from_frame(const Val& v) const {
    // TODO!
    return v;
  }

  // Indexed first by block then by instruction number
  std::vector<std::vector<Val>> regs_;
  std::vector<Val> args_;
  std::vector<Val> rets_;
  std::vector<StackFrame> stack_;
};

struct Cmd {
  Cmd() : op_code(Op::Nop), result(IR::Val::None()) {}
  Cmd(Type *t, Op op, std::vector<Val> args);
  std::vector<Val> args;
  Op op_code;

  Val result; // Will always be of Kind::Reg.

  void dump(size_t indent) const;
};

Val Neg(Val v);
Val Trunc(Val v);
Val Extend(Val v);
Val Add(Val v1, Val v2);
Val Sub(Val v1, Val v2);
Val Mul(Val v1, Val v2);
Val Div(Val v1, Val v2);
Val Mod(Val v1, Val v2);
Val Lt(Val v1, Val v2);
Val Le(Val v1, Val v2);
Val Eq(Val v1, Val v2);
Val Ne(Val v1, Val v2);
Val Ge(Val v1, Val v2);
Val Gt(Val v1, Val v2);
Val And(Val v1, Val v2);
Val Or(Val v1, Val v2);
Val Xor(Val v1, Val v2);
Val Print(Val v);
Val Cast(Val result_type, Val val);
Val Call(Val fn, std::vector<Val> vals);
Val SetReturn(size_t n, Val v);
Val Access(Val index, Val val);
Val Load(Val v);
Val Store(Val val, Val loc);
Val ArrayLength(Val v);
Val ArrayData(Val v);
Val PtrIncr(Val v1, Val v2);
Val Malloc(Type *t, Val v);
Val Free(Val v);
Val Phi(Type *t);
Val Field(Val v, size_t n);
Val Arrow(Val v1, Val v2);
Val Array(Val v1, Val v2);
Val Alloca(Type *t);

struct Jump {
  static void Unconditional(BlockIndex index);
  static void Conditional(Val cond, BlockIndex true_index,
                          BlockIndex false_index);
  static void Return();

  void dump(size_t indent) const;

  Jump() : type(Type::Uncond) {}
  ~Jump() {}
  struct CondData {
    Val cond;
    BlockIndex true_block;
    BlockIndex false_block;
  };

  enum class Type : u8 { Uncond, Cond, Ret } type;
  union {
    BlockIndex block_index; // for unconditional jump
    CondData cond_data; // value and block indices to jump for conditional jump.
  };
};

struct Block {
  static BlockIndex Current;
  Block() = delete;
  Block(Func* fn) : fn_(fn) {}

  void dump(size_t indent) const;

  Func *fn_; // Containing function
  std::vector<Cmd> cmds_;
  Jump jmp_;
};

struct Func {
  static Func *Current;
  Func(::Type *t) : type(t), blocks_(2, Block(this)) {}

  void dump() const;

  static BlockIndex AddBlock() {

    BlockIndex index;
    index.value =
        static_cast<decltype(index.value)>(IR::Func::Current->blocks_.size());
    IR::Func::Current->blocks_.emplace_back(IR::Func::Current);
    return index;
  }

  BlockIndex entry() const {
    BlockIndex index;
    index.value = 0;
    return index;
  }

  BlockIndex exit() const {
    BlockIndex index;
    index.value = 1;
    return index;
  }

  std::vector<Val> Execute(std::vector<Val> args) const;

  Type *type;
  std::string name;
  std::vector<Block> blocks_;
};

struct FuncResetter {
  FuncResetter(Func *fn)
      : old_fn_(IR::Func::Current), old_block_(IR::Block::Current) {
    IR::Func::Current = fn;
  }
  ~FuncResetter() {
    IR::Func::Current = old_fn_;
    IR::Block::Current = old_block_;
  }

  Func *old_fn_;
  BlockIndex old_block_;
  bool cond_ = true;
};

#define CURRENT_FUNC(fn)                                                       \
  for (auto resetter = ::IR::FuncResetter(fn); resetter.cond_;                 \
       resetter.cond_ = false)
} // namespace IR

#endif // ICARUS_IR_IR_H

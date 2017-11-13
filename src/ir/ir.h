#ifndef ICARUS_IR_IR_H
#define ICARUS_IR_IR_H

#include <limits>
#include <memory>
#include <queue>
#include <set>
#include <stack>
#include <string>
#include <unordered_map>
#include <vector>

#include "../base/debug.h"
#include "../base/strong_types.h"
#include "../base/util.h"
#include "../base/variant.h"
#include "../input/cursor.h"

struct Type;
struct Function;
struct Enum;
struct Pointer;

extern Type *Err, *Unknown, *Bool, *Char, *Int, *Real, *Code, *Type_, *Uint,
    *Void, *NullPtr, *String;

namespace AST {
struct Expression;
struct CodeBlock;
struct ScopeLiteral;
} // namespace AST

namespace IR {
struct Val;
struct Func;
struct Block;
struct Cmd;
struct Func;
struct Register;
struct ExecContext;
} // namespace IR

namespace std {
template <> struct hash<IR::Register>;
} // namespace std

namespace IR {
struct Register {
public:
  Register() = default;
  constexpr explicit Register(i32 n) : value_(n) {}
  bool is_void() { return value_ < 0; }
  bool is_arg(const Func &fn) const;

  friend std::ostream &operator<<(std::ostream &os, Register reg);
  friend std::hash<Register>;
  friend bool operator==(Register, Register);
  friend bool operator<(Register, Register);
  friend struct ExecContext; // TODO This isn't really needed

private:
  i32 value_ = std::numeric_limits<i32>::lowest();
};

inline bool operator==(Register lhs, Register rhs) {
  return lhs.value_ == rhs.value_;
}

inline bool operator<(Register lhs, Register rhs) {
  return lhs.value_ < rhs.value_;
}

inline bool operator>(Register lhs, Register rhs) { return rhs < lhs; }
inline bool operator<=(Register lhs, Register rhs) { return !(rhs < lhs); }
inline bool operator>=(Register lhs, Register rhs) { return !(lhs < rhs); }
inline bool operator!=(Register lhs, Register rhs) { return !(lhs == rhs); }

inline std::ostream& operator<<(std::ostream& os, Register reg) {
  return os << reg.value_;
}

DEFINE_STRONG_INT(ReturnValue, i32, -1);
DEFINE_STRONG_INT(BlockIndex, i32, -1);
DEFINE_STRONG_INT(EnumVal, size_t, 0);
} // namespace IR

namespace std {
template <> struct hash<IR::Register> {
  size_t operator()(const IR::Register &reg) const noexcept {
    return hash<i32>()(reg.value_);
  }
};
} // namespace std


DEFINE_STRONG_HASH(IR::BlockIndex);
DEFINE_STRONG_HASH(IR::ReturnValue);

std::unique_ptr<IR::Func> ExprFn(AST::Expression *expr,
                                 Type *input_type = nullptr);

namespace IR {
struct CmdIndex {
  BlockIndex block;
  i32 cmd;
};

struct Addr {
  enum class Kind : u8 { Null, Global, Stack, Heap } kind;
  union {
    u64 as_global;
    u64 as_stack;
    void *as_heap;
  };

  std::string to_string() const;
};

bool operator==(Addr lhs, Addr rhs);
inline bool operator!=(Addr lhs, Addr rhs) { return !(lhs == rhs); }

struct Val {
  ::Type *type = nullptr;
  base::variant<Register, ReturnValue, ::IR::Addr, bool, char, double, i32, u64,
                EnumVal, ::Type *, ::IR::Func *, AST::ScopeLiteral *,
                AST::CodeBlock *, AST::Expression *, BlockIndex, std::string>
      value{false};

  static Val Reg(Register r, ::Type *t) { return Val(t, r); }
  static Val Ret(ReturnValue r, ::Type *t) { return Val(t, r); }
  static Val Addr(Addr addr, ::Type *t) { return Val(t, addr); }
  static Val GlobalAddr(u64 addr, ::Type *t);
  static Val HeapAddr(void *addr, ::Type *t);
  static Val StackAddr(u64 addr, ::Type *t);
  static Val Bool(bool b) { return Val(::Bool, b); }
  static Val Char(char c) { return Val(::Char, c); }
  static Val Real(double r) { return Val(::Real, r); }
  static Val Int(i32 n) { return Val(::Int, n); }
  static Val Uint(u64 n) { return Val(::Uint, n); }
  static Val Enum(const ::Enum *enum_type, size_t integral_val);
  static Val Type(::Type *t) { return Val(::Type_, t); }
  static Val CodeBlock(AST::CodeBlock *block) { return Val(::Code, block); }
  static Val Func(::IR::Func *fn);
  static Val Block(BlockIndex bi) { return Val(nullptr, bi); }
  static Val Void() { return Val(::Void, false); }
  static Val Null(::Type *t);
  static Val StrLit(std::string str) { return Val(::String, std::move(str)); }
  static Val Ref(AST::Expression *expr);
  static Val None() { return Val(); }
  static Val Scope(AST::ScopeLiteral *scope_lit);

  std::string to_string() const;

  Val() : type(nullptr), value(false) {}

private:
  template <typename T> Val(::Type *t, T val) : type(t), value(val) {}
};

inline bool operator==(const Val &lhs, const Val &rhs) {
  return lhs.type == rhs.type && lhs.value == rhs.value;
}
inline bool operator!=(const Val &lhs, const Val &rhs) { return !(lhs == rhs); }

enum class Op : char {
  Trunc, Extend,
  Neg, // ! for bool, - for numeric types
  Add, Sub, Mul, Div, Mod, // numeric types only
  Lt, Le, Eq, Ne, Gt, Ge, // numeric types only
  Xor,
  Print,
  Malloc, Free,
  Load, Store,
  ArrayLength, ArrayData, PtrIncr,
  Phi, Field, Call, Cast,
  Nop,
  SetReturn, Arrow, Array, Ptr,
  Alloca,
  Contextualize,
  CondJump,
  UncondJump,
  ReturnJump,
};

struct Stack {
  Stack() = delete;
  Stack(size_t cap) : capacity_(cap), stack_(malloc(capacity_)) {}
  Stack(const Stack &) = delete;
  Stack(Stack &&other) {
    free(stack_);
    stack_          = other.stack_;
    other.stack_    = nullptr;
    other.capacity_ = other.size_ = 0;
  }
  ~Stack() { free(stack_); }

  template <typename T> T Load(size_t index) {
    ASSERT_EQ(index & (alignof(T) - 1), 0); // Alignment error
    return *reinterpret_cast<T *>(this->location(index));
  }

  template <typename T> void Store(T val, size_t index) {
    *reinterpret_cast<T *>(this->location(index)) = val;
  }

  IR::Val Push(Pointer *ptr);

  size_t capacity_ = 0;
  size_t size_     = 0;
  void *stack_     = nullptr;

private:
  void *location(size_t index) {
    ASSERT_LT(index, capacity_);
    return reinterpret_cast<void *>(reinterpret_cast<char *>(stack_) + index);
  }
};

struct ExecContext {
  ExecContext();

  struct Frame {
    Frame() = delete;
    Frame(Func *fn, const std::vector<Val>& arguments);

    void MoveTo(BlockIndex block_index) {
      ASSERT_GE(block_index.value, 0);
      prev_    = current_;
      current_ = block_index;
    }

    Func *fn_ = nullptr;
    BlockIndex current_;
    BlockIndex prev_;

    std::vector<Val> regs_ = {};
    std::vector<Val> rets_ = {};
  };

  Block &current_block();

  std::stack<Frame> call_stack;

  BlockIndex ExecuteBlock();
  Val ExecuteCmd(const Cmd &cmd);
  void Resolve(Val *v) const;


  Val reg(Register r) const {
    ASSERT_GE(r.value_, 0);
    return call_stack.top().regs_[static_cast<u32>(r.value_)];
  }
  Val &reg(Register r) {
    ASSERT_GE(r.value_, 0);
    return call_stack.top().regs_[static_cast<u32>(r.value_)];
  }

  Stack stack_;
};

struct Cmd {
  Cmd() : op_code(Op::Nop) {}
  Cmd(Type *t, Op op, std::vector<Val> args);
  std::vector<Val> args;
  Op op_code;

  Type *type = nullptr;
  Register result;

  Val reg() const { return Val::Reg(result, type); }

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
Val Xor(Val v1, Val v2);
Val Index(Val v1, Val v2);
Val Cast(Val result_type, Val val);
Val Call(Val fn, std::vector<Val> vals);
Val Load(Val v);
Val ArrayLength(Val v);
Val ArrayData(Val v);
Val PtrIncr(Val v1, Val v2);
Val Malloc(Type *t, Val v);
Val Field(Val v, size_t n);
Val Arrow(Val v1, Val v2);
Val Array(Val v1, Val v2);
Val Ptr(Val v1);
Val Alloca(Type *t);
Val Contextualize(AST::CodeBlock *code, std::vector<IR::Val> args);

void SetReturn(ReturnValue n, Val v2);
void Print(Val v);
void Store(Val val, Val loc);
void Free(Val v);
void CondJump(Val cond, BlockIndex true_block, BlockIndex false_block);
void UncondJump(BlockIndex block);
void ReturnJump();

CmdIndex Phi(Type *t);

struct Block {
  static BlockIndex Current;
  Block()               = delete;
  Block(const Block &&) = delete;
  Block(Block &&)       = default;
  Block(Func *fn) : fn_(fn) {}

  Block &operator=(Block &&) = default;
  Block &operator=(const Block &) = delete;

  void dump(size_t indent) const;

  Func *fn_; // Containing function
  std::vector<BlockIndex> incoming_blocks_;
  std::vector<Cmd> cmds_;
};

struct Func {
  static Func *Current;
  static std::vector<std::unique_ptr<Func>> All;

  Func(::Function *fn_type);

  void dump() const;
  Val Argument(u32 n);

  int ValidateCalls(std::queue<Func *> *validation_queue) const;

  const Block &block(BlockIndex index) const { return blocks_.at(index.value); }
  Block &block(BlockIndex index) {
    return const_cast<Block &>(static_cast<const Func *>(this)->block(index));
  }

  const Cmd &Command(CmdIndex cmd_index) const {
    return blocks_.at(cmd_index.block.value).cmds_.at(cmd_index.cmd);
  }
  Cmd &Command(CmdIndex cmd_index) {
    return const_cast<Cmd &>(
        static_cast<const Func *>(this)->Command(cmd_index));
  }
  const Cmd &Command(Register reg) const {
    return Command(reg_map_.find(reg)->second);
  }
  Cmd &Command(Register reg) {
    return const_cast<Cmd &>(static_cast<const Func *>(this)->Command(reg));
  }

  void SetArgs(CmdIndex cmd_index, std::vector<IR::Val> args);

  static BlockIndex AddBlock() {
    BlockIndex index;
    index.value = static_cast<decltype(index.value)>(Current->blocks_.size());
    Current->blocks_.emplace_back(Current);
    return index;
  }

  BlockIndex entry() const { return BlockIndex(0); }

  std::vector<Val> Execute(std::vector<Val> args, ExecContext *ctx,
                           bool *were_errors);

  // Is this needed? Or can it be determined from the containing FunctionLiteral
  // object?
  ::Function *const type = nullptr;
  const size_t num_args_ = 0;
  i32 num_regs_          = 0;
  i32 num_voids_         = 0;
  std::string name;
  std::vector<Block> blocks_;
  // TODO we can probably come up with a way to more closely tie Register and
  // CmdIndex so we don't need to store this map:
  std::unordered_map<Register, CmdIndex> reg_map_;
  std::vector<AST::Expression *> preconditions_;
  // TODO many of these maps could and should be vectors except they're keyed on
  // strong ints. Consider adding a strong int vector.
  std::unordered_map<Register, std::vector<CmdIndex>> references_;
  mutable int num_errors_ = -1; // -1 indicates not yet validated
};

struct FuncResetter {
  FuncResetter(Func *fn) : old_fn_(Func::Current), old_block_(Block::Current) {
    Func::Current = fn;
  }
  ~FuncResetter() {
    Func::Current  = old_fn_;
    Block::Current = old_block_;
  }

  Func *old_fn_;
  BlockIndex old_block_;
  bool cond_ = true;
};

#define CURRENT_FUNC(fn)                                                       \
  for (auto resetter  = ::IR::FuncResetter(fn); resetter.cond_;                \
       resetter.cond_ = false)

} // namespace IR

namespace debug {
inline std::string to_string(const IR::Val &val) { return val.to_string(); }
} // namespace debug

#endif // ICARUS_IR_IR_H

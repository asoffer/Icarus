#ifndef ICARUS_IR_VAL_H
#define ICARUS_IR_VAL_H

#include <functional>
#include <iostream>
#include <variant>

#include "../base/owned_ptr.h"
#include "../base/strong_types.h"
#include "../base/types.h"

struct Enum;
struct Type;
struct Pointer;

extern Type *Bool, *Char, *Real, *Int, *Uint, *Type_, *Void, *String;

namespace AST {
struct CodeBlock;
struct Expression;
struct ScopeLiteral;
} // namespace AST

namespace IR {
DEFINE_STRONG_INT(BlockIndex, i32, -1);
DEFINE_STRONG_INT(ReturnValue, i32, -1);
DEFINE_STRONG_INT(EnumVal, size_t, 0);

struct CmdIndex {
  BlockIndex block;
  i32 cmd;
};

inline bool operator==(CmdIndex lhs, CmdIndex rhs) {
  return lhs.block == rhs.block && lhs.cmd == rhs.cmd;
}
inline bool operator<(CmdIndex lhs, CmdIndex rhs) {
  if (lhs.block.value < rhs.block.value) return true;
  if (lhs.block.value > rhs.block.value) return false;
  return lhs.cmd < rhs.cmd;
}

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

struct Func;
struct Register;
} // namespace IR

DEFINE_STRONG_HASH(IR::ReturnValue);
DEFINE_STRONG_HASH(IR::BlockIndex);

namespace std {
template <> struct hash<IR::Register>;

template <> struct hash<IR::CmdIndex> {
  size_t operator()(const IR::CmdIndex &cmd_index) const noexcept {
    u64 num = (static_cast<u64>(static_cast<u32>(cmd_index.block.value)) << 32);
    num |= static_cast<u32>(cmd_index.cmd);
    return hash<u64>()(num);
  }
};
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
  std::string to_string() const { return "r." + std::to_string(value_); }

private:
  i32 value_ = std::numeric_limits<i32>::lowest();
};
} // namespace IR

namespace std {
template <> struct hash<IR::Register> {
  size_t operator()(const IR::Register &reg) const noexcept {
    return hash<i32>()(reg.value_);
  }
};
} // namespace std

namespace IR {
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

inline std::ostream& operator<<(std::ostream& os, ::IR::Register reg) {
  return os << reg.value_;
}


struct Val {
  ::Type *type = nullptr;
  std::variant<Register, ReturnValue, ::IR::Addr, bool, char, double, i32, u64,
               EnumVal, ::Type *, ::IR::Func *, AST::ScopeLiteral *,
               base::owned_ptr<AST::CodeBlock>, AST::Expression *, BlockIndex,
               std::string>
      value{false};

  static Val Reg(Register r, ::Type *t) { return Val(t, r); }
  static Val Ret(ReturnValue r, ::Type *t) { return Val(t, r); }
  static Val Addr(Addr addr, ::Type *t);
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
  static Val CodeBlock(base::owned_ptr<AST::CodeBlock> block);
  static Val Func(::IR::Func *fn);
  static Val Block(BlockIndex bi) { return Val(nullptr, bi); }
  static Val Void() { return Val(::Void, false); }
  static Val Null(::Type *t);
  static Val NullPtr();
  static Val StrLit(std::string str) { return Val(::String, std::move(str)); }
  static Val Ref(AST::Expression *expr);
  static Val None() { return Val(); }
  static Val Scope(AST::ScopeLiteral *scope_lit);

  std::string to_string() const;

  Val()                = default;
  ~Val() noexcept      = default;
  Val(const Val &)     = default;
  Val(Val &&) noexcept = default;
  Val &operator=(const Val &) = default;
  Val &operator=(Val &&) noexcept = default;

private:
  template <typename T>
  Val(::Type *t, T &&val) : type(t), value(std::forward<T>(val)) {}
};

inline bool operator==(const Val &lhs, const Val &rhs) {
  return lhs.type == rhs.type && lhs.value == rhs.value;
}
inline bool operator!=(const Val &lhs, const Val &rhs) { return !(lhs == rhs); }
} // namespace IR

#endif // ICARUS_IR_VAL_H

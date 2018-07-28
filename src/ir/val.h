#ifndef ICARUS_IR_VAL_H
#define ICARUS_IR_VAL_H

#include <functional>
#include <iostream>
#include <memory>
#include <variant>

#include "ast/codeblock.h"
#include "base/strong_types.h"
#include "base/types.h"
#include "ir/interface.h"

struct Module;

namespace type {
struct Enum;
struct Flags;
struct Pointer;
struct Struct;
struct Type;

const Type *Void();
extern Type *Bool, *Char, *Real, *Int, *Type_, *String, *Module, *Generic;
} // namespace type


namespace AST {
struct Expression;
struct ScopeLiteral;
struct BlockLiteral;
struct Function;
} // namespace AST

DEFINE_STRONG_INT(IR, BlockIndex, i32, -1);
DEFINE_STRONG_INT(IR, EnumVal, size_t, 0);
DEFINE_STRONG_INT(IR, FlagsVal, size_t, 0);
DEFINE_STRONG_INT(IR, Register, i32, std::numeric_limits<i32>::lowest());
DEFINE_STRONG_INT(IR, BuiltinGenericIndex, i32, -1);

namespace IR {
template <typename T>
struct RegisterOr {
  static_assert(!std::is_same_v<Register, T>);
  RegisterOr(Register reg) : reg_(reg), is_reg_(true) {}
  RegisterOr(T val) : val_(val), is_reg_(false) {}

  union {
    Register reg_;
    T val_;
  };
  bool is_reg_;
};

struct BlockSequence {
  base::vector<AST::BlockLiteral *> const *seq_;
};

// TODO not really comparable. just for variant? :(
inline bool operator==(const BlockSequence &lhs, const BlockSequence &rhs) {
  return lhs.seq_ == rhs.seq_;
}

// TODO not really comparable. just for variant? :(
inline bool operator<(const BlockSequence &lhs, const BlockSequence &rhs) {
  return lhs.seq_ < rhs.seq_;
}

struct CmdIndex {
  BlockIndex block;
  i32 cmd;
};

struct ForeignFn {
  std::string_view name_;
  AST::Expression *expr_;
};
inline bool operator==(ForeignFn lhs, ForeignFn rhs) {
  return lhs.name_ == rhs.name_;
}
inline bool operator<(ForeignFn lhs, ForeignFn rhs) {
  return lhs.name_ < rhs.name_;
}
inline bool operator>(ForeignFn lhs, ForeignFn rhs) { return rhs.name_ < lhs.name_; }

inline bool operator==(CmdIndex lhs, CmdIndex rhs) {
  return lhs.block == rhs.block && lhs.cmd == rhs.cmd;
}
inline bool operator<(CmdIndex lhs, CmdIndex rhs) {
  if (lhs.block.value < rhs.block.value) return true;
  if (lhs.block.value > rhs.block.value) return false;
  return lhs.cmd < rhs.cmd;
}

struct Addr {
  enum class Kind : u8 { Null, Stack, Heap } kind;
  union {
    u64 as_stack;
    void *as_heap;
  };

  std::string to_string() const;
};

bool operator==(Addr lhs, Addr rhs);
inline bool operator!=(Addr lhs, Addr rhs) { return !(lhs == rhs); }
inline bool operator<(Addr lhs, Addr rhs) {
  u8 lhs_kind = static_cast<u8>(lhs.kind);
  u8 rhs_kind = static_cast<u8>(rhs.kind);
  if (lhs_kind < rhs_kind) { return true; }
  if (lhs_kind > rhs_kind) { return false; }
  switch (lhs.kind) {
  case Addr::Kind::Null: return false;
  case Addr::Kind::Stack: return lhs.as_stack < rhs.as_stack;
  case Addr::Kind::Heap: return lhs.as_heap < rhs.as_heap;
  }
  UNREACHABLE();
}
inline bool operator<=(Addr lhs, Addr rhs) { return !(rhs < lhs); }
inline bool operator>(Addr lhs, Addr rhs) { return rhs < lhs; }
inline bool operator>=(Addr lhs, Addr rhs) { return !(lhs < rhs); }

struct Func;
} // namespace IR

namespace std {
template <> struct hash<IR::CmdIndex> {
  size_t operator()(const IR::CmdIndex &cmd_index) const noexcept {
    u64 num = (static_cast<u64>(static_cast<u32>(cmd_index.block.value)) << 32);
    num |= static_cast<u32>(cmd_index.cmd);
    return hash<u64>()(num);
  }
};
} // namespace std

namespace IR {
struct Val {
  const type::Type *type = nullptr;
  // TODO make trivial: interface
  std::variant<Register, IR::Addr, bool, char, double, i32, EnumVal, FlagsVal,
               const type::Type *, type::Struct *, IR::Func *, AST::Function *,
               AST::ScopeLiteral *, IR::Interface, AST::Expression *,
               BlockIndex, std::string_view, const Module *, BlockSequence,
               BuiltinGenericIndex, ForeignFn>
      value{false};

  template <typename T>
  RegisterOr<T> reg_or() const {
    if (auto *r = std::get_if<Register>(&value)) {
      return RegisterOr<T>(*r);
    } else {
      return RegisterOr<T>(std::get<T>(value));
    }
  }

  static Val Reg(Register r, const type::Type *t) { return Val(t, r); }
  static Val Addr(Addr addr, const type::Type *t);
  static Val HeapAddr(void *addr, const type::Type *t);
  static Val StackAddr(u64 addr, const type::Type *t);
  static Val Bool(bool b) { return Val(type::Bool, b); }
  static Val Char(char c) { return Val(type::Char, c); }
  static Val BuiltinGeneric(i32 n) { return Val(type::Generic, BuiltinGenericIndex{n}); }
  static Val Real(double r) { return Val(type::Real, r); }
  static Val Int(i32 n) { return Val(type::Int, n); }
  static Val Enum(const type::Enum *enum_type, size_t integral_val);
  static Val Flags(const type::Flags *flags_type, size_t integral_val);
  static Val Type(const type::Type *t) { return Val(type::Type_, t); }
  static Val Type_(const type::Type *t) { return Val::Type(t); } // TODO hack to get a cmd macro to work.
  static Val CodeBlock(AST::CodeBlock block);
  static Val Foreign(const type::Type *t, ForeignFn f) { return Val(t, f); }
  static Val Func(IR::Func *fn); // TODO deprecate?
  static Val Func(AST::Function* fn);
  static Val BasicBlock(BlockIndex bi) { return Val(nullptr, bi); }
  static Val Block(AST::BlockLiteral *b);
  static Val BlockSeq(BlockSequence b);
  static Val Void() { return Val(type::Void(), false); }
  static Val Mod(const Module *mod) { return Val(type::Module, mod); }
  static Val Null(const type::Type *t);
  static Val NullPtr();
  static Val Interface(IR::Interface ifc);

  static Val CharBuf(const std::string &str);
  static Val Ref(AST::Expression *expr);
  static Val None() { return Val(); }
  static Val Scope(AST::ScopeLiteral *scope_lit);
  static Val Struct();

  std::string to_string() const;

  Val()                = default;
  ~Val() noexcept      = default;
  Val(const Val &)     = default;
  Val(Val &&) noexcept = default;
  Val &operator=(const Val &) = default;
  Val &operator=(Val &&) noexcept = default;

private:
  template <typename T>
  Val(const type::Type *t, T &&val) : type(t), value(std::forward<T>(val)) {}
};

inline bool operator==(const Val &lhs, const Val &rhs) {
  return lhs.type == rhs.type && lhs.value == rhs.value;
}
inline bool operator!=(const Val &lhs, const Val &rhs) { return !(lhs == rhs); }
bool operator<(const ::IR::Val &lhs, const ::IR::Val &rhs);
} // namespace IR

#endif // ICARUS_IR_VAL_H

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
#include "ir/register.h"

struct Module;

namespace type {
struct Enum;
struct Flags;
struct Pointer;
struct Struct;
struct Type;

const Type *Void();
extern Type const *Bool, *Char, *Real, *Int, *Type_, *String, *Module, *Generic, *NullPtr;
} // namespace type

// TODO move this type stuff into type/
template <typename T>
constexpr type::Type const *GetType() {
  if constexpr (std::is_same_v<T, bool>) {
    return type::Bool;
  } else if constexpr (std::is_same_v<T, char>) {
    return type::Char;
  } else if constexpr (std::is_same_v<T, i32>) {
    return type::Int;
  } else if constexpr (std::is_same_v<T, double>) {
    return type::Real;
  } else if constexpr (std::is_same_v<
                           std::decay_t<decltype(*std::declval<T>())>,
                           type::Type>) {
    return type::Type_;
  } else if constexpr (std::is_same_v<
                           std::decay_t<decltype(*std::declval<T>())>,
                           Module>) {
    return type::Module;
  } else {
    NOT_YET();
  }
}

namespace AST {
struct Expression;
struct ScopeLiteral;
struct BlockLiteral;
struct Function;
} // namespace AST

DEFINE_STRONG_INT(IR, BlockIndex, i32, -1);
DEFINE_STRONG_INT(IR, EnumVal, size_t, 0);
DEFINE_STRONG_INT(IR, FlagsVal, size_t, 0);
DEFINE_STRONG_INT(IR, BuiltinGenericIndex, i32, -1);

namespace IR {
inline FlagsVal operator|(FlagsVal lhs, FlagsVal rhs) {
  return FlagsVal{lhs.value | rhs.value};
}
inline FlagsVal operator^(FlagsVal lhs, FlagsVal rhs) {
  return FlagsVal{lhs.value ^ rhs.value};
}
inline FlagsVal operator&(FlagsVal lhs, FlagsVal rhs) {
  return FlagsVal{lhs.value & rhs.value};
}

struct BlockSequence {
  base::vector<AST::BlockLiteral *> const *seq_;
};
inline std::ostream &operator<<(std::ostream &os, BlockSequence b) {
  return os << base::internal::stringify(*b.seq_);
}

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

  constexpr static Addr Null() {
    Addr result{};
    result.kind = Kind::Null;
    return result;
  }

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

// TODO This is a terrible name. Pick something better.
struct AnyFunc {
  AnyFunc(Func *fn = nullptr) : fn_(fn), is_fn_(true) {}
  AnyFunc(ForeignFn foreign) : foreign_(foreign), is_fn_(false) {}
  union {
    IR::Func *fn_;
    ForeignFn foreign_;
  };
  bool is_fn_;
};

inline std::ostream& operator<<(std::ostream& os, AnyFunc a) {
  if (a.is_fn_) { return os << a.fn_; }
  return os << a.foreign_;
}

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

  template <typename T>
  explicit Val(T val) : Val(GetType<T>(), std::move(val)) {}
  explicit Val(std::nullptr_t)
      : Val(type::NullPtr, IR::Addr{Addr::Kind::Null, 0}) {}
  explicit Val(AST::ScopeLiteral *scope_lit);

  static Val Reg(Register r, const type::Type *t) { return Val(t, r); }
  static Val BuiltinGeneric(i32 n) { return Val(type::Generic, BuiltinGenericIndex{n}); }
  static Val Enum(const type::Enum *enum_type, size_t integral_val);
  static Val Flags(const type::Flags *flags_type, FlagsVal val);
  static Val CodeBlock(AST::CodeBlock block);
  static Val Foreign(const type::Type *t, ForeignFn f) { return Val(t, f); }
  static Val Func(IR::Func *fn); // TODO deprecate?
  static Val Func(AST::Function* fn);
  static Val BasicBlock(BlockIndex bi) { return Val(nullptr, bi); }
  static Val Block(AST::BlockLiteral *b);
  static Val BlockSeq(BlockSequence b);
  static Val Null(const type::Type *t);
  static Val Interface(IR::Interface ifc);

  static Val CharBuf(const std::string &str);
  static Val Ref(AST::Expression *expr);
  static Val None() { return Val(); }
  static Val Struct();

  std::string to_string() const;

  Val()                = default;
  ~Val() noexcept      = default;
  Val(const Val &)     = default;
  Val(Val &&) noexcept = default;
  Val &operator=(const Val &) = default;
  Val &operator=(Val &&) noexcept = default;

 private:
  friend Val ValFrom(RegisterOr<IR::Addr> r, type::Pointer const *ptr_type);

  template <typename T>
  Val(const type::Type *t, T &&val) : type(t), value(std::forward<T>(val)) {}
};

template <typename T>
inline Val ValFrom(RegisterOr<T> r) {
  return r.is_reg_ ? Val::Reg(r.reg_, GetType<T>()) : Val(r.val_);
}

Val ValFrom(RegisterOr<FlagsVal> r, type::Flags const *t);
Val ValFrom(RegisterOr<IR::Addr> r, type::Pointer const *ptr_type);

inline bool operator==(const Val &lhs, const Val &rhs) {
  return lhs.type == rhs.type && lhs.value == rhs.value;
}
inline bool operator!=(const Val &lhs, const Val &rhs) { return !(lhs == rhs); }
bool operator<(const ::IR::Val &lhs, const ::IR::Val &rhs);
} // namespace IR

#endif // ICARUS_IR_VAL_H

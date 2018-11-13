#ifndef ICARUS_IR_REGISTER_H
#define ICARUS_IR_REGISTER_H

#include "base/container/vector.h"
#include "base/strong_types.h"
#include "base/types.h"

DEFINE_STRONG_INT(ir, BlockIndex, i32, -1);
DEFINE_STRONG_INT(ir, EnumVal, size_t, 0);
DEFINE_STRONG_INT(ir, FlagsVal, size_t, 0);
DEFINE_STRONG_INT(ir, BuiltinGenericIndex, i32, -1);

DEFINE_STRONG_INT(ir, Register, i32, std::numeric_limits<i32>::lowest());

namespace ast {
struct Expression;
struct BlockLiteral;
}  // namespace ast

namespace ir {
struct Func;

struct BlockSequence {
  base::vector<ast::BlockLiteral *> const *seq_;
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

struct ForeignFn {
  std::string_view name_;
  ast::Expression *expr_;
};
inline bool operator==(ForeignFn lhs, ForeignFn rhs) {
  return lhs.name_ == rhs.name_;
}
inline bool operator<(ForeignFn lhs, ForeignFn rhs) {
  return lhs.name_ < rhs.name_;
}
inline bool operator>(ForeignFn lhs, ForeignFn rhs) {
  return rhs.name_ < lhs.name_;
}

// TODO This is a terrible name. Pick something better.
struct AnyFunc {
  AnyFunc(Func *fn = nullptr) : fn_(fn), is_fn_(true) {}
  AnyFunc(ForeignFn foreign) : foreign_(foreign), is_fn_(false) {}
  union {
    ir::Func *fn_;
    ForeignFn foreign_;
  };
  bool is_fn_;
};

inline std::ostream &operator<<(std::ostream &os, AnyFunc a) {
  if (a.is_fn_) { return os << a.fn_; }
  return os << a.foreign_;
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

template <typename T>
struct RegisterOr {
  using type = T;
  static_assert(!std::is_same_v<Register, T>);
  RegisterOr() : reg_(-1), is_reg_(true) {}

  RegisterOr(Register reg) : reg_(reg), is_reg_(true) {}
  RegisterOr(T val) : val_(val), is_reg_(false) {}

  union {
    Register reg_;
    T val_;
  };
  bool is_reg_;
};

template <typename T>
struct IsRegOr : public std::false_type {};
template <typename T>
struct IsRegOr<RegisterOr<T>> : public std::true_type {};
}  // namespace ir

#endif  // ICARUS_IR_REGISTER_H

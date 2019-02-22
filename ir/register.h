#ifndef ICARUS_IR_REGISTER_H
#define ICARUS_IR_REGISTER_H

#include <vector>
#include "base/strong_types.h"

DEFINE_STRONG_INT(ir, BlockIndex, int32_t, -1);
DEFINE_STRONG_INT(ir, EnumVal, size_t, 0);
DEFINE_STRONG_INT(ir, BuiltinGenericIndex, int32_t, -1);

namespace ir {
#define MAKE_CMP(type)                                                         \
  constexpr bool operator<(type lhs, type rhs) {                               \
    return lhs.value < rhs.value;                                              \
  }                                                                            \
  constexpr bool operator<=(type lhs, type rhs) { return !(rhs < lhs); }       \
  constexpr bool operator>(type lhs, type rhs) { return rhs < lhs; }           \
  constexpr bool operator>=(type lhs, type rhs) { return !(lhs < rhs); }
MAKE_CMP(BlockIndex)
MAKE_CMP(EnumVal)
MAKE_CMP(BuiltinGenericIndex)
#undef MAKE_CMP
}  // namespace ir

namespace ast {
struct Expression;
struct BlockLiteral;
}  // namespace ast

namespace ir {
struct Reg {
 public:
  constexpr Reg() : val_(std::numeric_limits<uint64_t>::max()){};
  constexpr explicit Reg(uint64_t val) : val_(val & ~(arg_mask | out_mask)) {}
  constexpr static Reg Arg(uint64_t val) { return MakeReg(val | arg_mask); }
  constexpr static Reg Out(uint64_t val) { return MakeReg(val | arg_mask); }

  constexpr bool is_argument() { return val_ & arg_mask; }
  constexpr bool is_out() { return val_ & out_mask; }
  constexpr auto value() const { return val_; }

  std::string to_string() const { return "r." + std::to_string(val_); }

 private:
  constexpr static Reg MakeReg(uint64_t val) {
    Reg r;
    r.val_ = val;
    return r;
  }

  constexpr static uint64_t arg_mask = 0x8000'0000'0000'0000;
  constexpr static uint64_t out_mask = 0x4000'0000'0000'0000;
  uint64_t val_;
};

using Register = Reg;

constexpr bool operator==(Reg lhs, Reg rhs) {
  return lhs.value() == rhs.value();
}

constexpr bool operator<(Reg lhs, Reg rhs) { return lhs.value() < rhs.value(); }
constexpr bool operator!=(Reg lhs, Reg rhs) { return !(lhs == rhs); }
constexpr bool operator>(Reg lhs, Reg rhs) { return rhs < lhs; }
constexpr bool operator<=(Reg lhs, Reg rhs) { return !(lhs > rhs); }
constexpr bool operator>=(Reg lhs, Reg rhs) { return !(lhs < rhs); }

inline std::ostream &operator<<(std::ostream &os, Reg r) {
  return os << "r." << r.value();
}

}  // namespace ir

namespace std {
template <>
struct hash<ir::Reg> {
  size_t operator()(ir::Reg r) const { return hash<size_t>{}(r.value()); }
};
}  // namespace std

namespace ir {
struct Func;

struct BlockSequence {
  std::vector<ast::BlockLiteral *> const *seq_;
};
inline std::ostream &operator<<(std::ostream &os, BlockSequence b) {
  return os << base::stringify(*b.seq_);
}

// TODO not really comparable. just for variant? :(
inline bool operator==(const BlockSequence &lhs, const BlockSequence &rhs) {
  return lhs.seq_ == rhs.seq_;
}

// TODO not really comparable. just for variant? :(
inline bool operator<(const BlockSequence &lhs, const BlockSequence &rhs) {
  return lhs.seq_ < rhs.seq_;
}

inline std::ostream &operator<<(std::ostream &os, EnumVal e) {
  return os << e.value;
}

inline std::ostream &operator<<(std::ostream &os, BlockIndex b) {
  return os << "block." << b.value;
}

template <typename T>
struct RegisterOr {
  using type = T;
  static_assert(!std::is_same_v<Register, T>);
  RegisterOr() : is_reg_(true) {}

  RegisterOr(Register reg) : reg_(reg), is_reg_(true) {}
  RegisterOr(T val) : val_(val), is_reg_(false) {}

  union {
    Register reg_;
    T val_;
  };
  bool is_reg_;

  inline friend std::ostream &operator<<(std::ostream &os,
                                         RegisterOr const &r) {
    if (r.is_reg_) {
      return os << r.reg_;
    } else {
      return os << r.val_;
    }
  }
};

template <typename T>
bool operator==(RegisterOr<T> const&lhs, RegisterOr<T>const&rhs) {
  if (lhs.is_reg_) { return rhs.is_reg_ && lhs.reg_ == rhs.reg_; }
  return !rhs.is_reg_ && lhs.val_ == rhs.val_;
}

template<typename T>
std::string stringify(RegisterOr<T> const& r) {
  std::stringstream ss;
  ss << r;
  return ss.str();
}

template <typename T>
struct TypedRegister : public Register {
  using type = T;
  TypedRegister(Register r) : Register(r) {}
  operator RegisterOr<T>() const { return static_cast<Register>(*this); }
};

template <typename T>
struct IsRegOr : public std::false_type {};
template <typename T>
struct IsRegOr<RegisterOr<T>> : public std::true_type {};

template <typename T>
struct IsTypedReg : public std::false_type {};
template <typename T>
struct IsTypedReg<TypedRegister<T>> : public std::true_type {};

}  // namespace ir

#endif  // ICARUS_IR_REGISTER_H

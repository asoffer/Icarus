#ifndef ICARUS_IR_REGISTER_H
#define ICARUS_IR_REGISTER_H

#include <sstream>
#include <vector>

#include "base/strong_types.h"
#include "ir/reg.h"

DEFINE_STRONG_INT(ir, BlockIndex, int32_t, -1);
DEFINE_STRONG_INT(ir, EnumVal, size_t, 0);

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
#undef MAKE_CMP
}  // namespace ir

namespace ast {
struct Expression;
struct BlockLiteral;
}  // namespace ast

namespace ir {

inline std::ostream &operator<<(std::ostream &os, EnumVal e) {
  return os << e.value;
}

inline std::ostream &operator<<(std::ostream &os, BlockIndex b) {
  return os << "block." << b.value;
}

template <typename T>
struct TypedRegister : public Reg {
  using type = T;
  TypedRegister(Reg r) : Reg(r) {}
  operator RegOr<T>() const { return static_cast<Reg>(*this); }
};

template <typename T>
struct IsTypedReg : public std::false_type {};
template <typename T>
struct IsTypedReg<TypedRegister<T>> : public std::true_type {};

}  // namespace ir

#endif  // ICARUS_IR_REGISTER_H

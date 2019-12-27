#ifndef ICARUS_IR_CMD_BASIC_H
#define ICARUS_IR_CMD_BASIC_H

#include <functional>

#include "base/untyped_buffer.h"
#include "ir/cmd/util.h"
#include "ir/reg.h"
#include "type/util.h"

namespace ir {

// Note: We need to write these ourselves because we need to specif the return
// type to avoid integral promotion.
struct Addition {
  template <typename T>
  T operator()(T lhs, T rhs) {
    return lhs + rhs;
  }
};

struct Subtraction {
  template <typename T>
  T operator()(T lhs, T rhs) {
    return lhs - rhs;
  }
};

struct Multiplication {
  template <typename T>
  T operator()(T lhs, T rhs) {
    return lhs * rhs;
  }
};

struct Division {
  template <typename T>
  T operator()(T lhs, T rhs) {
    return lhs / rhs;
  }
};

struct Modulus {
  template <typename T>
  T operator()(T lhs, T rhs) {
    return lhs % rhs;
  }
};

using AddCmd = internal::BinaryCmd<1 * 256, Addition,  //
                                   int8_t, int16_t, int32_t, int64_t, uint8_t,
                                   uint16_t, uint32_t, uint64_t, float, double>;
using SubCmd = internal::BinaryCmd<2 * 256, Subtraction,  //
                                   int8_t, int16_t, int32_t, int64_t, uint8_t,
                                   uint16_t, uint32_t, uint64_t, float, double>;
using MulCmd = internal::BinaryCmd<3 * 256, Multiplication,  //
                                   int8_t, int16_t, int32_t, int64_t, uint8_t,
                                   uint16_t, uint32_t, uint64_t, float, double>;
using DivCmd = internal::BinaryCmd<4 * 256, Division,  //
                                   int8_t, int16_t, int32_t, int64_t, uint8_t,
                                   uint16_t, uint32_t, uint64_t, float, double>;
using ModCmd = internal::BinaryCmd<5 * 256, Modulus,  //
                                   int8_t, int16_t, int32_t, int64_t, uint8_t,
                                   uint16_t, uint32_t, uint64_t>;
using LtCmd  = internal::BinaryCmd<6 * 256, std::less<>,  //
                                  int8_t, int16_t, int32_t, int64_t, uint8_t,
                                  uint16_t, uint32_t, uint64_t, FlagsVal>;
using LeCmd  = internal::BinaryCmd<7 * 256, std::less_equal<>,  //
                                  int8_t, int16_t, int32_t, int64_t, uint8_t,
                                  uint16_t, uint32_t, uint64_t, FlagsVal>;
using EqCmd  = internal::BinaryCmd<8 * 256, std::equal_to<>,  //
                                  bool, int8_t, int16_t, int32_t, int64_t,
                                  uint8_t, uint16_t, uint32_t, uint64_t,
                                  FlagsVal, EnumVal, type::Type const*>;
using NeCmd  = internal::BinaryCmd<9 * 256, std::not_equal_to<>,  //
                                  bool, int8_t, int16_t, int32_t, int64_t,
                                  uint8_t, uint16_t, uint32_t, uint64_t,
                                  FlagsVal, EnumVal, type::Type const*>;
using NegCmd      = internal::UnaryCmd<10 * 256, std::negate<>, int8_t, int16_t,
                                  int32_t, int64_t, float, double>;
using NotCmd      = internal::UnaryCmd<11 * 256, std::logical_not<>, bool>;
using XorFlagsCmd = internal::BinaryCmd<20 * 256, std::bit_xor<>, FlagsVal>;
using AndFlagsCmd = internal::BinaryCmd<21 * 256, std::bit_and<>, FlagsVal>;
using OrFlagsCmd  = internal::BinaryCmd<22 * 256, std::bit_or<>, FlagsVal>;
}  // namespace ir

#endif  // ICARUS_IR_CMD_BASIC_H

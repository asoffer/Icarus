#ifndef ICARUS_IR_CMD_BASIC_H
#define ICARUS_IR_CMD_BASIC_H

#include <functional>
#include <optional>

#include "ir/cmd/util.h"
#include "ir/cmd_buffer.h"
#include "ir/reg.h"
#include "type/util.h"

namespace ir {
using AddCmd = internal::BinaryCmd<1, std::plus<>,  //
                                   int8_t, int16_t, int32_t, int64_t, uint8_t,
                                   uint16_t, uint32_t, uint64_t, float, double>;
using SubCmd = internal::BinaryCmd<2, std::minus<>,  //
                                   int8_t, int16_t, int32_t, int64_t, uint8_t,
                                   uint16_t, uint32_t, uint64_t, float, double>;
using MulCmd = internal::BinaryCmd<3, std::multiplies<>,  //
                                   int8_t, int16_t, int32_t, int64_t, uint8_t,
                                   uint16_t, uint32_t, uint64_t, float, double>;
using DivCmd = internal::BinaryCmd<4, std::divides<>,  //
                                   int8_t, int16_t, int32_t, int64_t, uint8_t,
                                   uint16_t, uint32_t, uint64_t, float, double>;
using ModCmd = internal::BinaryCmd<5, std::modulus<>,  //
                                   int8_t, int16_t, int32_t, int64_t, uint8_t,
                                   uint16_t, uint32_t, uint64_t>;
using LtCmd  = internal::BinaryCmd<6, std::less<>,  //
                                  int8_t, int16_t, int32_t, int64_t, uint8_t,
                                  uint16_t, uint32_t, uint64_t, FlagsVal>;
using LeCmd  = internal::BinaryCmd<7, std::less_equal<>,  //
                                  int8_t, int16_t, int32_t, int64_t, uint8_t,
                                  uint16_t, uint32_t, uint64_t, FlagsVal>;
using EqCmd =
    internal::BinaryCmd<8, std::equal_to<>,  //
                        bool, int8_t, int16_t, int32_t, int64_t, uint8_t,
                        uint16_t, uint32_t, uint64_t, FlagsVal, EnumVal>;
using NeCmd =
    internal::BinaryCmd<9, std::not_equal_to<>,  //
                        bool, int8_t, int16_t, int32_t, int64_t, uint8_t,
                        uint16_t, uint32_t, uint64_t, FlagsVal, EnumVal>;
using GeCmd = internal::BinaryCmd<10, std::greater_equal<>,  //
                                  int8_t, int16_t, int32_t, int64_t, uint8_t,
                                  uint16_t, uint32_t, uint64_t, FlagsVal>;
using GtCmd =
    internal::BinaryCmd<11, std::greater<>, int8_t, int16_t, int32_t, int64_t,
                        uint8_t, uint16_t, uint32_t, uint64_t, FlagsVal>;
using NegCmd = internal::UnaryCmd<12, std::negate<>, int8_t, int16_t, int32_t,
                                  int64_t, float, double>;
using NotCmd = internal::UnaryCmd<15, std::logical_not<>, bool>;
using XorFlagsCmd = internal::BinaryCmd<23, std::bit_xor<>, FlagsVal>;
using AndFlagsCmd = internal::BinaryCmd<24, std::bit_and<>, FlagsVal>;
using OrFlagsCmd  = internal::BinaryCmd<25, std::bit_or<>, FlagsVal>;
}  // namespace ir

#endif  // ICARUS_IR_CMD_BASIC_H

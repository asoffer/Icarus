#ifndef ICARUS_IR_CMD_BASIC_H
#define ICARUS_IR_CMD_BASIC_H

#include <functional>
#include <optional>

#include "ir/basic_block.h"
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
                        int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t,
                        uint32_t, uint64_t, FlagsVal, EnumVal>;
using NeCmd =
    internal::BinaryCmd<9, std::not_equal_to<>,  //
                        int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t,
                        uint32_t, uint64_t, FlagsVal, EnumVal>;
using GeCmd = internal::BinaryCmd<10, std::greater_equal<>,  //
                                  int8_t, int16_t, int32_t, int64_t, uint8_t,
                                  uint16_t, uint32_t, uint64_t, FlagsVal>;
using GtCmd  = internal::BinaryCmd<11, std::greater<>,  
                                  int8_t, int16_t, int32_t, int64_t, uint8_t,
                                  uint16_t, uint32_t, uint64_t, FlagsVal>;
using NegCmd = internal::UnaryCmd<12, std::negate<>, int8_t, int16_t, int32_t,
                                  int64_t, float, double>;
using NotCmd = internal::UnaryCmd<15, std::logical_not<>, bool>;
using XorFlagsCmd = internal::BinaryCmd<23, std::bit_xor<>, FlagsVal>;
using AndFlagsCmd = internal::BinaryCmd<24, std::bit_and<>, FlagsVal>;
using OrFlagsCmd  = internal::BinaryCmd<25, std::bit_or<>, FlagsVal>;

constexpr inline auto Add      = internal::BinaryHandler<AddCmd>{};
constexpr inline auto Sub      = internal::BinaryHandler<SubCmd>{};
constexpr inline auto Mul      = internal::BinaryHandler<MulCmd>{};
constexpr inline auto Div      = internal::BinaryHandler<DivCmd>{};
constexpr inline auto Mod      = internal::BinaryHandler<ModCmd>{};
constexpr inline auto Lt       = internal::BinaryHandler<LtCmd>{};
constexpr inline auto Le       = internal::BinaryHandler<LeCmd>{};
constexpr inline auto Eq       = internal::BinaryHandler<EqCmd>{};
constexpr inline auto Ne       = internal::BinaryHandler<NeCmd>{};
constexpr inline auto Ge       = internal::BinaryHandler<GeCmd>{};
constexpr inline auto Gt       = internal::BinaryHandler<GtCmd>{};
constexpr inline auto Neg      = internal::UnaryHandler<NegCmd>{};
constexpr inline auto Not      = internal::UnaryHandler<NotCmd>{};
constexpr inline auto XorFlags = internal::BinaryHandler<XorFlagsCmd>{};
constexpr inline auto AndFlags = internal::BinaryHandler<AndFlagsCmd>{};
constexpr inline auto OrFlags  = internal::BinaryHandler<OrFlagsCmd>{};
}  // namespace ir

#endif  // ICARUS_IR_CMD_BASIC_H

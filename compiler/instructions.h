#ifndef ICARUS_COMPILER_INSTRUCTIONS_H
#define ICARUS_COMPILER_INSTRUCTIONS_H

#include "ir/instruction/arithmetic.h"
#include "ir/instruction/compare.h"
#include "ir/instruction/core.h"
#include "ir/instruction/instructions.h"
#include "ir/instruction/set.h"
#include "ir/value/char.h"
#include "ir/value/value.h"
#include "type/array.h"
#include "type/enum.h"
#include "type/flags.h"
#include "type/function.h"
#include "type/opaque.h"
#include "type/pointer.h"
#include "type/slice.h"
#include "type/struct.h"

namespace compiler {

namespace internal_instructions {

// TODO: Include ModInstruction, but only for non-floating-point types.
template <typename... Ts>
using ArithmeticInstructions =
    ir::InstructionSet<ir::AddInstruction<Ts>..., ir::SubInstruction<Ts>...,
                       ir::MulInstruction<Ts>..., ir::DivInstruction<Ts>...>;
template <typename... Ts>
using EqualityComparisonInstructions =
    ir::InstructionSet<ir::EqInstruction<Ts>..., ir::NeInstruction<Ts>...>;
template <typename... Ts>
using OrderedComparisonInstructions =
    ir::InstructionSet<ir::LtInstruction<Ts>..., ir::LeInstruction<Ts>...,
                       EqualityComparisonInstructions<Ts...>>;
template <typename... Ts>
using CastInstructions = ir::InstructionSet<ir::CastInstruction<Ts>...>;

}  // namespace internal_instructions

struct instruction_set_t
    : ir::InstructionSet<
          ir::CoreInstructions, ir::AddInstruction<uint8_t>,
          internal_instructions::ArithmeticInstructions<
              uint8_t, int8_t, uint16_t, int16_t, uint32_t, int32_t, uint64_t,
              int64_t, float, double>,
          ir::PtrDiffInstruction, ir::ModInstruction<uint8_t>,
          ir::ModInstruction<int8_t>, ir::ModInstruction<uint16_t>,
          ir::ModInstruction<int16_t>, ir::ModInstruction<uint32_t>,
          ir::ModInstruction<int32_t>, ir::ModInstruction<uint64_t>,
          ir::ModInstruction<int64_t>,
          internal_instructions::EqualityComparisonInstructions<
              bool, uint8_t, int8_t, uint16_t, int16_t, uint32_t, int32_t,
              uint64_t, int64_t, float, double, type::Type, ir::Addr>,
          internal_instructions::OrderedComparisonInstructions<
              ir::Char, uint8_t, int8_t, uint16_t, int16_t, uint32_t, int32_t,
              uint64_t, int64_t, float, double, ir::Addr>,
          ir::NegInstruction<int8_t>, ir::NegInstruction<int16_t>,
          ir::NegInstruction<int32_t>, ir::NegInstruction<int64_t>,
          ir::NegInstruction<float>, ir::NegInstruction<double>,
          internal_instructions::CastInstructions<
              ir::Char(uint8_t), ir::Char(int8_t), int8_t(ir::Char),
              uint8_t(ir::Char), int16_t(ir::Char), uint16_t(ir::Char),
              int32_t(ir::Char), uint32_t(ir::Char), int64_t(ir::Char),
              uint64_t(ir::Char), uint8_t(int8_t), uint8_t(uint16_t),
              uint8_t(int16_t), uint8_t(uint32_t), uint8_t(int32_t),
              uint8_t(uint64_t), uint8_t(int64_t), uint8_t(float),
              uint8_t(double), int8_t(uint8_t), int8_t(uint16_t),
              int8_t(int16_t), int8_t(uint32_t), int8_t(int32_t),
              int8_t(uint64_t), int8_t(int64_t), int8_t(float), int8_t(double),
              uint16_t(uint8_t), uint16_t(int8_t), uint16_t(int16_t),
              uint16_t(uint32_t), uint16_t(int32_t), uint16_t(uint64_t),
              uint16_t(int64_t), uint16_t(float), uint16_t(double),
              int16_t(uint8_t), int16_t(int8_t), int16_t(uint16_t),
              int16_t(uint32_t), int16_t(int32_t), int16_t(uint64_t),
              int16_t(int64_t), int16_t(float), int16_t(double),
              uint32_t(uint8_t), uint32_t(int8_t), uint32_t(uint16_t),
              uint32_t(int16_t), uint32_t(int32_t), uint32_t(uint64_t),
              uint32_t(int64_t), uint32_t(float), uint32_t(double),
              uint32_t(uint8_t), int32_t(int8_t), int32_t(uint16_t),
              int32_t(int16_t), int32_t(uint32_t), int32_t(uint64_t),
              int32_t(int64_t), int32_t(float), int32_t(double),
              int32_t(uint8_t), uint64_t(int8_t), uint64_t(uint16_t),
              uint64_t(int16_t), uint64_t(uint32_t), uint64_t(int32_t),
              uint64_t(int64_t), uint64_t(float), uint64_t(double),
              uint64_t(uint8_t), int64_t(int8_t), int64_t(uint16_t),
              int64_t(int16_t), int64_t(uint32_t), int64_t(int32_t),
              int64_t(uint64_t), int64_t(float), int64_t(double),
              int64_t(uint8_t), float(int8_t), float(uint16_t), float(int16_t),
              float(uint32_t), float(int32_t), float(uint64_t), float(int64_t),
              float(double), double(int8_t), double(uint16_t), double(int16_t),
              double(uint32_t), double(int32_t), double(uint64_t),
              double(int64_t), double(float)>,
          ir::AndInstruction, ir::NotInstruction, type::SliceInstruction,
          type::XorFlagsInstruction, type::AndFlagsInstruction,
          type::OrFlagsInstruction, type::PtrInstruction,
          type::BufPtrInstruction, type::OpaqueTypeInstruction,
          type::FunctionTypeInstruction, ir::LoadSymbolInstruction,
          type::ArrayInstruction, type::StructInstruction,
          ir::MakeBlockInstruction, ir::MakeScopeInstruction,
          ir::StructIndexInstruction, ir::PtrIncrInstruction,
          type::EnumInstruction, type::FlagsInstruction,
          ir::TypeInfoInstruction, ir::InitInstruction, ir::DestroyInstruction,
          ir::MoveInitInstruction, ir::CopyInitInstruction, ir::MoveInstruction,
          ir::CopyInstruction, type::SliceLengthInstruction,
          type::SliceDataInstruction, ir::DebugIrInstruction,
          ir::AbortInstruction> {};

}  // namespace compiler

#endif  // ICARUS_COMPILER_INSTRUCTIONS_H

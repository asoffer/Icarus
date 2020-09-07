#ifndef ICARUS_IR_INTERPRETTER_EVALUATE_H
#define ICARUS_IR_INTERPRETTER_EVALUATE_H

#include <type_traits>

#include "base/debug.h"
#include "base/expected.h"
#include "base/untyped_buffer.h"
#include "ir/compiled_fn.h"
#include "ir/instruction/set.h"
#include "ir/interpretter/evaluation_failure.h"
#include "ir/value/value.h"

namespace interpretter {
template <typename... Ts>
using CoreInstructions =
    ir::InstructionSet<ir::RequiredCapabilities(),
                       ir::RegisterInstruction<Ts>...,
                       ir::StoreInstruction<Ts>..., ir::PhiInstruction<Ts>...>;

// TODO: Include ModInstruction, but only for non-floating-point types.
template <typename... Ts>
using ArithmeticInstructions =
    ir::InstructionSet<ir::RequiredCapabilities(), ir::AddInstruction<Ts>...,
                       ir::SubInstruction<Ts>..., ir::MulInstruction<Ts>...,
                       ir::DivInstruction<Ts>...>;
template <typename... Ts>
using EqualityComparisonInstructions =
    ir::InstructionSet<ir::RequiredCapabilities(), ir::EqInstruction<Ts>...,
                       ir::NeInstruction<Ts>...>;
template <typename... Ts>
using OrderedComparisonInstructions =
    ir::InstructionSet<ir::RequiredCapabilities(), ir::LtInstruction<Ts>...,
                       ir::LeInstruction<Ts>...,
                       EqualityComparisonInstructions<Ts...>>;

using instruction_set_t = ir::InstructionSet<
    ir::RequiredCapabilities(), ir::AddInstruction<uint8_t>,
    ArithmeticInstructions<uint8_t, int8_t, uint16_t, int16_t, uint32_t,
                           int32_t, uint64_t, int64_t, float, double>,
    ir::ModInstruction<uint8_t>, ir::ModInstruction<int8_t>,
    ir::ModInstruction<uint16_t>, ir::ModInstruction<int16_t>,
    ir::ModInstruction<uint32_t>, ir::ModInstruction<int32_t>,
    ir::ModInstruction<uint64_t>, ir::ModInstruction<int64_t>,
    EqualityComparisonInstructions<bool, uint8_t, int8_t, uint16_t, int16_t,
                                   uint32_t, int32_t, uint64_t, int64_t, float,
                                   double, ir::FlagsVal, type::Type const *,
                                   ir::Addr, ir::EnumVal>,
    OrderedComparisonInstructions<uint8_t, int8_t, uint16_t, int16_t, uint32_t,
                                  int32_t, uint64_t, int64_t, float, double,
                                  ir::FlagsVal>,
    ir::NegInstruction<int8_t>, ir::NegInstruction<int16_t>,
    ir::NegInstruction<int32_t>, ir::NegInstruction<int64_t>,
    ir::NegInstruction<float>, ir::NegInstruction<double>,
    CoreInstructions<bool, uint8_t, int8_t, uint16_t, int16_t, uint32_t,
                     int32_t, uint64_t, int64_t, float, double, ir::FlagsVal,
                     type::Type const *, ir::Addr, ir::EnumVal, ir::String,
                     ir::Fn>,
    ir::SetReturnInstruction<uint8_t>, ir::SetReturnInstruction<int8_t>,
    ir::SetReturnInstruction<uint16_t>, ir::SetReturnInstruction<int16_t>,
    ir::SetReturnInstruction<uint32_t>, ir::SetReturnInstruction<int32_t>,
    ir::SetReturnInstruction<uint64_t>, ir::SetReturnInstruction<int64_t>,
    ir::SetReturnInstruction<float>, ir::SetReturnInstruction<double>,
    ir::SetReturnInstruction<type::Type const *>,
    ir::SetReturnInstruction<ir::Addr>, ir::SetReturnInstruction<ir::EnumVal>,
    ir::SetReturnInstruction<ir::FlagsVal>, ir::SetReturnInstruction<bool>,
    ir::SetReturnInstruction<ir::String>, ir::SetReturnInstruction<ir::Fn>,
    ir::SetReturnInstruction<core::Bytes>,
    ir::SetReturnInstruction<core::Alignment>,
    ir::SetReturnInstruction<ir::BlockDef const *>,
    ir::SetReturnInstruction<ir::ScopeDef const *>,
    ir::SetReturnInstruction<module::BasicModule const *>,
    ir::SetReturnInstruction<ir::GenericFn>,
    ir::SetReturnInstruction<ir::Jump *>,
    ir::SetReturnInstruction<type::GenericStruct const *>,
    ir::CastInstruction<uint8_t>, ir::CastInstruction<int8_t>,
    ir::CastInstruction<uint16_t>, ir::CastInstruction<int16_t>,
    ir::CastInstruction<uint32_t>, ir::CastInstruction<int32_t>,
    ir::CastInstruction<uint64_t>, ir::CastInstruction<int64_t>,
    ir::CastInstruction<float>, ir::CastInstruction<double>, ir::NotInstruction,
    ir::XorFlagsInstruction, ir::AndFlagsInstruction, ir::OrFlagsInstruction,
    ir::PtrInstruction, ir::BufPtrInstruction, ir::GetReturnInstruction,
    ir::OpaqueTypeInstruction, ir::ArrowInstruction, ir::CallInstruction,
    ir::LoadSymbolInstruction, ir::ArrayInstruction, ir::StructInstruction,
    ir::MakeBlockInstruction, ir::MakeScopeInstruction,
    ir::StructIndexInstruction, ir::TupleIndexInstruction,
    ir::PtrIncrInstruction, ir::TupleInstruction, ir::EnumerationInstruction,
    ir::TypeInfoInstruction, ir::TypeManipulationInstruction,
    ir::ByteViewLengthInstruction, ir::ByteViewDataInstruction,
    ir::DebugIrInstruction>;

void Execute(ir::Fn fn, base::untyped_buffer arguments,
             absl::Span<ir::Addr const> ret_slots);

base::expected<ir::Value, EvaluationFailure> Evaluate(ir::CompiledFn &&fn);

// TODO wrap output in expected.
void Execute(ir::CompiledFn &&fn);

// TODO wrap output in expected.
base::untyped_buffer EvaluateToBuffer(ir::CompiledFn &&fn);

// TODO wrap output in expected.
template <typename T>
T EvaluateAs(ir::CompiledFn &&fn) {
  static_assert(std::is_trivially_copyable_v<T>);
  base::untyped_buffer result_buf = EvaluateToBuffer(std::move(fn));
  ASSERT(result_buf.size() == sizeof(T));
  return result_buf.get<T>(0);
}

}  // namespace interpretter

#endif  // ICARUS_IR_INTERPRETTER_EVALUATE_H

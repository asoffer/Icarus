#include "ir/interpretter/evaluate.h"

#include "ast/expression.h"
#include "ir/compiled_fn.h"
#include "ir/instruction/set.h"
#include "ir/interpretter/architecture.h"
#include "ir/interpretter/execute.h"
#include "ir/jump.h"
#include "ir/value/generic_fn.h"
#include "type/function.h"
#include "type/generic_function.h"
#include "type/generic_struct.h"
#include "type/util.h"

namespace interpretter {
// TODO remove duplication.
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
    ir::RegisterInstruction<uint8_t>, ir::RegisterInstruction<int8_t>,
    ir::RegisterInstruction<uint16_t>, ir::RegisterInstruction<int16_t>,
    ir::RegisterInstruction<uint32_t>, ir::RegisterInstruction<int32_t>,
    ir::RegisterInstruction<uint64_t>, ir::RegisterInstruction<int64_t>,
    ir::RegisterInstruction<float>, ir::RegisterInstruction<double>,
    ir::RegisterInstruction<type::Type const *>,
    ir::RegisterInstruction<ir::Addr>, ir::RegisterInstruction<ir::EnumVal>,
    ir::RegisterInstruction<ir::FlagsVal>, ir::RegisterInstruction<bool>,
    ir::StoreInstruction<uint8_t>, ir::StoreInstruction<int8_t>,
    ir::StoreInstruction<uint16_t>, ir::StoreInstruction<int16_t>,
    ir::StoreInstruction<uint32_t>, ir::StoreInstruction<int32_t>,
    ir::StoreInstruction<uint64_t>, ir::StoreInstruction<int64_t>,
    ir::StoreInstruction<float>, ir::StoreInstruction<double>,
    ir::StoreInstruction<type::Type const *>, ir::StoreInstruction<ir::Addr>,
    ir::StoreInstruction<ir::EnumVal>, ir::StoreInstruction<ir::FlagsVal>,
    ir::StoreInstruction<bool>, ir::StoreInstruction<ir::String>,
    ir::StoreInstruction<ir::Fn>, ir::PhiInstruction<uint8_t>,
    ir::PhiInstruction<int8_t>, ir::PhiInstruction<uint16_t>,
    ir::PhiInstruction<int16_t>, ir::PhiInstruction<uint32_t>,
    ir::PhiInstruction<int32_t>, ir::PhiInstruction<uint64_t>,
    ir::PhiInstruction<int64_t>, ir::PhiInstruction<float>,
    ir::PhiInstruction<double>, ir::PhiInstruction<type::Type const *>,
    ir::PhiInstruction<ir::Addr>, ir::PhiInstruction<ir::EnumVal>,
    ir::PhiInstruction<ir::FlagsVal>, ir::PhiInstruction<bool>,
    ir::PhiInstruction<ir::String>, ir::SetReturnInstruction<uint8_t>,
    ir::SetReturnInstruction<int8_t>, ir::SetReturnInstruction<uint16_t>,
    ir::SetReturnInstruction<int16_t>, ir::SetReturnInstruction<uint32_t>,
    ir::SetReturnInstruction<int32_t>, ir::SetReturnInstruction<uint64_t>,
    ir::SetReturnInstruction<int64_t>, ir::SetReturnInstruction<float>,
    ir::SetReturnInstruction<double>,
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

constexpr int kMaxSize = 8;

void Execute(ir::Fn fn, base::untyped_buffer arguments,
             absl::Span<ir::Addr const> ret_slots) {
  ExecutionContext exec_ctx;
  Execute<instruction_set_t>(fn, std::move(arguments), ret_slots, &exec_ctx);
}

// TODO: Return potential errors.
void Execute(ir::CompiledFn &&fn) {
  ASSERT(fn.type()->output().size() == 0u);
  ExecutionContext exec_context;
  // TODO actually just have a good way to construct the buffer
  DEBUG_LOG("Execute")(fn);
  Execute<instruction_set_t>(
      &fn,
      base::untyped_buffer::MakeFull(
          (fn.type()->params().size() + fn.num_regs()) * kMaxSize),
      {}, &exec_context);
}

base::untyped_buffer EvaluateToBuffer(ir::CompiledFn &&fn) {
  ASSERT(fn.type()->output().size() != 0u);
  size_t bytes_needed = fn.type()->output()[0]->bytes(kArchitecture).value();
  auto ret_buf        = base::untyped_buffer::MakeFull(bytes_needed);
  std::vector<ir::Addr> ret_slots;

  ret_slots.push_back(ir::Addr::Heap(ret_buf.raw(0)));
  ExecutionContext exec_context;
  // TODO actually just have a good way to construct the buffer
  DEBUG_LOG("EvaluateToBuffer")(fn);
  Execute<instruction_set_t>(
      &fn,
      base::untyped_buffer::MakeFull(
          (fn.type()->params().size() + fn.num_regs()) * kMaxSize),
      ret_slots, &exec_context);
  return ret_buf;
}

// TODO why an r-value reference?
base::expected<ir::Value, EvaluationFailure> Evaluate(ir::CompiledFn &&fn) {
  auto buf = EvaluateToBuffer(std::move(fn));
  std::vector<ir::Value> values;
  values.reserve(fn.type()->output().size());

  auto iter = buf.begin();
  for (auto *t : fn.type()->output()) {
    if (t->is<type::GenericStruct>()) {
      values.push_back(ir::Value(t));
    } else {
      type::ApplyTypes<bool, int8_t, int16_t, int32_t, int64_t, uint8_t,
                       uint16_t, uint32_t, uint64_t, float, double,
                       type::Type const *, ir::EnumVal, ir::FlagsVal, ir::Addr,
                       ir::String, module::BasicModule *, ir::ScopeDef *,
                       ir::Fn, ir::Jump *, ir::BlockDef *, ir::GenericFn>(
          t, [&](auto tag) {
            using T = typename decltype(tag)::type;
            T val   = iter.read<T>();
            values.push_back(ir::Value(val));
          });
    }
  }

  switch (values.size()) {
    case 0: return ir::Value();
    case 1: return values[0];
    default: NOT_YET();
  }
}

}  // namespace interpretter

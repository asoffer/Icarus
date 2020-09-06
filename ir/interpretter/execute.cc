#include "ir/interpretter/execute.h"

#include <string_view>
#include <vector>

#include "ir/block_def.h"
#include "ir/instruction/set.h"
#include "ir/interpretter/foreign.h"
#include "ir/jump.h"
#include "ir/read_only_data.h"
#include "ir/scope_def.h"
#include "ir/value/fn.h"
#include "ir/value/foreign_fn.h"
#include "ir/value/native_fn.h"
#include "type/opaque.h"
#include "type/primitive.h"

namespace interpretter {

// TODO rename the `arguments` parameter. It actually should be arguments and
// space for registers.
void Execute(ir::Fn fn, base::untyped_buffer arguments,
             absl::Span<ir::Addr const> ret_slots, ExecutionContext *ctx) {
  switch (fn.kind()) {
    case ir::Fn::Kind::Native: {
      StackFrame frame(fn.native(), std::move(arguments), &ctx->stack_);
      CallFn(fn.native(), &frame, ret_slots, ctx);
    } break;
    case ir::Fn::Kind::Builtin: {
      CallFn(fn.builtin(), arguments, ret_slots, ctx);
    } break;
    case ir::Fn::Kind::Foreign: {
      CallFn(fn.foreign(), arguments, ret_slots, &ctx->stack_);
    } break;
  }
}

void ExecutionContext::MemCpyRegisterBytes(void *dst, ir::Reg reg,
                                           size_t length) {
  std::memcpy(dst, current_frame()->regs_.raw(reg), length);
}

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

void ExecutionContext::ExecuteBlocks(absl::Span<ir::Addr const> ret_slots) {
  DEBUG_LOG()(*current_frame()->current_block());
  DEBUG_LOG()(*current_frame()->fn_.get());
  auto &buffer = current_frame()->fn_->byte_code();

  DEBUG_LOG()(buffer.to_string());

  auto iter = buffer.begin();
  while (true) {
    ASSERT(iter < buffer.end());
    ir::cmd_index_t cmd_index = iter.read<ir::cmd_index_t>();
    DEBUG_LOG()(cmd_index);

    switch (cmd_index) {
      case ir::internal::kReturnInstruction: return;
      case ir::internal::kUncondJumpInstruction: {
        uintptr_t offset = iter.read<uintptr_t>();
        current_frame()->MoveTo(offset);
        iter = current_frame()->fn_->byte_code().begin();
        iter.skip(offset);
      } break;
      case ir::internal::kCondJumpInstruction: {
        ir::Reg r             = iter.read<ir::Reg>();
        uintptr_t true_block  = iter.read<uintptr_t>();
        uintptr_t false_block = iter.read<uintptr_t>();
        uintptr_t offset      = resolve<bool>(r) ? true_block : false_block;
        current_frame()->MoveTo(offset);
        iter = current_frame()->fn_->byte_code().begin();
        iter.skip(offset);
      } break;
      case ir::LoadInstruction::kIndex: {
        uint16_t num_bytes = iter.read<uint16_t>();
        bool is_reg        = iter.read<bool>();
        ir::Addr addr      = ReadAndResolve<ir::Addr>(is_reg, &iter);
        auto result_reg    = iter.read<ir::Reg>().get();
        DEBUG_LOG("load-instruction")(num_bytes, " ", addr, " ", result_reg);
        switch (addr.kind()) {
          case ir::Addr::Kind::Stack: {
            current_frame()->regs_.set_raw(result_reg, stack_.raw(addr.stack()),
                                           num_bytes);
          } break;
          case ir::Addr::Kind::ReadOnly: {
            auto handle = ir::ReadOnlyData.lock();
            current_frame()->regs_.set_raw(
                result_reg, handle->raw(addr.rodata()), num_bytes);
          } break;
          case ir::Addr::Kind::Heap: {
            current_frame()->regs_.set_raw(result_reg, addr.heap(), num_bytes);
          } break;
        }
      } break;

      default: instruction_set_t::Execute[cmd_index](&iter, this, ret_slots);
    }
  }
}

}  // namespace interpretter

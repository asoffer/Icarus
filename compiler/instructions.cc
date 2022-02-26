#include "compiler/instructions.h"

#include "compiler/builtin_module.h"
#include "core/call.h"
#include "ir/instruction/arithmetic.h"
#include "ir/instruction/compare.h"
#include "ir/instruction/core.h"
#include "ir/instruction/instructions.h"
#include "ir/instruction/set.h"
#include "ir/interpreter/evaluate.h"
#include "ir/value/char.h"
#include "type/array.h"
#include "type/enum.h"
#include "type/flags.h"
#include "type/function.h"
#include "type/interface/ir.h"
#include "type/opaque.h"
#include "type/pointer.h"
#include "type/slice.h"
#include "type/struct.h"

namespace compiler {
namespace {

struct InsertBlockInstruction
    : base::Extend<InsertBlockInstruction>::With<base::BaseTraverseExtension,
                                                 base::BaseSerializeExtension,
                                                 ir::DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "insert block %1$s";

  void Apply(interpreter::ExecutionContext& ctx) {
    // TODO: Out(0) may not be sufficient.
    auto& blocks = *ASSERT_NOT_NULL(reinterpret_cast<std::vector<ir::Block>*>(
        ctx.resolve<ir::addr_t>(ir::Reg::Out(0))));
    blocks.push_back(block);
  }

  ir::Block block;
};

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

using TypeConstructorInstructions = ir::InstructionSet<
    type::PtrInstruction, type::BufPtrInstruction, type::OpaqueTypeInstruction,
    type::FunctionTypeInstruction, type::SliceInstruction,
    interface::ConvertsToInstruction, interface::JustInstruction,
    interface::CallableInstruction, type::StructDataInstruction,
    type::StructInstruction, type::AllocateStructInstruction,
    type::EnumInstruction, type::FlagsInstruction>;

struct instruction_set_t
    : ir::InstructionSet<
          ir::CoreInstructions<bool, ir::Char, uint8_t, int8_t, uint16_t,
                               int16_t, uint32_t, int32_t, uint64_t, int64_t,
                               float, double, type::Type, ir::addr_t, ir::Fn,
                               ir::Scope, ir::ModuleId, ir::UnboundScope,
                               interface::Interface>,
          ir::SetReturnInstruction<ir::GenericFn>,
          ir::SetReturnInstruction<ir::ScopeContext>,
          ir::SetReturnInstruction<ir::Block>,
          ArithmeticInstructions<ir::Integer, uint8_t, int8_t, uint16_t,
                                 int16_t, uint32_t, int32_t, uint64_t, int64_t,
                                 float, double>,
          ir::PtrDiffInstruction, ir::ModInstruction<ir::Integer>,
          ir::ModInstruction<uint8_t>, ir::ModInstruction<int8_t>,
          ir::ModInstruction<uint16_t>, ir::ModInstruction<int16_t>,
          ir::ModInstruction<uint32_t>, ir::ModInstruction<int32_t>,
          ir::ModInstruction<uint64_t>, ir::ModInstruction<int64_t>,
          EqualityComparisonInstructions</*ir::Integer,*/ bool, uint8_t, int8_t,
                                         uint16_t, int16_t, uint32_t, int32_t,
                                         uint64_t, int64_t, float, double,
                                         type::Type, ir::addr_t>,
          OrderedComparisonInstructions<
              /*ir::Integer,*/ ir::Char, uint8_t, int8_t, uint16_t, int16_t,
              uint32_t, int32_t, uint64_t, int64_t, float, double, ir::addr_t>,
          ir::NegInstruction<ir::Integer>, ir::NegInstruction<int8_t>,
          ir::NegInstruction<int16_t>, ir::NegInstruction<int32_t>,
          ir::NegInstruction<int64_t>, ir::NegInstruction<float>,
          ir::NegInstruction<double>,
          CastInstructions<
              // ir::Char from bytes (signed or unsigned)
              ir::Char(uint8_t), ir::Char(int8_t),
              // u8 from any numeric primitive
              uint8_t(ir::Integer), uint8_t(ir::Char), uint8_t(int8_t),
              uint8_t(uint16_t), uint8_t(int16_t), uint8_t(uint32_t),
              uint8_t(int32_t), uint8_t(uint64_t), uint8_t(int64_t),
              uint8_t(float), uint8_t(double), uint8_t(bool),
              // i8 from any numeric primitive
              int8_t(ir::Integer), int8_t(ir::Char), int8_t(uint8_t),
              int8_t(uint16_t), int8_t(int16_t), int8_t(uint32_t),
              int8_t(int32_t), int8_t(uint64_t), int8_t(int64_t), int8_t(float),
              int8_t(double), int8_t(bool),
              // u16 from any numeric primitive
              uint16_t(ir::Integer), uint16_t(ir::Char), uint16_t(uint8_t),
              uint16_t(int8_t), uint16_t(int16_t), uint16_t(uint32_t),
              uint16_t(int32_t), uint16_t(uint64_t), uint16_t(int64_t),
              uint16_t(float), uint16_t(double), uint16_t(bool),
              // i16 from any numeric primitive
              int16_t(ir::Integer), int16_t(ir::Char), int16_t(uint8_t),
              int16_t(int8_t), int16_t(uint16_t), int16_t(uint32_t),
              int16_t(int32_t), int16_t(uint64_t), int16_t(int64_t),
              int16_t(float), int16_t(double), int16_t(bool),
              // u32 from any numeric primitive
              uint32_t(ir::Integer), uint32_t(ir::Char), uint32_t(uint8_t),
              uint32_t(int8_t), uint32_t(uint16_t), uint32_t(int16_t),
              uint32_t(int32_t), uint32_t(uint64_t), uint32_t(int64_t),
              uint32_t(float), uint32_t(double), uint32_t(bool),
              // i32 from any numeric primitive
              int32_t(ir::Integer), int32_t(ir::Char), int32_t(uint8_t),
              int32_t(int8_t), int32_t(uint16_t), int32_t(int16_t),
              int32_t(uint32_t), int32_t(uint64_t), int32_t(int64_t),
              int32_t(float), int32_t(double), int32_t(bool),
              // u64 from any numeric primitive
              uint64_t(ir::Integer), uint64_t(ir::Char), uint64_t(uint8_t),
              uint64_t(int8_t), uint64_t(uint16_t), uint64_t(int16_t),
              uint64_t(uint32_t), uint64_t(int32_t), uint64_t(int64_t),
              uint64_t(float), uint64_t(double), uint64_t(bool),
              // i64 from any numeric primitive
              int64_t(ir::Integer), int64_t(ir::Char), int64_t(uint8_t),
              int64_t(int8_t), int64_t(uint16_t), int64_t(int16_t),
              int64_t(uint32_t), int64_t(int32_t), int64_t(uint64_t),
              int64_t(float), int64_t(double), int64_t(bool),
              // f32 from regular numeric primitives
              float(uint8_t), float(int8_t), float(uint16_t), float(int16_t),
              float(uint32_t), float(int32_t), float(uint64_t), float(int64_t),
              double(ir::Integer), float(double),
              // f64 from regular numeric primitives
              double(uint8_t), double(int8_t), double(uint16_t),
              double(int16_t), double(uint32_t), double(int32_t),
              double(uint64_t), double(int64_t), double(ir::Integer),
              double(float),
              // integer from regular numeric primitives
              ir::Integer(uint8_t), ir::Integer(int8_t), ir::Integer(uint16_t),
              ir::Integer(int16_t), ir::Integer(uint32_t), ir::Integer(int32_t),
              ir::Integer(uint64_t), ir::Integer(int64_t)>,
          ir::AndInstruction, ir::NotInstruction, type::XorFlagsInstruction,
          type::AndFlagsInstruction, type::OrFlagsInstruction,
          ir::LoadDataSymbolInstruction, type::ArrayInstruction,
          ir::StructIndexInstruction, ir::PtrIncrInstruction,
          ir::InitInstruction, ir::DestroyInstruction, ir::MoveInitInstruction,
          ir::CopyInitInstruction, ir::MoveInstruction, ir::CopyInstruction,
          type::SliceLengthInstruction, type::SliceDataInstruction,
          ir::DebugIrInstruction, BuiltinInstructions,
          TypeConstructorInstructions, InsertBlockInstruction,
          ir::CompileTime<ir::Action::CopyInit, ir::Integer>,
          ir::CompileTime<ir::Action::MoveInit, ir::Integer>,
          ir::CompileTime<ir::Action::CopyAssign, ir::Integer>,
          ir::CompileTime<ir::Action::MoveAssign, ir::Integer>> {};

void EmitByteCode(ir::ByteCodeWriter& writer, ir::BasicBlock const& block) {
  writer.set_block(&block);

  for (auto const& inst : block.instructions()) {
    if (not inst) { continue; }
    base::Serialize(writer, instruction_set_t::Index(inst), inst);
  }

  block.jump().Visit([&](auto& j) {
    constexpr auto type = base::meta<std::decay_t<decltype(j)>>;
    if constexpr (type == base::meta<ir::JumpCmd::RetJump>) {
      base::Serialize(writer, ir::internal::kReturnInstruction);
    } else if constexpr (type == base::meta<ir::JumpCmd::UncondJump>) {
      base::Serialize(writer, ir::internal::kUncondJumpInstruction, j.block);
    } else if constexpr (type == base::meta<ir::JumpCmd::CondJump>) {
      base::Serialize(writer, ir::internal::kCondJumpInstruction, j.reg,
                      j.true_block, j.false_block);
    } else if constexpr (type == base::meta<ir::JumpCmd::BlockJump>) {
      // TODO: Block arguments
      base::Serialize(writer,
                      instruction_set_t::Index<InsertBlockInstruction>(),
                      InsertBlockInstruction{.block = j.block},
                      ir::internal::kUncondJumpInstruction, j.after);
    } else if constexpr (type == base::meta<ir::JumpCmd::UnreachableJump>) {
      // We very well may have built up a representation in IR that has
      // unreachable blocks. That's okay and we can simply ignore them when
      // emitting IR to byte-code. It's only when executing an unreachable jump
      // that we know a problem occurred.
    } else {
      static_assert(base::always_false(type));
    }
  });
}

}  // namespace

ir::ByteCode EmitByteCode(ir::Subroutine const& sr) {
  ir::ByteCode byte_code;
  ir::ByteCodeWriter writer(&byte_code);
  for (auto const& block : sr.blocks()) { EmitByteCode(writer, *block); }
  std::move(writer).Finalize();
  return byte_code;
}

void InterpretAtCompileTime(ir::Subroutine const& fn,
                            ir::CompleteResultBuffer const& arguments) {
  ir::ByteCode byte_code = EmitByteCode(fn);
  ir::NativeFn::Data data{
      .fn        = &const_cast<ir::Subroutine&>(fn),
      .type      = &fn.type()->as<type::Function>(),
      .byte_code = byte_code.begin(),
  };
  InterpretAtCompileTime(ir::NativeFn(&data), arguments);
}

void InterpretAtCompileTime(ir::NativeFn f,
                            ir::CompleteResultBuffer const& arguments) {
  interpreter::Execute<instruction_set_t>(f, arguments);
}

std::vector<ir::Block> InterpretScopeAtCompileTime(
    ir::Scope s,
    core::Arguments<type::Typed<ir::CompleteResultRef>> const& arguments) {
  interpreter::ExecutionContext ctx;
  interpreter::StackFrame frame(s, ctx.stack());
  core::BindArguments(
      s.type()->params(), arguments,
      [&, i = 0](type::QualType param,
                 type::Typed<ir::CompleteResultRef> argument) mutable {
        absl::Cleanup c  = [&] { ++i; };
        core::Bytes size = param.type().is_big()
                               ? interpreter::kArchitecture.pointer().bytes()
                               : param.type().bytes(interpreter::kArchitecture);
        frame.set_raw(ir::Reg::Arg(i), argument->raw().data(), size.value());
      });

  std::vector<ir::Block> result;
  auto* result_ptr = &result;
  frame.set(ir::Reg::Out(0), reinterpret_cast<ir::addr_t>(result_ptr));
  ctx.Execute<instruction_set_t>(s, frame);
  return result;
}

namespace internal_instructions {


ir::CompleteResultBuffer EvaluateAtCompileTimeToBufferImpl(
    ir::NativeFn fn, ir::CompleteResultBuffer const & arguments) {
  return interpreter::Evaluate<instruction_set_t>(fn, arguments);
}

}  // namespace internal_instructions

}  // namespace compiler

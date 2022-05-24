#include "builtin_module.h"

#include "compiler/instructions.h"
#include "ir/instruction/core.h"
#include "ir/subroutine.h"
#include "ir/value/result_buffer.h"
#include "ir/value/slice.h"
#include "type/opaque.h"

namespace compiler {
namespace {

struct AbortInstruction
    : base::Extend<AbortInstruction>::With<base::BaseTraverseExtension,
                                           base::BaseSerializeExtension,
                                           ir::DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "abort";

  friend bool InterpretInstruction(ir::interpreter::Interpreter& interpreter,
                                   AbortInstruction const&) {
    interpreter.FatalError("`builtin.abort` invoked.");
    return false;
  }
};

struct AlignmentInstruction
    : base::Extend<AlignmentInstruction>::With<base::BaseTraverseExtension,
                                               base::BaseSerializeExtension,
                                               ir::DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "%2$s = alignment %1$s";

  friend bool InterpretInstruction(ir::interpreter::Interpreter& interpreter,
                                   AlignmentInstruction const& inst) {
    interpreter.frame().set(
        inst.result,
        interpreter.frame().resolve(inst.type).alignment(core::Host).value());
    return true;
  }

  ir::RegOr<type::Type> type;
  ir::Reg result;
};

struct AsciiEncodeInstruction
    : base::Extend<AsciiEncodeInstruction>::With<base::BaseTraverseExtension,
                                                 base::BaseSerializeExtension,
                                                 ir::DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "%2$s = ascii-encode %1$s";

  friend bool InterpretInstruction(ir::interpreter::Interpreter& interpreter,
                                   AsciiEncodeInstruction const& inst) {
    interpreter.frame().set(inst.result,
                            ir::Char(interpreter.frame().resolve(inst.value)));
    return true;
  }

  ir::RegOr<uint8_t> value;
  ir::Reg result;
};

struct AsciiDecodeInstruction
    : base::Extend<AsciiDecodeInstruction>::With<base::BaseTraverseExtension,
                                                 base::BaseSerializeExtension,
                                                 ir::DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "%2$s = ascii-decode %1$s";

  friend bool InterpretInstruction(ir::interpreter::Interpreter& interpreter,
                                   AsciiDecodeInstruction const& inst) {
    interpreter.frame().set(
        inst.result,
        static_cast<uint8_t>(interpreter.frame().resolve(inst.character)));
    return true;
  }

  ir::RegOr<ir::Char> character;
  ir::Reg result;
};

struct BytesInstruction
    : base::Extend<BytesInstruction>::With<base::BaseTraverseExtension,
                                           base::BaseSerializeExtension,
                                           ir::DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "%2$s = bytes %1$s";

  friend bool InterpretInstruction(ir::interpreter::Interpreter& interpreter,
                                   BytesInstruction const& inst) {
    interpreter.frame().set(
        inst.result,
        interpreter.frame().resolve(inst.type).bytes(core::Host).value());
    return true;
  }

  ir::RegOr<type::Type> type;
  ir::Reg result;
};

struct HasBlockInstruction
    : base::Extend<HasBlockInstruction>::With<base::BaseTraverseExtension,
                                              base::BaseSerializeExtension,
                                              ir::DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "%3$s = has_block %1$s %2$s";

  friend bool InterpretInstruction(ir::interpreter::Interpreter& interpreter,
                                   HasBlockInstruction const& inst) {
    interpreter.frame().set(
        inst.result, interpreter.frame()
                             .resolve(inst.context)
                             .find(interpreter.frame().resolve(inst.name)) !=
                         ir::Block::Invalid());
    return true;
  }

  ir::RegOr<ir::ScopeContext> context;
  ir::RegOr<ir::Slice> name;
  ir::Reg result;
};

void AbortFn(ir::Subroutine& subroutine) {
  subroutine.entry()->Append(AbortInstruction{});
  subroutine.entry()->set_jump(ir::JumpCmd::Return());
}

void AlignmentFn(ir::Subroutine& subroutine) {
  auto* entry = subroutine.entry();
  auto value  = entry->Append(AlignmentInstruction{
      .type   = ir::Reg::Parameter(0),
      .result = subroutine.Reserve(),
  });
  entry->Append(ir::StoreInstruction<uint64_t>{
      .value    = value,
      .location = ir::Reg::Output(0),
  });
  entry->set_jump(ir::JumpCmd::Return());
}

void AsciiEncode(ir::Subroutine& subroutine) {
  auto* entry = subroutine.entry();
  auto value  = entry->Append(AsciiEncodeInstruction{
      .value  = ir::Reg::Parameter(0),
      .result = subroutine.Reserve(),
  });
  entry->Append(ir::StoreInstruction<ir::Char>{
      .value    = value,
      .location = ir::Reg::Output(0),
  });
  entry->set_jump(ir::JumpCmd::Return());
}

void AsciiDecode(ir::Subroutine& subroutine) {
  auto* entry = subroutine.entry();
  auto value  = entry->Append(AsciiDecodeInstruction{
      .character = ir::Reg::Parameter(0),
      .result    = subroutine.Reserve(),
  });
  entry->Append(ir::StoreInstruction<uint8_t>{
      .value    = value,
      .location = ir::Reg::Output(0),
  });
  entry->set_jump(ir::JumpCmd::Return());
}

void BytesFn(ir::Subroutine& subroutine) {
  auto* entry = subroutine.entry();
  auto value  = entry->Append(BytesInstruction{
      .type   = ir::Reg::Parameter(0),
      .result = subroutine.Reserve(),
  });
  entry->Append(ir::StoreInstruction<uint64_t>{
      .value    = value,
      .location = ir::Reg::Output(0),
  });
  entry->set_jump(ir::JumpCmd::Return());
}

void OpaqueFn(ir::Subroutine& subroutine) {
  auto* entry = subroutine.entry();
  auto value  = entry->Append(type::OpaqueTypeInstruction{
      .mod    = ir::Reg::Parameter(0),
      .result = subroutine.Reserve(),
  });
  entry->Append(ir::StoreInstruction<type::Type>{
      .value    = value,
      .location = ir::Reg::Output(0),
  });
  entry->set_jump(ir::JumpCmd::Return());
}

void ReserveMemoryFn(ir::Subroutine& subroutine) {
  // TODO: Not yet implemented.

  // out.append(c.current().subroutine->Alloca(core::TypeContour(
  //     core::Bytes(*c.EvaluateOrDiagnoseAs<uint64_t>(&args[0].expr())),
  //     core::Alignment(
  //         *c.EvaluateOrDiagnoseAs<uint64_t>(&args[1].expr())))));
  subroutine.entry()->set_jump(ir::JumpCmd::Return());
}

void HasBlockFn(ir::Subroutine& subroutine) {
  auto* entry = subroutine.entry();
  auto value  = entry->Append(HasBlockInstruction{
      .context = ir::Reg::Parameter(0),
      .name    = ir::Reg::Parameter(1),
      .result  = subroutine.Reserve(),
  });
  entry->Append(ir::StoreInstruction<bool>{
      .value    = value,
      .location = ir::Reg::Output(0),
  });
  entry->set_jump(ir::JumpCmd::Return());
}

template <auto F>
void InsertSymbolFor(module::BuiltinModule& module, std::string_view name,
                     type::Function const* fn_type) {
  ir::Fn f = module.insert_function(fn_type, F);

  ir::CompleteResultBuffer buffer;
  buffer.append(f);
  module.insert(name, module::Module::SymbolInformation{
                          .qualified_type = type::QualType::Constant(fn_type),
                          .value          = buffer,
                          .visibility = module::Module::Visibility::Exported,
                      });
}

}  // namespace

std::unique_ptr<module::BuiltinModule> MakeBuiltinModule() {
  auto module = std::make_unique<module::BuiltinModule>();

  InsertSymbolFor<AbortFn>(*module, "abort", type::Func({}, {}));
  InsertSymbolFor<AlignmentFn>(
      *module, "alignment",
      type::Func(
          {core::AnonymousParameter(type::QualType::NonConstant(type::Type_))},
          {type::U64}));
  InsertSymbolFor<AsciiEncode>(
      *module, "ascii_encode",
      type::Func(
          {core::AnonymousParameter(type::QualType::NonConstant(type::U8))},
          {type::Char}));
  InsertSymbolFor<AsciiDecode>(
      *module, "ascii_decode",
      type::Func(
          {core::AnonymousParameter(type::QualType::NonConstant(type::Char))},
          {type::U8}));
  InsertSymbolFor<BytesFn>(
      *module, "bytes",
      type::Func(
          {core::AnonymousParameter(type::QualType::NonConstant(type::Type_))},
          {type::U64}));
  InsertSymbolFor<OpaqueFn>(
      *module, "opaque",
      type::EagerFunc(
          {core::Parameter<type::QualType>{
              .name  = "",
              .value = type::QualType::NonConstant(type::CallingModule),
              .flags = core::ParameterFlags::MustNotName() |
                       core::ParameterFlags::HasDefault()}},
          {type::Type_}));
  InsertSymbolFor<ReserveMemoryFn>(
      *module, "reserve_memory",
      type::Func(
          {core::AnonymousParameter(type::QualType::Constant(type::Integer)),
           core::AnonymousParameter(type::QualType::Constant(type::Integer))},
          {type::BufPtr(type::Byte)}));
  InsertSymbolFor<HasBlockFn>(
      *module, "has_block",
      type::EagerFunc({core::AnonymousParameter(
                           type::QualType::Constant(type::ScopeContext)),
                       core::AnonymousParameter(
                           type::QualType::Constant(type::Slc(type::Char)))},
                      {type::Bool}));

  return module;
}

// TODO: Handle slice, foreign, compilation_error, reserve_memory, debug_ir

}  // namespace compiler

#include "builtin_module.h"

#include "compiler/instructions.h"
#include "ir/subroutine.h"
#include "ir/value/result_buffer.h"
#include "ir/value/slice.h"
#include "type/opaque.h"

namespace compiler {
namespace {

void AbortFn(ir::Subroutine& subroutine) {
  subroutine.entry()->Append(AbortInstruction{});
  subroutine.entry()->set_jump(ir::JumpCmd::Return());
}

void AlignmentFn(ir::Subroutine& subroutine) {
  subroutine.entry()->Append(ir::SetReturnInstruction<uint64_t>{
      .index = 0,
      .value = subroutine.entry()->Append(AlignmentInstruction{
          .type   = ir::Reg::Parameter(0),
          .result = subroutine.Reserve(),
      }),
  });
  subroutine.entry()->set_jump(ir::JumpCmd::Return());
}

void BytesFn(ir::Subroutine& subroutine) {
  subroutine.entry()->Append(ir::SetReturnInstruction<uint64_t>{
      .index = 0,
      .value = subroutine.entry()->Append(BytesInstruction{
          .type   = ir::Reg::Parameter(0),
          .result = subroutine.Reserve(),
      }),
  });
  subroutine.entry()->set_jump(ir::JumpCmd::Return());
}

void OpaqueFn(ir::Subroutine& subroutine) {
  subroutine.entry()->Append(ir::SetReturnInstruction<type::Type>{
      .index = 0,
      .value = subroutine.entry()->Append(type::OpaqueTypeInstruction{
          .mod    = ir::Reg::Parameter(0),
          .result = subroutine.Reserve(),
      }),
  });
  subroutine.entry()->set_jump(ir::JumpCmd::Return());
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
  subroutine.entry()->Append(ir::SetReturnInstruction<bool>{
      .index = 0,
      .value = subroutine.entry()->Append(HasBlockInstruction{
          .context = ir::Reg::Parameter(0),
          .name    = ir::Reg::Parameter(1),
          .result  = subroutine.Reserve(),
      }),
  });
  subroutine.entry()->set_jump(ir::JumpCmd::Return());
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
  auto module = std::make_unique<module::BuiltinModule>(EmitByteCode);

  InsertSymbolFor<AbortFn>(*module, "abort", type::Func({}, {}));
  InsertSymbolFor<AlignmentFn>(
      *module, "alignment",
      type::Func(
          {core::AnonymousParameter(type::QualType::NonConstant(type::Type_))},
          {type::U64}));
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

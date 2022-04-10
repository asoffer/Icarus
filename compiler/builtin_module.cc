#include "builtin_module.h"

#include "compiler/instructions.h"
#include "ir/subroutine.h"
#include "ir/value/result_buffer.h"
#include "ir/value/slice.h"
#include "type/opaque.h"

namespace compiler {
namespace {

ir::Subroutine AbortFn() {
  ir::Subroutine subroutine(type::Func({}, {}));
  subroutine.entry()->Append(AbortInstruction{});
  subroutine.entry()->set_jump(ir::JumpCmd::Return());
  return subroutine;
}

ir::Subroutine AlignmentFn() {
  ir::Subroutine subroutine(type::Func(
      {core::AnonymousParameter(type::QualType::NonConstant(type::Type_))},
      {type::U64}));
  subroutine.entry()->Append(ir::SetReturnInstruction<uint64_t>{
      .index = 0,
      .value = subroutine.entry()->Append(AlignmentInstruction{
          .type   = ir::Reg::Parameter(0),
          .result = subroutine.Reserve(),
      }),
  });
  subroutine.entry()->set_jump(ir::JumpCmd::Return());
  return subroutine;
}

ir::Subroutine BytesFn() {
  ir::Subroutine subroutine(type::Func(
      {core::AnonymousParameter(type::QualType::NonConstant(type::Type_))},
      {type::U64}));
  subroutine.entry()->Append(ir::SetReturnInstruction<uint64_t>{
      .index = 0,
      .value = subroutine.entry()->Append(BytesInstruction{
          .type   = ir::Reg::Parameter(0),
          .result = subroutine.Reserve(),
      }),
  });
  subroutine.entry()->set_jump(ir::JumpCmd::Return());
  return subroutine;
}

ir::Subroutine OpaqueFn() {
  ir::Subroutine subroutine(type::EagerFunc(
      {core::Parameter<type::QualType>{
          .name  = "",
          .value = type::QualType::NonConstant(type::CallingModule),
          .flags = core::ParameterFlags::MustNotName() |
                   core::ParameterFlags::HasDefault()}},
      {type::Type_}));
  subroutine.entry()->Append(ir::SetReturnInstruction<type::Type>{
      .index = 0,
      .value = subroutine.entry()->Append(type::OpaqueTypeInstruction{
          .mod    = ir::Reg::Parameter(0),
          .result = subroutine.Reserve(),
      }),
  });
  subroutine.entry()->set_jump(ir::JumpCmd::Return());
  return subroutine;
}

ir::Subroutine ReserveMemoryFn() {
  ir::Subroutine subroutine(type::Func(
      {core::AnonymousParameter(type::QualType::Constant(type::Integer)),
       core::AnonymousParameter(type::QualType::Constant(type::Integer))},
      {type::BufPtr(type::Byte)}));
  // TODO: Not yet implemented.

  // out.append(c.current().subroutine->Alloca(core::TypeContour(
  //     core::Bytes(*c.EvaluateOrDiagnoseAs<uint64_t>(&args[0].expr())),
  //     core::Alignment(
  //         *c.EvaluateOrDiagnoseAs<uint64_t>(&args[1].expr())))));
  subroutine.entry()->set_jump(ir::JumpCmd::Return());
  return subroutine;
}

ir::Subroutine HasBlockFn() {
  ir::Subroutine subroutine(type::EagerFunc(
      {core::AnonymousParameter(type::QualType::Constant(type::ScopeContext)),
       core::AnonymousParameter(type::QualType::Constant(type::Slc(type::Char)))},
      {type::Bool}));
  subroutine.entry()->Append(ir::SetReturnInstruction<bool>{
      .index = 0,
      .value = subroutine.entry()->Append(HasBlockInstruction{
          .context = ir::Reg::Parameter(0),
          .name    = ir::Reg::Parameter(1),
          .result  = subroutine.Reserve(),
      }),
  });
  subroutine.entry()->set_jump(ir::JumpCmd::Return());
  return subroutine;
}

template <auto F>
void InsertSymbolFor(module::BuiltinModule& module, std::string_view name) {
  auto subroutine = F();
  ir::Fn f(module.insert_function(
      F(), &subroutine.type()->template as<type::Function>(),
      EmitByteCode(subroutine)));

  ir::CompleteResultBuffer buffer;
  buffer.append(f);
  module.insert(
      name, module::Module::SymbolInformation{
                .qualified_type = type::QualType::Constant(f.native().type()),
                .value          = buffer,
                .visibility     = module::Module::Visibility::Exported,
            });
}

}  // namespace

std::unique_ptr<module::BuiltinModule> MakeBuiltinModule() {
  auto module = std::make_unique<module::BuiltinModule>();

  InsertSymbolFor<AbortFn>(*module, "abort");
  InsertSymbolFor<AlignmentFn>(*module, "alignment");
  InsertSymbolFor<BytesFn>(*module, "bytes");
  InsertSymbolFor<OpaqueFn>(*module, "opaque");
  InsertSymbolFor<ReserveMemoryFn>(*module, "reserve_memory");
  InsertSymbolFor<HasBlockFn>(*module, "has_block");

  return module;
}

// TODO: Handle slice, foreign, compilation_error, reserve_memory, debug_ir

}  // namespace compiler

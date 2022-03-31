#include "builtin_module.h"

#include "compiler/instructions.h"
#include "ir/subroutine.h"
#include "ir/value/result_buffer.h"
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
      {core::AnonymousParam(type::QualType::NonConstant(type::Type_))},
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
      {core::AnonymousParam(type::QualType::NonConstant(type::Type_))},
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

template <auto F>
module::Module::SymbolInformation SymbolFor() {
  static ir::Subroutine subroutine = F();
  static ir::ByteCode byte_code    = EmitByteCode(subroutine);
  static ir::NativeFn::Data data{
      .fn        = &subroutine,
      .type      = &subroutine.type()->template as<type::Function>(),
      .byte_code = &byte_code,
  };

  ir::CompleteResultBuffer buffer;
  ir::Fn f = ir::NativeFn(&data);
  buffer.append(f);
  return module::Module::SymbolInformation{
      .qualified_type = type::QualType::Constant(data.type),
      .value          = buffer,
  };
}

ir::Subroutine OpaqueFn() {
  ir::Subroutine subroutine(
      type::Func({core::Param<type::QualType>(
                     "", type::QualType::NonConstant(type::CallingModule),
                     static_cast<core::ParamFlags>(core::MUST_NOT_NAME |
                                                   core::HAS_DEFAULT))},
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

}  // namespace

std::unique_ptr<module::BuiltinModule> MakeBuiltinModule() {
  auto module = std::make_unique<module::BuiltinModule>();

  module->insert("abort", SymbolFor<AbortFn>());
  module->insert("alignment", SymbolFor<AlignmentFn>());
  module->insert("bytes", SymbolFor<BytesFn>());
  module->insert("opaque", SymbolFor<OpaqueFn>());

  return module;
}

}  // namespace compiler

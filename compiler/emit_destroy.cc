#include "absl/random/random.h"
#include "ast/ast.h"
#include "base/permutation.h"
#include "compiler/compiler.h"
#include "compiler/special_function.h"
#include "ir/builder.h"
#include "ir/compiled_fn.h"
#include "type/type.h"
#include "type/typed_value.h"

namespace compiler {

void Compiler::EmitDestroy(type::Typed<ir::Reg, type::Primitive> reg) {}
void Compiler::EmitDestroy(type::Typed<ir::Reg, type::Pointer> reg) {}
void Compiler::EmitDestroy(type::Typed<ir::Reg, type::BufferPointer> reg) {}

void Compiler::EmitDestroy(type::Typed<ir::Reg, type::Struct> reg) {
  if (not reg.type()->HasDestructor()) { return; }
  // TODO: Call fields dtors.
  builder().Destroy(reg.type(), *reg);
}

void Compiler::EmitDestroy(type::Typed<ir::Reg, type::Tuple> reg) {
  if (not reg.type()->HasDestructor()) { return; }
  reg.type()->destroy_func_.init([=]() {
    auto const *fn_type =
        type::Func(core::Params<type::QualType>{core::AnonymousParam(
                       type::QualType::NonConstant(type::Ptr(reg.type())))},
                   {});
    ir::NativeFn fn =
        AddFunc(fn_type, fn_type->params().Transform([](type::QualType q) {
          return type::Typed<ast::Declaration const *>(nullptr, q.type());
        }));
    ICARUS_SCOPE(ir::SetCurrent(fn)) {
      builder().CurrentBlock() = builder().CurrentGroup()->entry();
      auto var                 = ir::Reg::Arg(0);

      for (size_t i :
           base::make_random_permutation(absl::BitGen{}, reg.type()->entries_.size())) {
        EmitDestroy(
            type::Typed<ir::Reg>(builder().Field(var, reg.type(), i).get(),
                                 reg.type()->entries_.at(i)));
      }

      builder().ReturnJump();
    }
    return fn;
  });

  builder().Destroy(reg.type(), *reg);
}

void Compiler::EmitDestroy(type::Typed<ir::Reg, type::Array> reg) {
  if (not reg.type()->HasDestructor()) { return; }
  data().destroy_.emplace(
      reg.type(), base::lazy_convert{[&] {
        auto const *fn_type =
            type::Func(core::Params<type::QualType>{core::AnonymousParam(
                           type::QualType::NonConstant(type::Ptr(reg.type())))},
                       {});
        ir::NativeFn fn =
            AddFunc(fn_type, fn_type->params().Transform([](type::QualType q) {
              return type::Typed<ast::Declaration const *>(nullptr, q.type());
            }));
        ICARUS_SCOPE(ir::SetCurrent(fn)) {
          builder().CurrentBlock() = fn->entry();
          builder().OnEachArrayElement(
              reg.type(), ir::Reg::Arg(0), [=](ir::Reg r) {
                EmitDestroy(type::Typed<ir::Reg>(r, reg.type()->data_type()));
              });
          builder().ReturnJump();
        }
        return fn;
      }});
  builder().Destroy(reg.type(), *reg);
}

}  // namespace compiler

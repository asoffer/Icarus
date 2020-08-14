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

void Compiler::Visit(type::Primitive const *, ir::Reg, EmitDestroyTag) {}
void Compiler::Visit(type::Pointer const *, ir::Reg, EmitDestroyTag) {}

void Compiler::Visit(type::Struct const *t, ir::Reg reg, EmitDestroyTag) {
  if (not t->HasDestructor()) { return; }
  // TODO: Call fields dtors.
  builder().Destroy(t, reg);
}

void Compiler::Visit(type::Tuple const *t, ir::Reg reg, EmitDestroyTag) {
  if (not t->HasDestructor()) { return; }
  t->destroy_func_.init([=]() {
    auto const *fn_type = type::Func(
        core::Params<type::QualType>{
            core::AnonymousParam(type::QualType::NonConstant(type::Ptr(t)))},
        {});
    ir::NativeFn fn =
        AddFunc(fn_type, fn_type->params().Transform([](type::QualType q) {
          return type::Typed<ast::Declaration const *>(nullptr, q.type());
        }));
    ICARUS_SCOPE(ir::SetCurrent(fn)) {
      builder().CurrentBlock() = builder().CurrentGroup()->entry();
      auto var                 = ir::Reg::Arg(0);

      for (size_t i :
           base::make_random_permutation(absl::BitGen{}, t->entries_.size())) {
        Visit(t->entries_.at(i), builder().Field(var, t, i).get(),
              EmitDestroyTag{});
      }

      builder().ReturnJump();
    }
    return fn;
  });

  builder().Destroy(t, reg);
}

void Compiler::Visit(type::Array const *t, ir::Reg reg, EmitDestroyTag) {
  if (not t->HasDestructor()) { return; }
  data().destroy_.emplace(
      t, base::lazy_convert{[&] {
        auto const *fn_type =
            type::Func(core::Params<type::QualType>{core::AnonymousParam(
                           type::QualType::NonConstant(type::Ptr(t)))},
                       {});
        ir::NativeFn fn =
            AddFunc(fn_type, fn_type->params().Transform([](type::QualType q) {
              return type::Typed<ast::Declaration const *>(nullptr, q.type());
            }));
        ICARUS_SCOPE(ir::SetCurrent(fn)) {
          builder().CurrentBlock() = fn->entry();
          builder().OnEachArrayElement(t, ir::Reg::Arg(0), [=](ir::Reg r) {
            Visit(t->data_type(), r, EmitDestroyTag{});
          });
          builder().ReturnJump();
        }
        return fn;
      }});
  builder().Destroy(t, reg);
}

}  // namespace compiler

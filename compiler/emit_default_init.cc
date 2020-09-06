#include "absl/random/random.h"
#include "ast/ast.h"
#include "base/lazy_convert.h"
#include "base/permutation.h"
#include "compiler/compiler.h"
#include "ir/builder.h"
#include "ir/compiled_fn.h"
#include "ir/value/value.h"
#include "type/primitive.h"

namespace compiler {

void Compiler::Visit(type::Array const *t, ir::Reg reg, EmitDefaultInitTag) {
  data().init_.emplace(
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
            Visit(t->data_type(), r, EmitDefaultInitTag{});
          });
          builder().ReturnJump();
        }
        fn->WriteByteCode<interpretter::instruction_set_t>();
        return fn;
      }});

  builder().Init(t, reg);
}

void Compiler::Visit(type::Flags const *t, ir::Reg reg, EmitDefaultInitTag) {
  builder().Store(ir::FlagsVal{0}, reg);
}

void Compiler::Visit(type::Pointer const *t, ir::Reg reg, EmitDefaultInitTag) {
  builder().Store(ir::Addr::Null(), reg);
}

void Compiler::Visit(type::Primitive const *t, ir::Reg reg,
                     EmitDefaultInitTag) {
  t->Apply([&](auto tag) {
    using T = typename decltype(tag)::type;
    builder().Store(T{}, reg);
  });
}

void Compiler::Visit(type::Struct const *t, ir::Reg reg, EmitDefaultInitTag) {
  t->init_func_.init([=]() {
    type::Pointer const *pt = type::Ptr(t);
    auto const *fn_type     = type::Func(
        core::Params<type::QualType>{
            core::AnonymousParam(type::QualType::NonConstant(pt))},
        {});
    ir::NativeFn fn =
        AddFunc(fn_type, fn_type->params().Transform([](type::QualType q) {
          return type::Typed<ast::Declaration const *>(nullptr, q.type());
        }));

    ICARUS_SCOPE(ir::SetCurrent(fn)) {
      builder().CurrentBlock() = builder().CurrentGroup()->entry();
      auto var                 = ir::Reg::Arg(0);

      for (size_t i = 0; i < t->fields_.size(); ++i) {
        auto &field = t->fields_[i];
        if (not field.initial_value.empty()) {
          if (field.type == type::Int64) {
            EmitCopyInit(type::Typed(field.initial_value, field.type),
                         builder().Field(var, t, i));
          } else {
            NOT_YET();
          }
        } else {
          Visit(field.type, builder().Field(var, t, i).get(),
                EmitDefaultInitTag{});
        }
      }

      builder().ReturnJump();
    }
    fn->WriteByteCode<interpretter::instruction_set_t>();
    return fn;
  });

  builder().Init(t, reg);
}

void Compiler::Visit(type::Tuple const *t, ir::Reg reg, EmitDefaultInitTag) {
  t->init_func_.init([=]() {
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
              EmitDefaultInitTag{});
      }

      builder().ReturnJump();
    }
    return fn;
  });
  builder().Init(t, reg);
}

}  // namespace compiler

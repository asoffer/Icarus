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

void Compiler::Visit(type::Struct const *t, ir::Reg reg, EmitDestroyTag) {
  if (not t->HasDestructor()) { return; }
  // TODO: Call fields dtors.
  builder().Destroy(t, reg);
}

void Compiler::Visit(type::Variant const *t, ir::Reg reg, EmitDestroyTag) {
  if (not t->HasDestructor()) { return; }
  // TODO design and build a jump table?
  // TODO remove these casts in favor of something easier to track properties on
  std::unique_lock lock(t->mtx_);
  if (not t->destroy_func_) {
    auto const *fn_type = type::Func(
        core::Params<type::QualType>{
            core::AnonymousParam(type::QualType::NonConstant(t))},
        {});
    ir::NativeFn f =
        AddFunc(fn_type, fn_type->params().Transform([](type::QualType q) {
          return type::Typed<ast::Declaration const *>(nullptr, q.type());
        }));
    t->destroy_func_ = f;
    ICARUS_SCOPE(ir::SetCurrent(f)) {
      builder().CurrentBlock() = f->entry();
      auto *landing            = builder().AddBlock();
      auto type                = builder().Load<type::Type const *>(
          builder().VariantType(ir::Reg::Arg(0)));

      auto var_val = builder().VariantValue(t, ir::Reg::Arg(0));
      for (type::Type const *v : t->variants_) {
        if (not v->HasDestructor()) { continue; }
        auto *old_block   = builder().CurrentBlock();
        auto *found_block = builder().AddBlock();

        builder().CurrentBlock() = found_block;
        Visit(v, builder().PtrFix(var_val, v), EmitDestroyTag{});
        builder().UncondJump(landing);

        builder().CurrentBlock() = old_block;
        builder().CurrentBlock() = builder().EarlyExitOn<true>(
            found_block, builder().Eq(ir::RegOr<type::Type const *>(type), v));
      }

      builder().UncondJump(landing);
      builder().CurrentBlock() = landing;
      builder().ReturnJump();
    }
  }

  builder().Call(ir::Fn{*t->destroy_func_}, t->destroy_func_->type(),
                 {ir::Value(reg)}, ir::OutParams());
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

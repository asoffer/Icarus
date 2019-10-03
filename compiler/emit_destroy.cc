#include "absl/random/random.h"
#include "ast/ast.h"
#include "base/permutation.h"
#include "ir/builder.h"
#include "ir/cmd/call.h"
#include "ir/cmd/misc.h"
#include "ir/compiled_fn.h"
#include "ir/components.h"
#include "type/type.h"
#include "type/typed_value.h"
#include "compiler/compiler.h"
#include "compiler/special_function.h"

namespace compiler {

void Compiler::EmitDestroy(type::Struct const *t, ir::Reg reg) {
  if (!t->HasDestructor()) { return; }
  t->destroy_func_.init([=]() {
    if (auto fn = SpecialFunction(this, t, "~")) { return *fn; }

    type::Pointer const *pt = type::Ptr(t);
    ir::AnyFunc fn =
        AddFunc(type::Func({pt}, {}),
                core::FnParams(core::Param{
                    "", type::Typed<ast::Expression const *>{nullptr, pt}}));

    ICARUS_SCOPE(ir::SetCurrentFunc(fn.func())) {
      builder().CurrentBlock() = builder().function()->entry();
      auto var                 = ir::Reg::Arg(0);

      for (int i = static_cast<int>(t->fields_.size()) - 1; i >= 0; --i) {
        t->fields_.at(i).type->EmitDestroy(this, ir::Field(var, t, i).get());
      }

      ir::ReturnJump();
    }
    return fn;
  });

  ir::Destroy(t, reg);
}

void Compiler::EmitDestroy(type::Variant const *t, ir::Reg reg) {
  if (!t->HasDestructor()) { return; }
  // TODO design and build a jump table?
  // TODO remove these casts in favor of something easier to track properties on
  std::unique_lock lock(t->mtx_);
  if (!t->destroy_func_) {
    t->destroy_func_ = AddFunc(
        type::Func({t}, {}),
        core::FnParams(
            core::Param{"", type::Typed<ast::Expression const *>{nullptr, t}}));
    ICARUS_SCOPE(ir::SetCurrentFunc(t->destroy_func_)) {
      builder().CurrentBlock() = t->destroy_func_->entry();
      auto *landing            = builder().AddBlock();
      auto type =
          ir::Load<type::Type const *>(ir::VariantType(ir::Reg::Arg(0)));

      auto var_val = ir::VariantValue(t, ir::Reg::Arg(0));
      for (type::Type const *v : t->variants_) {
        if (!v->HasDestructor()) { continue; }
        auto *old_block   = builder().CurrentBlock();
        auto *found_block = builder().AddBlock();

        builder().CurrentBlock() = found_block;
        v->EmitDestroy(this, ir::PtrFix(var_val, v));
        ir::UncondJump(landing);

        builder().CurrentBlock() = old_block;
        builder().CurrentBlock() = ir::EarlyExitOn<true>(
            found_block, ir::Eq(ir::RegOr<type::Type const *>(type), v));
      }

      ir::UncondJump(landing);
      builder().CurrentBlock() = landing;
      ir::ReturnJump();
    }
  }

  ir::Call(ir::AnyFunc{t->destroy_func_}, t->destroy_func_->type_,
           {ir::Results{reg}});
}

void Compiler::EmitDestroy(type::Tuple const *t, ir::Reg reg) {
  if (!t->HasDestructor()) { return; }
  t->destroy_func_.init([=]() {
    auto *fn = AddFunc(
        type::Func({Ptr(t)}, {}),
        core::FnParams(core::Param{
            "", type::Typed<ast::Expression const *>{nullptr, type::Ptr(t)}}));
    ICARUS_SCOPE(ir::SetCurrentFunc(fn)) {
      builder().CurrentBlock() = builder().function()->entry();
      auto var                 = ir::Reg::Arg(0);

      for (size_t i :
           base::make_random_permutation(absl::BitGen{}, t->entries_.size())) {
        t->entries_.at(i)->EmitDestroy(this, ir::Field(var, t, i).get());
      }

      ir::ReturnJump();
    }
    return fn;
  });

  ir::Destroy(t, reg);
}

void Compiler::EmitDestroy(type::Array const *t, ir::Reg reg) {
  if (!t->HasDestructor()) { return; }
  t->destroy_func_.init([=]() {
    // TODO special function?
    auto *fn = AddFunc(
        type::Func({type::Ptr(t)}, {}),
        core::FnParams(core::Param{
            "", type::Typed<ast::Expression const *>{nullptr, type::Ptr(t)}}));

    ir::OnEachArrayElement(
        t, fn, [=](ir::Reg r) { t->data_type->EmitDestroy(this, r); });
    return fn;
  });

  ir::Destroy(t, reg);
}

}  // namespace compiler

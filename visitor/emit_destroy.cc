#include "ast/declaration.h"
#include "base/permutation.h"
#include "ir/cmd.h"
#include "ir/compiled_fn.h"
#include "ir/components.h"
#include "misc/context.h"
#include "type/type.h"
#include "type/typed_value.h"
#include "visitor/emit_ir.h"
#include "visitor/special_function.h"

namespace visitor {

void EmitIr::Destroy(type::Struct const *t, ir::Reg reg, Context *ctx) const {
  if (!t->HasDestructor()) { return; }
  t->destroy_func_.init([=]() {
    if (auto fn = SpecialFunction(this, t, "~", ctx)) { return *fn; }

    type::Pointer const *pt = type::Ptr(t);
    ir::AnyFunc fn          = t->mod_->AddFunc(
        type::Func({pt}, {}),
        core::FnParams(core::Param{
            "", type::Typed<ast::Expression const *>{nullptr, pt}}));

    CURRENT_FUNC(fn.func()) {
      ir::BasicBlock::Current = ir::CompiledFn::Current->entry();
      auto var                = ir::Reg::Arg(0);

      for (int i = static_cast<int>(t->fields_.size()) - 1; i >= 0; --i) {
        t->fields_.at(i).type->EmitDestroy(this, ir::Field(var, t, i).get(), ctx);
      }

      ir::ReturnJump();
    }
    return fn;
  });

  ir::Destroy(t, reg);
}

void EmitIr::Destroy(type::Variant const *t, ir::Reg reg, Context *ctx) const {
  if (!t->HasDestructor()) { return; }
  // TODO design and build a jump table?
  // TODO remove these casts in favor of something easier to track properties on
  std::unique_lock lock(t->mtx_);
  if (!t->destroy_func_) {
    t->destroy_func_ = ctx->mod_->AddFunc(
        type::Func({t}, {}),
        core::FnParams(
            core::Param{"", type::Typed<ast::Expression const *>{nullptr, t}}));
    CURRENT_FUNC(t->destroy_func_) {
      ir::BasicBlock::Current = t->destroy_func_->entry();
      auto landing            = ir::CompiledFn::Current->AddBlock();
      auto type =
          ir::Load<type::Type const *>(ir::VariantType(ir::Reg::Arg(0)));

      for (type::Type const *v : t->variants_) {
        if (!v->HasDestructor()) { continue; }
        auto old_block   = ir::BasicBlock::Current;
        auto found_block = ir::CompiledFn::Current->AddBlock();

        ir::BasicBlock::Current = found_block;
        v->EmitDestroy(
            this, ir::PtrFix(ir::VariantValue(v, ir::Reg::Arg(0)), v), ctx);
        ir::UncondJump(landing);

        ir::BasicBlock::Current = old_block;
        ir::BasicBlock::Current = ir::EarlyExitOn<true>(
            found_block, ir::Eq(ir::RegisterOr<type::Type const *>(type), v));
      }

      ir::UncondJump(landing);
      ir::BasicBlock::Current = landing;
      ir::ReturnJump();
    }
  }

  ir::Call(ir::AnyFunc{t->destroy_func_},
           ir::Arguments(t->destroy_func_->type_, ir::Results{reg}));
}

void EmitIr::Destroy(type::Tuple const *t, ir::Reg reg, Context *ctx) const {
  if (!t->HasDestructor()) { return; }
  t->destroy_func_.init([=]() {
    auto *fn = ctx->mod_->AddFunc(
        type::Func({Ptr(t)}, {}),
        core::FnParams(core::Param{
            "", type::Typed<ast::Expression const *>{nullptr, type::Ptr(t)}}));
    CURRENT_FUNC(fn) {
      ir::BasicBlock::Current = ir::CompiledFn::Current->entry();
      auto var                = ir::Reg::Arg(0);

      for (size_t i : base::make_random_permutation(t->entries_.size())) {
        t->entries_.at(i)->EmitDestroy(this, ir::Field(var, t, i).get(), ctx);
      }

      ir::ReturnJump();
    }
    return fn;
  });

  ir::Destroy(t, reg);
}

void EmitIr::Destroy(type::Array const* t, ir::Reg reg, Context *ctx) const {
  if (!t->HasDestructor()) { return; }
  t->destroy_func_.init([=]() {
    // TODO special function?
    auto *fn = ctx->mod_->AddFunc(
        type::Func({type::Ptr(t)}, {}),
        core::FnParams(core::Param{
            "", type::Typed<ast::Expression const *>{nullptr, type::Ptr(t)}}));

    ir::OnEachArrayElement(
        t, fn, [=](ir::Reg r) { t->data_type->EmitDestroy(this, r, ctx); });
    return fn;
  });

  ir::Destroy(t, reg);
}

}  // namespace visitor

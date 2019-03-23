#include "ast/struct_literal.h"

#include <sstream>
#include "ast/declaration.h"
#include "backend/exec.h"
#include "base/untyped_buffer.h"
#include "ir/components.h"
#include "ir/func.h"
#include "ir/val.h"
#include "type/function.h"
#include "type/generic_struct.h"
#include "type/pointer.h"
#include "type/struct.h"
#include "type/tuple.h"

namespace ir {
// TODO: The functions here that modify struct fields typically do so by
// modifying the last field, since we always build them in order. This saves us
// from having to pass extra information and thereby bloating all commands. At
// some point we should switch to a buffer-chunk system so that one won't bloat
// another.
Register CreateStruct(core::Scope const *scope,
                      ast::StructLiteral const *parent);
void CreateStructField(Register struct_type,
                       RegisterOr<type::Type const *> type);
void SetStructFieldName(Register struct_type, std::string_view field_name);
void AddHashtagToField(Register struct_type, ast::Hashtag hashtag);
void AddHashtagToStruct(Register struct_type, ast::Hashtag hashtag);
Register FinalizeStruct(Register r);

Register ArgumentCache(ast::StructLiteral *sl);
}  // namespace ir

namespace ast {
std::string StructLiteral::to_string(size_t n) const {
  std::stringstream ss;
  ss << "struct (";
  for (auto &a : args_) { ss << a->to_string(n) << ", "; }
  ss << ") {\n";
  for (const auto &f : fields_) {
    ss << std::string((n + 1) * 2, ' ') << f->to_string(n) << "\n";
  }
  ss << std::string(2 * n, ' ') << "}";
  return ss.str();
}

void StructLiteral::DependentDecls(DeclDepGraph *g,
                                   Declaration *d) const {
  for (auto &a : args_) { a->DependentDecls(g, d); }
  for (auto &f : fields_) { f->DependentDecls(g, d); }
}

void StructLiteral::assign_scope(core::Scope *scope) {
  scope_     = scope;
  type_scope = scope->add_child<core::DeclScope>();
  for (auto &a : args_) { a->assign_scope(type_scope.get()); }
  for (auto &f : fields_) { f->assign_scope(type_scope.get()); }
}

VerifyResult StructLiteral::VerifyType(Context *ctx) {
  std::vector<type::Type const *> ts;
  ts.reserve(args_.size());
  for (auto &a : args_) { ts.push_back(a->VerifyType(ctx).type_); }
  if (std::any_of(ts.begin(), ts.end(),
                  [](type::Type const *t) { return t == nullptr; })) {
    return VerifyResult::Error();
  }

  if (args_.empty()) {
    for (auto &field : fields_) { field->VerifyType(ctx); }
    return ctx->set_result(this, VerifyResult::Constant(type::Type_));
  } else {
    return ctx->set_result(
        this, VerifyResult::Constant(type::GenStruct(scope_, std::move(ts))));
  }
}

void StructLiteral::ExtractJumps(JumpExprs *rets) const {
  for (auto &a : args_) { a->ExtractJumps(rets); }
  for (auto &f : fields_) { f->ExtractJumps(rets); }
}

static ir::TypedRegister<type::Type const *> GenerateStruct(
    StructLiteral *sl, ir::Register struct_reg, Context *ctx) {
  for (auto const &field : sl->fields_) {
    // TODO initial values? hashatgs?

    // NOTE: CreateStructField may invalidate all other struct fields, so it's
    // not safe to access these registers returned by CreateStructField after
    // a subsequent call to CreateStructField.
    ir::CreateStructField(
        struct_reg, field->type_expr->EmitIr(ctx).get<type::Type const *>(0));
    ir::SetStructFieldName(struct_reg, field->id_);
    for (auto const &hashtag : field->hashtags_) {
      ir::AddHashtagToField(struct_reg, hashtag);
    }
  }

  for (auto hashtag : sl->hashtags_) {
    ir::AddHashtagToStruct(struct_reg, hashtag);
  }
  return ir::FinalizeStruct(struct_reg);
}

ir::Results StructLiteral::EmitIr(Context *ctx) {
  if (args_.empty()) {
    return ir::Results{GenerateStruct(this, ir::CreateStruct(scope_, this), ctx)};
  }

  // TODO A bunch of things need to be fixed here.
  // * Lock access during creation so two requestors don't clobber each other.
  // * Add a way way for one requestor to wait for another to have created the
  // object and be notified.
  //
  // For now, it's safe to do this from within a single module compilation
  // (which is single-threaded).
  ir::Func *&ir_func = mod_->data_[ctx->bound_constants_].ir_funcs_[this];
  if (!ir_func) {
    auto &work_item = ctx->mod_->deferred_work_.emplace(
        [bc{ctx->bound_constants_}, this, mod{ctx->mod_}]() mutable {
          Context ctx(mod);
          ctx.bound_constants_ = std::move(bc);
          CompleteBody(&ctx);
        });

    auto const &arg_types = ctx->type_of(this)->as<type::GenericStruct>().deps_;

    core::FnParams<type::Typed<Expression *>> params;
    params.reserve(args_.size());
    size_t i = 0;
    for (auto const &d : args_) {
      params.append(d->id_, type::Typed(d->init_val.get(), arg_types.at(i++)));
    }

    ir_func =
        mod_->AddFunc(type::Func(arg_types, {type::Type_}), std::move(params));

    ir_func->work_item = &work_item;
  }

  return ir::Results{
      std::get<ir::AnyFunc>(ir::Val::Func(ir_func->type_, ir_func).value)};
}

void StructLiteral::CompleteBody(Context *ctx) {
  ir::Func *&ir_func = mod_->data_[ctx->bound_constants_].ir_funcs_[this];
  for (size_t i = 0; i < args_.size(); ++i) {
    ctx->set_addr(args_[i].get(), ir_func->Argument(i));
  }

  CURRENT_FUNC(ir_func) {
    ir::BasicBlock::Current = ir_func->entry();
    auto cache_slot_addr    = ir::ArgumentCache(this);
    auto cache_slot         = ir::Load<type::Type const *>(cache_slot_addr);

    auto land_block         = ir::Func::Current->AddBlock();
    ir::BasicBlock::Current = ir::EarlyExitOn<false>(
        land_block,
        ir::Eq(cache_slot, static_cast<type::Type const *>(nullptr)));
    auto ctx_reg    = ir::CreateContext(ctx->mod_);
    auto struct_reg = ir::CreateStruct(scope_, this);

    // TODO why isn't implicit TypedRegister -> RegisterOr cast working on
    // either of these? On the first it's clear because we don't even return a
    // typedRegister, but this is a note to remind you to make that work. On the
    // second... I don't know.
    ir::Store(static_cast<ir::RegisterOr<type::Type const *>>(struct_reg),
              cache_slot_addr);
    for (auto const &arg : args_) {
      ir::AddBoundConstant(ctx_reg, arg.get(), ctx->addr(arg.get()));
    }

    for (auto const &field : fields_) {
      ir::VerifyType(field.get(), ctx_reg);

      // TODO exit early if verifytype fails.

      auto type_reg = ir::EvaluateAsType(field->type_expr.get(), ctx_reg);

      ir::CreateStructField(struct_reg, type_reg);
      ir::SetStructFieldName(struct_reg, field->id_);

      for (auto const &hashtag : field->hashtags_) {
        ir::AddHashtagToField(struct_reg, hashtag);
      }
    }

    for (auto hashtag : hashtags_) {
      ir::AddHashtagToStruct(struct_reg, hashtag);
    }

    ir::RegisterOr<type::Type const *> result = ir::FinalizeStruct(struct_reg);
    ir::DestroyContext(ctx_reg);

    // Exit path from creating a new struct.
    ir::SetRet(0, static_cast<ir::RegisterOr<type::Type const *>>(result));
    ir::Store(static_cast<ir::RegisterOr<type::Type const *>>(result),
              cache_slot_addr);
    ir::ReturnJump();

    // Exit path from finding the cache
    ir::BasicBlock::Current = land_block;
    ir::SetRet(0, static_cast<ir::RegisterOr<type::Type const *>>(cache_slot));
    ir::ReturnJump();
  }
}
}  // namespace ast

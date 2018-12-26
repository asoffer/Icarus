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
Register CreateStruct(Module const *mod);
void CreateStructField(Register struct_type,
                       RegisterOr<type::Type const *> type);
void SetStructFieldName(Register struct_type, std::string_view field_name);
void AddHashtagToField(Register struct_type, ast::Hashtag hashtag);
Register FinalizeStruct(Register r);
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

void StructLiteral::assign_scope(Scope *scope) {
  for (auto &a : args_) { a->assign_scope(scope); }
  scope_     = scope;
  type_scope = scope->add_child<DeclScope>();
  for (auto &f : fields_) { f->assign_scope(type_scope.get()); }
}

type::Type const *StructLiteral::VerifyType(Context *ctx) {
  base::vector<type::Type const *> ts;
  ts.reserve(args_.size());
  for (auto &a : args_) { ts.push_back(a->VerifyType(ctx)); }
  if (std::any_of(ts.begin(), ts.end(),
                  [](type::Type const *t) { return t == nullptr; })) {
    return nullptr;
  }

  if (args_.empty()) {
    for (auto &field : fields_) { field->VerifyType(ctx); }
    return ctx->set_type(this, type::Type_);
  } else {
    return ctx->set_type(this, type::GenStruct(std::move(ts)));
  }
}

void StructLiteral::Validate(Context *ctx) {
  for (auto &a : args_) { a->Validate(ctx); }
  for (auto &field : fields_) {
    if (field->type_expr) { field->type_expr->VerifyType(ctx); }
    field->Validate(ctx);
  }
}

void StructLiteral::ExtractJumps(JumpExprs *rets) const {
  for (auto &a : args_) { a->ExtractJumps(rets); }
  for (auto &f : fields_) { f->ExtractJumps(rets); }
}

static ir::Register GenerateStruct(StructLiteral *sl, Context *ctx) {
  ir::Register r = ir::CreateStruct(ctx->mod_);
  for (auto const &field : sl->fields_) {
    // TODO initial values? hashatgs?

    // NOTE: CreateStructField may invalidate all other struct fields, so it's
    // not safe to access these registers returned by CreateStructField after
    // a subsequent call to CreateStructField.
    ir::CreateStructField(
        r, field->type_expr->EmitIR(ctx)[0].reg_or<type::Type const *>());
    ir::SetStructFieldName(r, field->id_);
    for (auto const &hashtag : field->hashtags_) {
      ir::AddHashtagToField(r, hashtag);
    }
  }
  return ir::FinalizeStruct(r);
}

base::vector<ir::Val> ast::StructLiteral::EmitIR(Context *ctx) {
  if (args_.empty()) {
    return {ir::Val::Reg(GenerateStruct(this, ctx), type::Type_)};

  } else {
    LOG << ctx->bound_constants_;
    ir::Func *&ir_func = ctx->mod_->ir_funcs_[ctx->bound_constants_][this];
    if (!ir_func) {
      auto &work_item = ctx->mod_->to_complete_.emplace(ctx->bound_constants_,
                                                        this, ctx->mod_);

      base::vector<std::pair<std::string, Expression *>> args;
      args.reserve(args_.size());
      for (auto const &d : args_) {
        args.emplace_back(d->id_, d->init_val.get());
      }
      ir_func = ctx->mod_->AddFunc(
          type::Func(ctx->type_of(this)->as<type::GenericStruct>().deps_,
                     {type::Type_}),
          std::move(args));
      ir_func->work_item = &work_item;
    }
    return {ir::Val::Func(ir_func->type_, ir_func)};
  }
}

base::vector<ir::RegisterOr<ir::Addr>> ast::StructLiteral::EmitLVal(Context *ctx) {
  UNREACHABLE(*this);
}

void StructLiteral::CompleteBody(Context *ctx) {
  // TODO have validate return a bool distinguishing if there are errors and
  // whether or not we can proceed.

  auto *t = ctx->type_of(this);

  ir::Func *&ir_func = ctx->mod_->ir_funcs_[ctx->bound_constants_][this];
  CURRENT_FUNC(ir_func) {
    ir::BasicBlock::Current = ir_func->entry();
    ir::SetRet(0, ir::GenerateStruct(this));
    ir::ReturnJump();
  }
}
}  // namespace ast

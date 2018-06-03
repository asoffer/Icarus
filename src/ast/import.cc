#include "ast/import.h"

#include <future>
#include "ast/stages.h"
#include "ast/verify_macros.h"
#include "backend/eval.h"
#include "base/guarded.h"
#include "ir/val.h"

void ScheduleModule(const Source::Name &src);

extern base::guarded<std::unordered_map<
    Source::Name, std::shared_future<std::unique_ptr<Module>>>>
    modules;

namespace AST {
std::string Import::to_string(size_t n) const {
  return "import " + operand_->to_string(n);
}
void Import::assign_scope(Scope *scope) {
  if (stage_range_.high < AssignScopeStage ||
      stage_range_.low >= AssignScopeStage) {
    return;
  }
  stage_range_.low = AssignScopeStage;
  scope_           = scope;
  operand_->assign_scope(scope);
}

void Import::contextualize(
    const Node *correspondant,
    const std::unordered_map<const Expression *, IR::Val> &replacements) {
  operand_->contextualize(correspondant->as<Import>().operand_.get(),
                          replacements);
}
void Import::SaveReferences(Scope *scope, std::vector<IR::Val> *args) {
  operand_->SaveReferences(scope, args);
}

IR::Val Import::EmitIR(Context *ctx) {
  ASSERT(cache_.has_value());
  return IR::Val::Mod(modules.lock()->at(*cache_).get().get());
}

IR::Val Import::EmitLVal(Context *ctx) { UNREACHABLE(); }
Import *Import::Clone() const {
  auto *result = new Import(base::wrap_unique(operand_->Clone()));
  result->span = span;
  return result;
}

void Import::VerifyType(Context *ctx) {
  VERIFY_AND_RETURN_ON_ERROR(operand_);
  lvalue = Assign::Const;
  type   = type::Module;
  if (operand_->type != type::String || operand_->lvalue != Assign::Const) {
    ctx->error_log_.InvalidImport(operand_->span);
  } else {
    cache_ =
        Source::Name{backend::EvaluateAs<const char *>(operand_.get(), ctx)};
    ScheduleModule(*cache_);
  }
  limit_to(operand_);
}
}  // namespace AST

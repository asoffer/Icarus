#include "ast/import.h"

#include <future>
#include "ast/stages.h"
#include "ast/verify_macros.h"
#include "backend/eval.h"
#include "base/guarded.h"
#include "ir/val.h"
#include "run/run.h"
#include "type/char_buffer.h"

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
    const base::unordered_map<const Expression *, IR::Val> &replacements) {
  operand_->contextualize(correspondant->as<Import>().operand_.get(),
                          replacements);
}
void Import::SaveReferences(Scope *scope, base::vector<IR::Val> *args) {
  operand_->SaveReferences(scope, args);
}

base::vector<IR::Val> Import::EmitIR(Context *ctx) {
  ASSERT(cache_.has_value());
  auto fut = modules.lock()->at(*cache_);
  auto *mod = fut.get().get();
  return {IR::Val(mod)};
}

base::vector<IR::Register> Import::EmitLVal(Context *ctx) { UNREACHABLE(); }
Import *Import::Clone() const {
  auto *result = new Import(base::wrap_unique(operand_->Clone()));
  result->span = span;
  return result;
}

type::Type const *Import::VerifyType(Context *ctx) {
  VERIFY_OR_RETURN(operand_type, operand_);
  type = type::Module;
  ctx->types_.buffered_emplace(this, type::Module);

  if (!operand_type->is<type::CharBuffer>()) {
    ctx->error_log_.InvalidImport(operand_->span);
  } else {
    cache_ = frontend::Source::Name{
        backend::EvaluateAs<std::string_view>(operand_.get(), ctx)};
    ScheduleModule(*cache_);
  }
  limit_to(operand_);
  return type::Module;
}
}  // namespace AST

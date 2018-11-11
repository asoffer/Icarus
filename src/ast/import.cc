#include "ast/import.h"

#include <future>
#include "ast/overload_set.h"
#include "ast/verify_macros.h"
#include "backend/eval.h"
#include "base/guarded.h"
#include "ir/val.h"
#include "run/run.h"
#include "type/char_buffer.h"

namespace ast {
std::string Import::to_string(size_t n) const {
  return "import " + operand_->to_string(n);
}
void Import::assign_scope(Scope *scope) {
  scope_ = scope;
  operand_->assign_scope(scope);
}

base::vector<ir::Val> Import::EmitIR(Context *ctx) {
  ASSERT(cache_.has_value());
  auto fut  = modules.lock()->at(*cache_);
  auto *mod = fut.get().get();
  return {ir::Val(mod)};
}

base::vector<ir::Register> Import::EmitLVal(Context *ctx) { UNREACHABLE(); }

type::Type const *Import::VerifyType(Context *ctx) {
  VERIFY_OR_RETURN(operand_type, operand_);
  ctx->set_type(this, type::Module);

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
}  // namespace ast

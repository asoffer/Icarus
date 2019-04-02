#include "ast/import.h"

#include <filesystem>
#include "ast/overload_set.h"
#include "backend/eval.h"
#include "base/guarded.h"
#include "misc/context.h"
#include "type/primitive.h"

namespace ast {
std::string Import::to_string(size_t n) const {
  return "import " + operand_->to_string(n);
}
void Import::assign_scope(core::Scope *scope) {
  scope_ = scope;
  operand_->assign_scope(scope);
}

ir::Results Import::EmitIr(Context *ctx) { return ir::Results{module_.get()}; }

VerifyResult Import::VerifyType(Context *ctx) {
  ASSIGN_OR(return _, auto result, operand_->VerifyType(ctx));
  bool err = false;
  if (result.type_ != type::ByteView) {
    // TODO allow (import) overload
    ctx->error_log()->InvalidImport(operand_->span);
    err = true;
  }

  if (!result.const_) {
    ctx->error_log()->NonConstantImport(operand_->span);
    err = true;
  }

  if (err) { return VerifyResult::Error(); }
  // TODO storing this might not be safe.
  module_ = Module::Schedule(
      ctx->error_log(),
      std::filesystem::path{
          backend::EvaluateAs<std::string_view>(operand_.get(), ctx)},
      *ctx->mod_->path_);
  if (!module_.valid()) { return VerifyResult::Error(); }
  return ctx->set_result(this, VerifyResult::Constant(type::Module));
}
}  // namespace ast

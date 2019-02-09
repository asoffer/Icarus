#include "ast/import.h"

#include <filesystem>
#include "ast/overload_set.h"
#include "backend/eval.h"
#include "base/guarded.h"
#include "misc/context.h"
#include "ir/val.h"
#include "type/primitive.h"

namespace ast {
std::string Import::to_string(size_t n) const {
  return "import " + operand_->to_string(n);
}
void Import::assign_scope(Scope *scope) {
  scope_ = scope;
  operand_->assign_scope(scope);
}

std::vector<ir::Val> Import::EmitIR(Context *ctx) {
  return {ir::Val(module_.get())};
}

std::vector<ir::RegisterOr<ir::Addr>> Import::EmitLVal(Context *ctx) { UNREACHABLE(); }

VerifyResult Import::VerifyType(Context *ctx) {
  ASSIGN_OR(return VerifyResult::Error(), auto result,
                   operand_->VerifyType(ctx));
  bool err = false;
  if (result.type_ != type::ByteView) {
    // TODO allow (import) overload
    ctx->error_log_.InvalidImport(operand_->span);
    err = true;
  }

  if (!result.const_) {
    ctx->error_log_.NonConstantImport(operand_->span);
    err = true;
  }

  if (err) { return VerifyResult::Error(); }
  // TODO storing this might not be safe.
  module_ = Module::Schedule(
      std::filesystem::path{
          backend::EvaluateAs<std::string_view>(operand_.get(), ctx)},
      *ctx->mod_->path_);
  return VerifyResult::Constant(ctx->set_type(this, type::Module));
}
}  // namespace ast

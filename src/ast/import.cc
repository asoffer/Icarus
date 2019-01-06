#include "ast/import.h"

#include <filesystem>
#include "ast/overload_set.h"
#include "backend/eval.h"
#include "base/guarded.h"
#include "context.h"
#include "ir/val.h"
#include "module.h"
#include "type/primitive.h"

namespace ast {
std::string Import::to_string(size_t n) const {
  return "import " + operand_->to_string(n);
}
void Import::assign_scope(Scope *scope) {
  scope_ = scope;
  operand_->assign_scope(scope);
}

base::vector<ir::Val> Import::EmitIR(Context *ctx) {
  return {ir::Val(module_.get())};
}

base::vector<ir::RegisterOr<ir::Addr>> Import::EmitLVal(Context *ctx) { UNREACHABLE(); }

VerifyResult Import::VerifyType(Context *ctx) {
  ASSIGN_OR(return VerifyResult::Error(), auto result,
                   operand_->VerifyType(ctx));
  if (result.type_ != type::ByteView) {
    ctx->error_log_.InvalidImport(operand_->span);
  } else {
    module_ = Module::Schedule(
        std::filesystem::path{
            backend::EvaluateAs<std::string_view>(operand_.get(), ctx)},
        *ctx->mod_->path_);
  }
  return VerifyResult::Constant(ctx->set_type(this, type::Module));
}
}  // namespace ast

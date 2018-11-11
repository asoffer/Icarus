#include "ast/struct_literal.h"

#include <sstream>
#include "ast/verify_macros.h"
#include "backend/exec.h"
#include "base/untyped_buffer.h"
#include "ir/func.h"
#include "ir/val.h"
#include "type/function.h"
#include "type/struct.h"

namespace ir {
Register CreateStruct(ast::StructLiteral *lit);
void CreateStructField(type::Struct *struct_type,
                       RegisterOr<type::Type const *> type);
void SetStructFieldName(type::Struct *struct_type, std::string_view field_name);
}  // namespace ir

namespace ast {
std::string StructLiteral::to_string(size_t n) const {
  std::stringstream ss;
  ss << "struct {\n";
  for (const auto &f : fields_) {
    ss << std::string((n + 1) * 2, ' ') << f->to_string(n) << "\n";
  }
  ss << std::string(2 * n, ' ') << "}";
  return ss.str();
}

void StructLiteral::assign_scope(Scope *scope) {
  scope_     = scope;
  type_scope = scope->add_child<DeclScope>();
  for (auto &f : fields_) { f->assign_scope(type_scope.get()); }
}

type::Type const *StructLiteral::VerifyType(Context *ctx) {
  ctx->set_type(this, type::Type_);
  return type::Type_;
}

void StructLiteral::Validate(Context *ctx) {
  for (auto &field : fields_) {
    if (field->type_expr) { field->type_expr->VerifyType(ctx); }
    field->Validate(ctx);
  }
}

void StructLiteral::ExtractJumps(JumpExprs *rets) const {
  for (auto &f : fields_) { f->ExtractJumps(rets); }
}

base::vector<ir::Val> ast::StructLiteral::EmitIR(Context *ctx) {
  return {ir::Val::Reg(ir::CreateStruct(this), type::Type_)};
}

void ast::StructLiteral::Complete(type::Struct *s) {
  ir::Func f(mod_, type::Func({}, {}), {});
  Context ctx(mod_);
  CURRENT_FUNC(&f) {
    ir::BasicBlock::Current = f.entry();

    for (const auto &field : fields_) {
      // TODO initial values? hashatgs?

      // NOTE: CreateStructField may invalidate all other struct fields, so it's
      // not safe to access these registers returned by CreateStructField after
      // a subsequent call to CreateStructField.
      ir::CreateStructField(
          s, field->type_expr->EmitIR(&ctx)[0].reg_or<type::Type const *>());
      ir::SetStructFieldName(s, field->id_);
    }
    ir::ReturnJump();
  }

  backend::ExecContext exec_ctx;
  backend::Execute(&f, base::untyped_buffer(0), {}, &exec_ctx);
}

base::vector<ir::Register> ast::StructLiteral::EmitLVal(Context *ctx) {
  UNREACHABLE(*this);
}
}  // namespace ast

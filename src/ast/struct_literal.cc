#include "ast/struct_literal.h"

#include <sstream>
#include "ast/declaration.h"
#include "backend/exec.h"
#include "base/untyped_buffer.h"
#include "ir/func.h"
#include "ir/val.h"
#include "type/function.h"
#include "type/struct.h"

namespace ir {
Register CreateStruct(ast::StructLiteral *lit);
void CreateStructField(Register struct_type,
                       RegisterOr<type::Type const *> type);
void SetStructFieldName(Register struct_type, std::string_view field_name);
Register FinalizeStruct(Register r);
}  // namespace ir

namespace ast {
std::string StructLiteral::to_string(size_t n) const {
  std::stringstream ss;
  ss << "struct " << args_.to_string(n) << "{\n";
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
  return ctx->set_type(this, type::Type_);
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
  ir::Register r = ir::CreateStruct(this);
  for (const auto &field : fields_) {
    // TODO initial values? hashatgs?

    // NOTE: CreateStructField may invalidate all other struct fields, so it's
    // not safe to access these registers returned by CreateStructField after
    // a subsequent call to CreateStructField.
    ir::CreateStructField(
        r, field->type_expr->EmitIR(ctx)[0].reg_or<type::Type const *>());
    ir::SetStructFieldName(r, field->id_);
  }
  return {ir::Val::Reg(ir::FinalizeStruct(r), type::Type_)};
}

base::vector<ir::Register> ast::StructLiteral::EmitLVal(Context *ctx) {
  UNREACHABLE(*this);
}
}  // namespace ast

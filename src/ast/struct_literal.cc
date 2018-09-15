#include "ast/struct_literal.h"

#include <sstream>
#include "ast/verify_macros.h"
#include "ir/val.h"

namespace IR {
Register CreateStruct();
void CreateStructField(Register struct_type,
                       RegisterOr<type::Type const *> type);
void SetStructFieldName(Register struct_type, std::string_view field_name);
void FinalizeStruct(Register r);
}  // namespace IR

namespace AST {
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
  STAGE_CHECK(AssignScopeStage, AssignScopeStage);
  scope_     = scope;
  type_scope = scope->add_child<DeclScope>();
  for (auto &f : fields_) { f->assign_scope(type_scope.get()); }
}

void StructLiteral::VerifyType(Context *ctx) {
  VERIFY_STARTING_CHECK_EXPR;
  lvalue = Assign::Const;
  type = type::Type_;
}

void StructLiteral::Validate(Context *ctx) {
  STAGE_CHECK(StartBodyValidationStage, DoneBodyValidationStage);

  for (auto &field : fields_) {
    if (field->type_expr) { field->type_expr->VerifyType(ctx); }
    field->Validate(ctx);
  }
}

void StructLiteral::SaveReferences(Scope *scope, base::vector<IR::Val> *args) {
  for (auto &f: fields_) { f->SaveReferences(scope, args); }
}

void StructLiteral::contextualize(
    const Node *correspondant,
    const base::unordered_map<const Expression *, IR::Val> &replacements) {
  for (size_t i = 0; i < fields_.size(); ++i) {
    fields_[i]->contextualize(
        correspondant->as<StructLiteral>().fields_[i].get(), replacements);
  }
}

void StructLiteral::ExtractReturns(
    base::vector<const Expression *> *rets) const {
  for (auto &f : fields_) { f->ExtractReturns(rets); }
}

StructLiteral *StructLiteral::Clone() const {
  auto *result = new StructLiteral;
  result->span = span;
  result->fields_.reserve(fields_.size());
  for (const auto &f : fields_) { result->fields_.emplace_back(f->Clone()); }
  return result;
}

base::vector<IR::Val> AST::StructLiteral::EmitIR(Context *ctx) {
  IR::Register new_struct = IR::CreateStruct();
  for (const auto &field : fields_) {
    // TODO initial values? hashatgs?

    // NOTE: CreateStructField may invalidate all other struct fields, so it's
    // not safe to access these registers returned by CreateStructField after a
    // subsequent call to CreateStructField.
    IR::CreateStructField(
        new_struct, field->type_expr->EmitIR(ctx)[0].reg_or<type::Type const *>());
    IR::SetStructFieldName(new_struct, field->identifier->token);
  }
  IR::FinalizeStruct(new_struct);
  return {IR::Val::Reg(new_struct, type::Type_)};
}

base::vector<IR::Register> AST::StructLiteral::EmitLVal(Context *ctx) { UNREACHABLE(*this); }
}  // namespace AST

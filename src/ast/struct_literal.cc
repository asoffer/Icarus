#include "ast/struct_literal.h"

#include <sstream>
#include "ast/verify_macros.h"
#include "ir/val.h"

namespace IR {
Val CreateStruct();
Val FinalizeStruct(Val v);
void InsertField(Val struct_type, std::string field_name, Val type,
                 Val init_val);
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

void StructLiteral::SaveReferences(Scope *scope, std::vector<IR::Val> *args) {
  for (auto &f: fields_) { f->SaveReferences(scope, args); }
}

void StructLiteral::contextualize(
    const Node *correspondant,
    const std::unordered_map<const Expression *, IR::Val> &replacements) {
  for (size_t i = 0; i < fields_.size(); ++i) {
    fields_[i]->contextualize(
        correspondant->as<StructLiteral>().fields_[i].get(), replacements);
  }
}

void StructLiteral::ExtractReturns(
    std::vector<const Expression *> *rets) const {
  for (auto &f : fields_) { f->ExtractReturns(rets); }
}

StructLiteral *StructLiteral::Clone() const {
  auto *result = new StructLiteral;
  result->span = span;
  result->fields_.reserve(fields_.size());
  for (const auto &f : fields_) { result->fields_.emplace_back(f->Clone()); }
  return result;
}

std::vector<IR::Val> AST::StructLiteral::EmitIR(Context *ctx) {
  auto new_struct = IR::CreateStruct();
  for (const auto &field : fields_) {
    // TODO in initial value doesn't match type of field?
    // That should probably be handled elsewhere consistently with function
    // default args.
    IR::Val init_val = IR::Val::None();
    if (field->init_val) {
      field->init_val->assign_scope(scope_);
      field->init_val->Validate(ctx);
      init_val = field->init_val->EmitIR(ctx)[0];
    }

    IR::Val field_type;
    if (field->type_expr) {
      field_type = field->type_expr->EmitIR(ctx)[0];
    } else {
      ASSERT(nullptr != field->init_val.get());
      field_type = IR::Val::Type(field->init_val->type);
    }
    IR::InsertField(new_struct, field->identifier->token, std::move(field_type),
                    std::move(init_val));
  }
  return {IR::FinalizeStruct(std::move(new_struct))};
}

std::vector<IR::Val> AST::StructLiteral::EmitLVal(Context *ctx) { UNREACHABLE(*this); }
}  // namespace AST

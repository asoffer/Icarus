#include "ast/struct_literal.h"

#include <sstream>
#include "ast/verify_macros.h"
#include "backend/exec.h"
#include "base/untyped_buffer.h"
#include "ir/func.h"
#include "ir/val.h"
#include "type/function.h"
#include "type/struct.h"

namespace IR {
Register CreateStruct(AST::StructLiteral *lit);
void CreateStructField(type::Struct *struct_type,
                       RegisterOr<type::Type const *> type);
void SetStructFieldName(type::Struct *struct_type, std::string_view field_name);
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

void StructLiteral::SaveReferences(Scope *scope, base::vector<IR::Val> *args) {
  for (auto &f : fields_) { f->SaveReferences(scope, args); }
}

void StructLiteral::contextualize(
    const Node *correspondant,
    const base::unordered_map<const Expression *, IR::Val> &replacements) {
  for (size_t i = 0; i < fields_.size(); ++i) {
    fields_[i]->contextualize(
        correspondant->as<StructLiteral>().fields_[i].get(), replacements);
  }
}

void StructLiteral::ExtractJumps(JumpExprs *rets) const {
  for (auto &f : fields_) { f->ExtractJumps(rets); }
}

StructLiteral *StructLiteral::Clone() const {
  auto *result = new StructLiteral;
  result->span = span;
  result->fields_.reserve(fields_.size());
  for (const auto &f : fields_) { result->fields_.emplace_back(f->Clone()); }
  return result;
}

base::vector<IR::Val> AST::StructLiteral::EmitIR(Context *ctx) {
  return {IR::Val::Reg(IR::CreateStruct(this), type::Type_)};
}

void AST::StructLiteral::Complete(type::Struct *s) {
  IR::Func f(mod_, type::Func({}, {}), {});
  Context ctx(mod_);
  CURRENT_FUNC(&f) {
    IR::BasicBlock::Current = f.entry();

    for (const auto &field : fields_) {
      // TODO initial values? hashatgs?

      // NOTE: CreateStructField may invalidate all other struct fields, so it's
      // not safe to access these registers returned by CreateStructField after
      // a subsequent call to CreateStructField.
      IR::CreateStructField(
          s, field->type_expr->EmitIR(&ctx)[0].reg_or<type::Type const *>());
      IR::SetStructFieldName(s, field->id_);
    }
    IR::ReturnJump();
  }

  backend::ExecContext exec_ctx;
  backend::Execute(&f, base::untyped_buffer(0), {}, &exec_ctx);
}

base::vector<IR::Register> AST::StructLiteral::EmitLVal(Context *ctx) {
  UNREACHABLE(*this);
}
}  // namespace AST

#include "ast/interface.h"

#include "ast/verify_macros.h"
#include "context.h"
#include "error/log.h"
#include "ir/val.h"
#include "type/type.h"

namespace AST {
void Interface::assign_scope(Scope *scope) {
  STAGE_CHECK(AssignScopeStage, AssignScopeStage);
  scope_     = scope;
  body_scope_ = scope->add_child<DeclScope>();
  for (auto &d : decls_) { d.assign_scope(body_scope_.get()); }
}

std::string Interface::to_string(size_t n) const {
  if (decls_.empty()) { return "interface {}"; }
  std::stringstream ss;
  ss << "interface {\n";
  for (const auto &decl : decls_) {
    ss << std::string(n * 2, ' ') << decl.to_string(n) << "\n";
  }
  ss << "}";
  return ss.str();
}

void Interface::VerifyType(Context *ctx) {
  VERIFY_STARTING_CHECK_EXPR;

  type = type::Interface;
  ctx->types_.buffered_emplace(this, type::Interface);

  for (auto &decl: decls_) {
    decl.VerifyType(ctx);
    HANDLE_CYCLIC_DEPENDENCIES;
    if (decl.init_val != nullptr) { NOT_YET(); }
    limit_to(&decl);
  }
}

void Interface::Validate(Context *ctx) {
  STAGE_CHECK(StartBodyValidationStage, DoneBodyValidationStage);
  for (auto &decl: decls_) { decl.Validate(ctx); }
}

void Interface::SaveReferences(Scope *scope, base::vector<IR::Val> *args) {
  for (auto &decl : decls_) { decl.SaveReferences(scope, args); }
}

void Interface::contextualize(
    const Node *correspondant,
    const base::unordered_map<const Expression *, IR::Val> &replacements) {
  for (size_t i = 0; i < decls_.size(); ++i) {
    decls_[i].contextualize(&correspondant->as<Interface>().decls_[i],
                            replacements);
  }
}

void Interface::ExtractReturns(base::vector<const Expression *> *rets) const {
  for (auto &d : decls_) { d.ExtractReturns(rets); }
}

Interface *Interface::Clone() const {
  auto *result = new Interface;
  result->span = span;
  result->decls_.reserve(decls_.size());
  for (const auto &decl : decls_) {
    result->decls_.emplace_back(decl.Clone());
  }
  return result;
}

base::vector<IR::Val> AST::Interface::EmitIR(Context *ctx) {
  // TODO this needs to be serialized as instructions so that we can evaluate
  // functions which return interfaces. For example,
  // HasFoo ::= (T: type) => interface {
  //   foo: T
  // }
  IR::Interface ifc;
  for (const auto &decl : decls_) {
    ifc.field_map_.emplace(decl.identifier->token, decl.type);
  }
  return { IR::Val::Interface(std::move(ifc))};
}

base::vector<IR::Register> AST::Interface::EmitLVal(Context *ctx) { UNREACHABLE(*this); }
}  // namespace AST

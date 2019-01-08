#include "ast/interface.h"

#include "context.h"
#include "error/log.h"
#include "ir/val.h"
#include "type/type.h"

namespace ast {
void Interface::assign_scope(Scope *scope) {
  scope_      = scope;
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

VerifyResult Interface::VerifyType(Context *ctx) {
  for (auto &decl : decls_) {
    decl.VerifyType(ctx);
    if (decl.init_val != nullptr) { NOT_YET(); }
  }
  return VerifyResult::Constant(ctx->set_type(this, type::Interface));
}

void Interface::Validate(Context *ctx) {
  for (auto &decl : decls_) { decl.Validate(ctx); }
}

void Interface::ExtractJumps(JumpExprs *rets) const {
  for (auto &d : decls_) { d.ExtractJumps(rets); }
}

base::vector<ir::Val> ast::Interface::EmitIR(Context *ctx) {
  // TODO this needs to be serialized as instructions so that we can evaluate
  // functions which return interfaces. For example,
  // HasFoo ::= (T: type) => interface {
  //   foo: T
  // }
  ir::Interface ifc;
  for (const auto &decl : decls_) {
    ifc.field_map_.emplace(decl.id_, ctx->type_of(&decl));
  }
  return {ir::Val::Interface(std::move(ifc))};
}

base::vector<ir::RegisterOr<ir::Addr>> ast::Interface::EmitLVal(Context *ctx) {
  UNREACHABLE(*this);
}
}  // namespace ast

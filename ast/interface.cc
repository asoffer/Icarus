#include "ast/interface.h"

#include "error/log.h"
#include "ir/val.h"
#include "misc/context.h"
#include "type/type.h"

namespace ir {
TypedRegister<type::Interface const *> CreateInterface(::Scope const *scope);
ir::TypedRegister<type::Interface const *> FinalizeInterface(Register r);
}

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
  return VerifyResult::Constant(ctx->set_type(this, type::Intf));
}

void Interface::Validate(Context *ctx) {
  for (auto &decl : decls_) { decl.Validate(ctx); }
}

void Interface::ExtractJumps(JumpExprs *rets) const {
  for (auto &d : decls_) { d.ExtractJumps(rets); }
}

std::vector<ir::Val> ast::Interface::EmitIR(Context *ctx) {
  // TODO this needs to be serialized as instructions so that we can evaluate
  // functions which return interfaces. For example,
  // HasFoo ::= (T: type) => interface {
  //   foo: T
  // }
  return {ir::Val(ir::FinalizeInterface(ir::CreateInterface(scope_)))};
}

std::vector<ir::RegisterOr<ir::Addr>> ast::Interface::EmitLVal(Context *ctx) {
  UNREACHABLE(*this);
}
}  // namespace ast

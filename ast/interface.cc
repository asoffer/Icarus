#include "ast/interface.h"

#include "error/log.h"
#include "ir/val.h"
#include "misc/context.h"
#include "type/type.h"

namespace ir {
TypedRegister<type::Interface const *> CreateInterface(
    core::Scope const *scope);
ir::TypedRegister<type::Interface const *> FinalizeInterface(Register r);
}  // namespace ir

namespace ast {
void Interface::assign_scope(core::Scope *scope) {
  scope_      = scope;
  body_scope_ = scope->add_child<core::DeclScope>();
  for (auto &d : decls_) { d.assign_scope(body_scope_.get()); }
}

void Interface::DependentDecls(base::Graph<Declaration *> *g,
                               Declaration *d) const {
  NOT_YET();
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

void Interface::ExtractJumps(JumpExprs *rets) const {
  for (auto &d : decls_) { d.ExtractJumps(rets); }
}

ir::Results Interface::EmitIr(Context *ctx) {
  // TODO this needs to be serialized as instructions so that we can evaluate
  // functions which return interfaces. For example,
  // HasFoo ::= (T: type) => interface {
  //   foo: T
  // }
  return ir::Results {ir::FinalizeInterface(ir::CreateInterface(scope_))};
}

}  // namespace ast

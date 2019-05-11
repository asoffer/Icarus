#include "ast/interface.h"

#include "error/log.h"
#include "misc/context.h"
#include "type/type.h"

namespace ir {
TypedRegister<type::Interface const *> CreateInterface(
    core::Scope const *scope);
ir::TypedRegister<type::Interface const *> FinalizeInterface(Reg r);
}  // namespace ir

namespace ast {
void Interface::DependentDecls(DeclDepGraph *g,
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

ir::Results Interface::EmitIr(Context *ctx) {
  // TODO this needs to be serialized as instructions so that we can evaluate
  // functions which return interfaces. For example,
  // HasFoo ::= (T: type) => interface {
  //   foo: T
  // }
  return ir::Results {ir::FinalizeInterface(ir::CreateInterface(scope_))};
}

}  // namespace ast

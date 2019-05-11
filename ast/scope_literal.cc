#include "ast/scope_literal.h"

#include "error/log.h"
#include "misc/context.h"
#include "core/scope.h"
#include "type/function.h"
#include "type/pointer.h"

namespace ast {
std::string ScopeLiteral::to_string(size_t n) const {
  std::stringstream ss;
  ss << "scope " << (stateful_ ? "!" : "") << "{\n";
  for (const auto &decl : decls_) {
    ss << std::string(n * 2, ' ') << decl.to_string(n) << "\n";
  }
  ss << "}";
  return ss.str();
}

void ScopeLiteral::DependentDecls(DeclDepGraph *g,
                                  Declaration *d) const {
  for (auto &decl : decls_) { decl.DependentDecls(g, d); }
}

ir::Results ScopeLiteral::EmitIr(Context *ctx) {
  return ir::Results{this};
}

}  // namespace ast

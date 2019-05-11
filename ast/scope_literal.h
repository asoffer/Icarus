#ifndef ICARUS_AST_SCOPE_LITERAL_H
#define ICARUS_AST_SCOPE_LITERAL_H

#include "ast/declaration.h"
#include "ast/literal.h"

namespace ast {
struct ScopeLiteral : public Literal {
  ScopeLiteral(bool stateful) : stateful_(stateful) {}
  ~ScopeLiteral() override {}

#include "ast_visitor/visitors.xmacro.h"

  std::string to_string(size_t n) const override;

  ir::Results EmitIr(Context *) override;

  std::vector<Declaration> decls_;
  std::unique_ptr<core::ScopeLitScope> body_scope_;
  bool stateful_ = false;
};
}  // namespace ast

#endif  // ICARUS_AST_SCOPE_LITERAL_H

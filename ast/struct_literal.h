#ifndef ICARUS_AST_STRUCT_LITERAL_H
#define ICARUS_AST_STRUCT_LITERAL_H

#include "ast/comma_list.h"
#include "ast/declaration.h"
#include "ast/literal.h"
#include "core/scope.h"

namespace type {
struct Struct;
}  // namespace type

namespace ast {
struct StructLiteral : public Literal {
  StructLiteral()                          = default;
  StructLiteral(StructLiteral &&) noexcept = default;
  ~StructLiteral() override {}

  StructLiteral &operator=(StructLiteral &&) noexcept = default;

#include "ast_visitor/visitors.xmacro.h"

  std::string to_string(size_t n) const override;
    
  void CompleteBody(Context *ctx);

  ir::Results EmitIr(Context *) override;

  std::unique_ptr<core::DeclScope> type_scope;
  std::vector<Declaration> fields_, args_;
  Module *mod_ = nullptr;
};
}  // namespace ast

#endif  // ICARUS_AST_STRUCT_LITERAL_H

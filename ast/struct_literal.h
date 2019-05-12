#ifndef ICARUS_AST_STRUCT_LITERAL_H
#define ICARUS_AST_STRUCT_LITERAL_H

#include "ast/declaration.h"
#include "ast/expression.h"
#include "core/scope.h"

namespace type {
struct Struct;
}  // namespace type

namespace ast {
struct StructLiteral : public Expression {
  StructLiteral()                          = default;
  StructLiteral(StructLiteral &&) noexcept = default;
  ~StructLiteral() override {}

  StructLiteral &operator=(StructLiteral &&) noexcept = default;

#include "ast_visitor/visitors.xmacro.h"

  std::string to_string(size_t n) const override {
    std::stringstream ss;
    ss << "struct (";
    for (auto &a : args_) { ss << a.to_string(n) << ", "; }
    ss << ") {\n";
    for (const auto &f : fields_) {
      ss << std::string((n + 1) * 2, ' ') << f.to_string(n) << "\n";
    }
    ss << std::string(2 * n, ' ') << "}";
    return ss.str();
  }

  std::unique_ptr<core::DeclScope> type_scope;
  std::vector<Declaration> fields_, args_;
  Module *mod_ = nullptr;
};
}  // namespace ast

#endif  // ICARUS_AST_STRUCT_LITERAL_H

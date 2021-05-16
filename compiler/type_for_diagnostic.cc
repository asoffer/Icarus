#include "compiler/type_for_diagnostic.h"

#include <sstream>

#include "absl/strings/str_cat.h"
#include "absl/strings/str_join.h"
#include "ast/ast.h"
#include "ast/visitor.h"

namespace compiler {
namespace {

struct StringifyExpression : ast::Visitor<std::string()> {
  explicit StringifyExpression(Context const *c)
      : context_(*ASSERT_NOT_NULL(c)) {}

  std::string Visit(ast::Expression const *node) {
    return ast::Visitor<std::string()>::Visit(node);
  }

  std::string Visit(ast::Access const *node) final {
    return absl::StrCat(Visit(node->operand()), ".", node->member_name());
  }

  std::string Visit(ast::Identifier const *node) final {
    return std::string(node->name());
  }

  std::string Visit(ast::Terminal const *node) final {
    std::stringstream ss;
    ss << node->value();
    return ss.str();
  }

 private:
  Context const &context_;
};

struct StringifyType : ast::Visitor<std::string()> {
  explicit StringifyType(Context const *c) : context_(*ASSERT_NOT_NULL(c)) {}

  std::string Visit(ast::Expression const *node) {
    return ast::Visitor<std::string()>::Visit(node);
  }

  std::string Visit(ast::Access const *node) final {
    return absl::StrCat(StringifyExpression(&context_).Visit(node->operand()),
                        ".", node->member_name());
  }

  std::string Visit(ast::ArrayType const *node) final {
    return absl::StrCat(
        "[",
        absl::StrJoin(node->lengths(), ", ",
                      [&](std::string *out, auto const *length_expr) {
                        absl::StrAppend(
                            out, StringifyExpression(&context_).Visit(node));
                      }),
        "; ", StringifyExpression(&context_).Visit(node->data_type()), "]");
  }

  std::string Visit(ast::Cast const *node) {
    return StringifyExpression(&context_).Visit(node->type());
  }

  std::string Visit(ast::Declaration const *node) final {
    if (auto const *expr = node->type_expr()) {
      return StringifyExpression(&context_).Visit(expr);
    } else {
      return StringifyType(&context_).Visit(node->init_val());
    }
  }

  std::string Visit(ast::DesignatedInitializer const *node) final {
    return StringifyExpression(&context_).Visit(node->type());
  }

  std::string Visit(ast::Identifier const *node) final {
    absl::Span<ast::Declaration const *const> decls = context_.decls(node);
    ASSERT(decls.size() == 1u);
    return Visit(decls[0]);
  }

  std::string Visit(ast::Import const *node) final { return "module"; }

  std::string Visit(ast::Terminal const *node) final {
    return context_.qual_types(node)[0].type().to_string();
  }

 private:
  Context const &context_;
};

}  // namespace

std::string TypeForDiagnostic(ast::Expression const *expr,
                              Context const &context) {
  return StringifyType(&context).Visit(expr);
}

}  // namespace compiler

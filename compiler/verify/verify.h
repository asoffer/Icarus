#ifndef ICARUS_COMPILER_VERIFY_VERIFY_H
#define ICARUS_COMPILER_VERIFY_VERIFY_H

#include "absl/types/span.h"
#include "ast/ast.h"
#include "ast/module.h"
#include "compiler/compilation_data.h"
#include "type/qual_type.h"

namespace compiler {

struct IncompleteField {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "incomplete-field";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Struct field has incomplete type."),
        diagnostic::SourceQuote().Highlighted(range,
                                              diagnostic::Style::ErrorText()));
  }

  std::string_view range;
};

struct TypeVerifier : CompilationDataReference {
  using signature = absl::Span<type::QualType const>();

  explicit TypeVerifier(CompilationDataReference ref)
      : CompilationDataReference(ref) {}

  absl::Span<type::QualType const> operator()(auto const *node) {
    return VerifyType(node);
  }
  absl::Span<type::QualType const> VerifyType(ast::Node const *node) {
    return node->visit(*this);
  }

#define ICARUS_AST_NODE_X(name)                                                \
  absl::Span<type::QualType const> VerifyType(ast::name const *node);

#include "ast/node.xmacro.h"
#undef ICARUS_AST_NODE_X
};

absl::Span<type::QualType const> VerifyType(CompilationDataReference data,
                                            ast::Node const *node);

// The reason to separate out type/body verification is if the body might
// transitively have identifiers referring to a declaration that is assigned
// directly to this node.
struct BodyVerifier : CompilationDataReference {
  using signature = bool();

  explicit BodyVerifier(CompilationDataReference ref)
      : CompilationDataReference(ref) {}

  bool operator()(auto const *node) { return VerifyBody(node); }
  bool VerifyBody(ast::Node const *node) { return node->visit(*this); }

  bool VerifyBody(ast::Expression const *node) { return true; }
  bool VerifyBody(ast::EnumLiteral const *node);
  bool VerifyBody(ast::FunctionLiteral const *node);
  bool VerifyBody(ast::ParameterizedStructLiteral const *node);
  bool VerifyBody(ast::StructLiteral const *node);
};

bool VerifyBody(CompilationDataReference data, ast::Node const *node);

struct PatternTypeVerifier : CompilationDataReference {
  using signature = bool(type::Type);

  explicit PatternTypeVerifier(CompilationDataReference ref)
      : CompilationDataReference(ref) {}

  bool operator()(auto const *node, type::Type t) {
    return VerifyPatternType(node, t);
  }

  bool VerifyPatternType(ast::Node const *node, type::Type t) {
    return node->visit(*this, t);
  }

  bool VerifyPatternType(ast::Access const *node, type::Type t);
  bool VerifyPatternType(ast::ArrayType const *node, type::Type t);
  bool VerifyPatternType(ast::BinaryOperator const *node, type::Type t);
  bool VerifyPatternType(ast::BindingDeclaration const *node, type::Type t);
  bool VerifyPatternType(ast::Call const *node, type::Type t);
  bool VerifyPatternType(ast::Declaration const *node, type::Type t);
  bool VerifyPatternType(ast::SliceType const *node, type::Type t);
  bool VerifyPatternType(ast::Terminal const *node, type::Type t);
  bool VerifyPatternType(ast::UnaryOperator const *node, type::Type t);
};
bool VerifyPatternType(CompilationDataReference data, ast::Node const *node,
                       type::Type t);

// Returns A function which can be executed to complete the data for an
// incomplete struct type pointed to by `s`.
std::optional<ir::Subroutine> StructDataCompletionFn(
    CompilationDataReference c, type::Struct *s,
    absl::Span<ast::Declaration const> fields);

bool CompleteStructData(CompilationDataReference data,
                        ast::StructLiteral const *node);
bool CompleteStructData(CompilationDataReference data,
                        ast::ParameterizedStructLiteral const *node);

}  // namespace compiler

#endif  // ICARUS_COMPILER_VERIFY_VERIFY_H

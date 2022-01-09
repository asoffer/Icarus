#ifndef ICARUS_COMPILER_VERIFY_VERIFY_H
#define ICARUS_COMPILER_VERIFY_VERIFY_H

#include "absl/types/span.h"
#include "ast/ast.h"
#include "compiler/compilation_data.h"
#include "type/qual_type.h"

namespace compiler {

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

// Returns A function which can be executed to complete the data for an
// incomplete struct type pointed to by `s`.
std::optional<ir::CompiledFn> StructDataCompletionFn(
    CompilationDataReference c, type::Struct *s,
    absl::Span<ast::Declaration const> fields);

bool CompleteStructData(CompilationDataReference data,
                        ast::StructLiteral const *node);

}  // namespace compiler

#endif  // ICARUS_COMPILER_VERIFY_VERIFY_H


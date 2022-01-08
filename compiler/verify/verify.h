#ifndef ICARUS_COMPILER_VERIFY_VERIFY_H
#define ICARUS_COMPILER_VERIFY_VERIFY_H

#include "ast/ast.h"
#include "compiler/compilation_data.h"

namespace compiler {

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


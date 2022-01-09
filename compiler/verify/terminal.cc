#include "ast/ast.h"
#include "base/meta.h"
#include "compiler/verify/verify.h"
#include "type/primitive.h"
#include "type/qual_type.h"
#include "type/slice.h"

namespace compiler {

absl::Span<type::QualType const> TypeVerifier::VerifyType(
    ast::Terminal const *node) {
  return context().set_qual_type(node,
                                 type::QualType::Constant(TerminalType(*node)));
}

bool PatternTypeVerifier::VerifyPatternType(ast::Terminal const *node,
                                            type::Type t) {
  return VerifyType(*this, node)[0].type() == t;
}

}  // namespace compiler

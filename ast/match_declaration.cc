#include "ast/match_declaration.h"

#include "backend/eval.h"
#include "type/interface.h"
#include "type/primitive.h"

namespace ast {
std::string MatchDeclaration::to_string(size_t n) const {
  return type_expr->to_string(n) + "`" + id_;
}

}  // namespace ast

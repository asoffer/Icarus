#include "ast/function_literal.h"

#include <sstream>

#include "absl/container/flat_hash_set.h"
#include "ast/declaration.h"
#include "ast/match_declaration.h"
#include "ast/repeated_unop.h"
#include "ast/terminal.h"
#include "backend/eval.h"
#include "error/log.h"
#include "ir/compiled_fn.h"
#include "misc/context.h"
#include "misc/module.h"
#include "type/function.h"
#include "type/tuple.h"
#include "type/type.h"

namespace ast {
std::string FunctionLiteral::to_string(size_t n) const {
  std::stringstream ss;
  ss << "(";
  if (!inputs_.empty()) {
    auto iter = inputs_.begin();
    ss << iter->value->to_string(n);
    ++iter;
    while (iter != inputs_.end()) {
      ss << ", " << iter->value->to_string(n);
      ++iter;
    }
  }
  ss << ") -> ";
  if (!return_type_inferred_) {
    ss << "(";
    if (!outputs_.empty()) {
      auto iter = outputs_.begin();
      ss << iter->get()->to_string(n);
      ++iter;
      while (iter != outputs_.end()) {
        ss << ", " << iter->get()->to_string(n);
        ++iter;
      }
    }
    ss << ")";
  }
  ss << " {\n"
     << statements_.to_string(n + 1) << std::string(2 * n, ' ') << "}";
  return ss.str();
}

}  // namespace ast

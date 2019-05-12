#include "ast/array_literal.h"

#include "error/log.h"
#include "ir/cmd.h"
#include "misc/context.h"
#include "type/array.h"
#include "type/cast.h"
#include "type/pointer.h"

namespace ast {
std::string ArrayLiteral::to_string(size_t n) const {
  std::stringstream ss;
  ss << "[";
  if (!cl_.exprs_.empty()) {
    auto iter = cl_.exprs_.begin();
    ss << (*iter)->to_string(n);
    ++iter;
    while (iter != cl_.exprs_.end()) {
      ss << ", " << (*iter)->to_string(n);
      ++iter;
    }
  }
  ss << "]";
  return ss.str();
}

}  // namespace ast

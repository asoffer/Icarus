#include "ast/comma_list.h"

#include "ir/cmd.h"
#include "misc/context.h"
#include "type/tuple.h"

namespace ast {
std::string CommaList::to_string(size_t n) const {
  std::stringstream ss;
  if (exprs_.empty()) { return "()"; }
  if (parenthesized_) { ss << "("; }
  auto iter = exprs_.begin();
  ss << (*iter)->to_string(n);
  ++iter;
  while (iter != exprs_.end()) {
    ss << ", " << (*iter)->to_string(n);
    ++iter;
  }
  if (parenthesized_) { ss << ")"; }
  return ss.str();
}

}  // namespace ast

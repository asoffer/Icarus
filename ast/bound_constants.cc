#include "ast/bound_constants.h"

#include "ast/declaration.h"

namespace ast {
std::string BoundConstants::to_string() const {
  std::stringstream ss;
  ss << "{\n";
  for (auto const& [decl, val] : constants_) {
    ss << reinterpret_cast<uintptr_t>(decl) << "(" << decl->id_
       << ") : " << val.to_string() << "\n";
  }

  ss << "}";
  return ss.str();
}
}  // namespace ast

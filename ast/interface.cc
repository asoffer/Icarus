#include "ast/interface.h"

#include "error/log.h"
#include "misc/context.h"
#include "type/type.h"

namespace ast {

std::string Interface::to_string(size_t n) const {
  if (decls_.empty()) { return "interface {}"; }
  std::stringstream ss;
  ss << "interface {\n";
  for (const auto &decl : decls_) {
    ss << std::string(n * 2, ' ') << decl.to_string(n) << "\n";
  }
  ss << "}";
  return ss.str();
}

}  // namespace ast

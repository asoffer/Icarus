#include "ast/enum_literal.h"

#include "ast/identifier.h"
#include "ir/cmd.h"
#include "misc/context.h"

namespace ast {
std::string EnumLiteral::to_string(size_t n) const {
  std::stringstream ss;
  switch (kind_) {
    case Kind::Enum: ss << "enum"; break;
    case Kind::Flags: ss << "flags"; break;
  }
  ss << " {\n";
  for (auto &elem : elems_) {
    ss << std::string((n + 1) * 2, ' ') << elem->to_string(n + 1) << "\n";
  }
  ss << std::string(n * 2, ' ') << "}";
  return ss.str();
}

}  // namespace ast

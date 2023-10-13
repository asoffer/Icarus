#ifndef ICARUS_COMMON_RESOURCES_H
#define ICARUS_COMMON_RESOURCES_H

#include <cstdint>
#include <string>
#include <string_view>

#include "nth/container/flyweight_set.h"
#include "type/type.h"

namespace ic {

struct Resources {
  size_t StringLiteralIndex(std::string &&s) {
    return strings.index(strings.insert(std::move(s)).first);
  }
  size_t StringLiteralIndex(std::string const &s) {
    return strings.index(strings.insert(s).first);
  }
  size_t StringLiteralIndex(char const *s) {
    return strings.index(strings.insert(s).first);
  }
  std::string_view StringLiteral(size_t index) const {
    return strings.from_index(index);
  }
  size_t IdentifierIndex(std::string_view s) {
    return identifiers.index(identifiers.insert(s).first);
  }

  std::string_view Identifier(size_t index) {
    return identifiers.from_index(index);
  }

  // Values of string literals used in the program.
  nth::flyweight_set<std::string> strings;

  // Values of integer constants used in the program.
  nth::flyweight_set<uint64_t> integers;

  // Values of all identifiers in the program. Identifier content must outlive
  // any access to this member.
  nth::flyweight_set<std::string_view> identifiers;

  nth::flyweight_set<std::pair<std::string_view, type::FunctionType>>
      foreign_functions;
};

inline Resources resources;

}  // namespace ic

#endif  // ICARUS_COMMON_RESOURCES_H

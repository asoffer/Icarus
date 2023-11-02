#ifndef ICARUS_COMMON_RESOURCES_H
#define ICARUS_COMMON_RESOURCES_H

#include <cstdint>
#include <string>
#include <string_view>

#include "common/module_map.h"
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
  size_t StringLiteralIndex(std::string_view s) {
    return strings.index(strings.insert(std::string(s)).first);
  }
  size_t StringLiteralIndex(char const *s) {
    return strings.index(strings.insert(s).first);
  }
  std::string_view StringLiteral(size_t index) const {
    return strings.from_index(index);
  }
  size_t ForeignFunctionIndex(std::string_view name, type::FunctionType t) {
    return foreign_functions.index(
        foreign_functions.insert(std::make_pair(StringLiteralIndex(name), t))
            .first);
  }

  // Values of string literals used in the program.
  nth::flyweight_set<std::string> strings;

  nth::flyweight_set<std::pair<size_t, type::FunctionType>> foreign_functions;

  ModuleMap module_map;
};

inline Resources resources;

}  // namespace ic

#endif  // ICARUS_COMMON_RESOURCES_H

#include "common/string_literal.h"

namespace ic {
namespace {

nth::flyweight_set<std::string> strings = {""};

}  // namespace

StringLiteral::StringLiteral()
    : StrongIdentifierType<StringLiteral, uint32_t>(0) {}
StringLiteral::StringLiteral(std::string &&s)
    : StrongIdentifierType<StringLiteral, uint32_t>(
          strings.index(strings.insert(std::move(s)).first)) {}
StringLiteral::StringLiteral(std::string const &s)
    : StrongIdentifierType<StringLiteral, uint32_t>(
          strings.index(strings.insert(s).first)) {}
StringLiteral::StringLiteral(std::string_view s)
    : StrongIdentifierType<StringLiteral, uint32_t>(
          strings.index(strings.insert(std::string(s)).first)) {}

std::string_view StringLiteral::str() const {
  return strings.from_index(index());
}

nth::flyweight_set<std::string> const &StringLiteral::All() { return strings; }

}  // namespace ic

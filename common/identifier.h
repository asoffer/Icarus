#ifndef ICARUS_COMMON_IDENTIFIER_H
#define ICARUS_COMMON_IDENTIFIER_H

#include <cstdint>

#include "common/strong_identifier_type.h"
#include "nth/container/flyweight_set.h"

namespace ic {
namespace internal_identifier {

inline nth::flyweight_set<std::string> identifiers;

}  // namespace internal_identifier

// Represents an identifier in a program.
struct Identifier : StrongIdentifierType<Identifier, uint32_t> {
  explicit Identifier(uint32_t n) : StrongIdentifierType(n) {}

  explicit Identifier(char const *identifier)
      : StrongIdentifierType(internal_identifier::identifiers.index(
            internal_identifier::identifiers.insert(identifier).first)) {}
  explicit Identifier(std::string_view identifier)
      : StrongIdentifierType(internal_identifier::identifiers.index(
            internal_identifier::identifiers.insert(std::string(identifier))
                .first)) {}
  explicit Identifier(std::string &&identifier)
      : StrongIdentifierType(internal_identifier::identifiers.index(
            internal_identifier::identifiers.insert(std::move(identifier))
                .first)) {}
  explicit Identifier(std::string const &identifier)
      : StrongIdentifierType(internal_identifier::identifiers.index(
            internal_identifier::identifiers.insert(identifier).first)) {}

  friend void NthPrint(auto &p, auto &f, Identifier const &i) {
    f(p, internal_identifier::identifiers.from_index(i.value()));
  }

  explicit operator std::string_view() const {
    return internal_identifier::identifiers.from_index(value());
  }
};

}  // namespace ic

#endif  // ICARUS_COMMON_IDENTIFIER_H

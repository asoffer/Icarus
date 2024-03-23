#include "common/identifier.h"

#include "common/internal/identifiers.h"

namespace ic {
namespace {}  // namespace

Identifier::Identifier() : Identifier(std::string("")) {}

Identifier::Identifier(std::string &&s) {
  auto [iter, inserted] = internal_common::Identifiers().insert(std::move(s));
  mutable_value()       = internal_common::Identifiers().index(iter);
}

Identifier::Identifier(std::string const &s) {
  auto [iter, inserted] = internal_common::Identifiers().insert(s);
  mutable_value()       = internal_common::Identifiers().index(iter);
}

Identifier::Identifier(std::string_view s) : Identifier(std::string(s)) {}

Identifier::operator std::string const &() const {
  return internal_common::Identifiers().from_index(value());
}

}  // namespace ic

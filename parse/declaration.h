#ifndef ICARUS_PARSE_DECLARATION_H
#define ICARUS_PARSE_DECLARATION_H

#include <cstdint>

#include "parse/node_index.h"

namespace ic {

// Describes syntactic information about a declaration, describing
//   * Whether the declaration is constant or non-constant (declared with `::`
//     or with `:` respectively).
//   * Whether the declaration has an initializer expression.
//   * Whether the declaration has an explicit type or the type is inferred from
//     an initilaizer.
//   * Whether the declaration is a parameter
// One may be tempted to talk about "having an initializer expression" as
// whether the declaration will be default-initialized, but this can lead to
// confusion. Specifically, `let a: b` has no initializer. As a standalone
// declaration, this will initialize `a` to the default value associated with
// the type `b`. However, as function parameter, it means that the parameter
// does *not* have a proposed default for when an argument is not bound to that
// parameter. Similarly, `let a: b = c` as a standalone declaration is
// initialized with `c`, and therefore is not default-initialized, but as a
// parameter, the value `c` is understood to be the default. The confusion
// arises from two distinct uses of the word "default" (default initialization,
// versus default arguments). Regardless this confusion is why we opt for the
// language of "having an initializer".
struct DeclarationKind {
  enum class Constness : uint8_t { NonConstant = 0, Constant = 4 };
  using enum Constness;

  constexpr DeclarationKind() : DeclarationKind(0) {}

  constexpr void set_explicit_type(bool e) {
    if (e) {
      data_ |= uint8_t{1};
    } else {
      data_ &= ~uint8_t{1};
    }
  }
  constexpr void set_initializer(bool initializer) {
    if (initializer) {
      data_ |= uint8_t{2};
    } else {
      data_ &= ~uint8_t{2};
    }
  }

  constexpr void set_constant(bool c) {
    if (c) {
      data_ |= uint8_t{4};
    } else {
      data_ &= ~uint8_t{4};
    }
  }
  constexpr void set_parameter(bool parameter) {
    if (parameter) {
      data_ |= uint8_t{8};
    } else {
      data_ &= ~uint8_t{8};
    }
  }

  constexpr bool explicit_type() const { return data_ & uint8_t{1}; }
  constexpr bool inferred_type() const { return not explicit_type(); }
  constexpr bool has_initializer() const { return data_ & uint8_t{2}; }
  constexpr bool constant() const { return data_ & uint8_t{4}; }
  constexpr bool parameter() const { return data_ & uint8_t{8}; }

 private:
  explicit constexpr DeclarationKind(uint8_t n) : data_(n) {}

  uint8_t data_;
};

struct DeclarationInfo {
  ParseNodeIndex index = ParseNodeIndex::Invalid();
  DeclarationKind kind;
};

}  // namespace ic

#endif  // ICARUS_PARSE_DECLARATION_H

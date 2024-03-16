#ifndef ICARUS_COMMON_INTERNAL_COMPONENT_H
#define ICARUS_COMMON_INTERNAL_COMPONENT_H

#include <cstdint>

#include "common/result.h"
#include "nth/utility/bytes.h"

namespace ic::internal_constants {

struct Component {
  enum class Category : uint8_t {
#define IC_XMACRO_TYPE_KIND(kind) kind##Type,
#include "common/language/type_kind.xmacro.h"
    Followup,
    Integer,
    String,
  };

  Component() = default;
  explicit constexpr Component(Category category, uint32_t value)
      : value_(value), category_(static_cast<uint8_t>(category)) {}

  friend Result NthSerialize(auto& s, Component const& c) {
    return s.write(nth::bytes(c));
  }
  friend Result NthDeserialize(auto& d, Component& c) {
    return d.read(nth::bytes(c));
  }

  [[nodiscard]] constexpr Category category() const {
    return static_cast<Category>(category_);
  }
  [[nodiscard]] constexpr uint32_t value() const { return value_; }

 private:
  // `value_` stores the width for categories that have variable-length widths
  // and an actual value for categories with a fixed-length width.
  uint32_t value_ : 24;
  uint32_t category_ : 8;
};

}  // namespace ic::internal_constants

#endif  // ICARUS_COMMON_INTERNAL_COMPONENT_H

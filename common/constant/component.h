#ifndef ICARUS_COMMON_CONSTANT_COMPONENT_H
#define ICARUS_COMMON_CONSTANT_COMPONENT_H

#include <cstdint>

#include "common/constant/category.h"
#include "common/result.h"
#include "nth/io/deserialize/deserialize.h"
#include "nth/io/serialize/serialize.h"
#include "nth/utility/bytes.h"

namespace ic {

struct ConstantComponent {
  ConstantComponent() = default;
  explicit constexpr ConstantComponent(ConstantCategory category, uint32_t value)
      : value_(value), category_(static_cast<uint8_t>(category)) {}

  friend Result NthSerialize(auto& s, ConstantComponent const& c) {
    return s.write(nth::bytes(c));
  }
  friend Result NthDeserialize(auto& d, ConstantComponent& c) {
    return d.read(nth::bytes(c));
  }

  [[nodiscard]] constexpr ConstantCategory category() const {
    return static_cast<ConstantCategory>(category_);
  }
  [[nodiscard]] constexpr uint32_t value() const { return value_; }

 private:
  // `value_` stores the width for categories that have variable-length widths
  // and an actual value for categories with a fixed-length width.
  uint32_t value_ : 24;
  uint32_t category_ : 8;
};

}  // namespace ic

#endif // ICARUS_COMMON_CONSTANT_COMPONENT_H

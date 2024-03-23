#ifndef ICARUS_COMMON_CONSTANT_ENTRY_H
#define ICARUS_COMMON_CONSTANT_ENTRY_H

#include <cstdint>

namespace ic {

struct Constant {
  constexpr uint32_t value() const { return value_; }

 private:
  friend struct ConstantManifest;
  constexpr Constant(uint32_t index) : value_(index) {}

  uint32_t value_;
};

}  // namespace ic

#endif  // ICARUS_COMMON_CONSTANT_ENTRY_H

#ifndef ICARUS_MODULE_MODULE_ID_H
#define ICARUS_MODULE_MODULE_ID_H

#include <cstdint>
#include <span>
#include <utility>

#include "jasmin/value.h"

namespace ic {

struct ModuleId {
  constexpr ModuleId() = default;

  static constexpr ModuleId Invalid() {
    return ModuleId(std::numeric_limits<uint32_t>::max());
  }
  static constexpr ModuleId Builtin() { return ModuleId(0); }

  friend constexpr bool operator==(ModuleId, ModuleId) = default;
  friend constexpr bool operator!=(ModuleId, ModuleId) = default;

  constexpr uint32_t value() const { return id_; }

  friend bool IcarusDeserializeValue(std::span<jasmin::Value const> values,
                                     ModuleId& id) {
    if (values.size() != 1) { return false; }
    id.id_ = values.front().as<uint32_t>();
    return true;
  }

  template <typename H>
  friend H AbslHashValue(H h, ModuleId m) {
    return H::combine(std::move(h), m.id_);
  }

 private:
  constexpr ModuleId(uint32_t id) : id_(id) {}

  uint32_t id_;
};

}  // namespace ic

#endif  // ICARUS_MODULE_MODULE_ID_H

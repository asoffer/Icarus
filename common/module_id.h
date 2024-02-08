#ifndef ICARUS_COMMON_MODULE_ID_H
#define ICARUS_COMMON_MODULE_ID_H

#include <cstdint>
#include <span>
#include <utility>

#include "jasmin/core/value.h"

namespace ic {

struct ModuleId {
  explicit constexpr ModuleId() = default;
  explicit constexpr ModuleId(uint32_t id) : id_(id) {}

  static constexpr ModuleId Invalid() {
    return ModuleId(std::numeric_limits<uint32_t>::max());
  }
  static constexpr ModuleId Builtin() { return ModuleId(0); }
  static constexpr ModuleId Foreign() {
    return ModuleId(std::numeric_limits<uint32_t>::max() - 2);
  }
  static constexpr ModuleId Current() {
    return ModuleId(std::numeric_limits<uint32_t>::max() - 1);
  }

  friend constexpr bool operator==(ModuleId, ModuleId) = default;

  constexpr uint32_t value() const { return id_; }

  friend void NthPrint(auto &p, auto &f, ModuleId id) {
    if (id == ModuleId::Builtin()) {
      p.write("builtin");
    } else if (id == ModuleId::Foreign()) {
      p.write("foreign");
    } else if (id == ModuleId::Current()) {
      p.write("current");
    } else {
      f(p, id.value());
    }
  }

  friend bool NthSerialize(auto &, ModuleId) {
    NTH_UNIMPLEMENTED();
    return true;
  }

  friend bool NthDeserialize(auto &, ModuleId &) {
    NTH_UNIMPLEMENTED();
    return true;
  }

  friend bool IcarusDeserializeValue(std::span<jasmin::Value const> values,
                                     ModuleId& id) {
    // TODO: Deal with the fact that you write these things multiple times.
    if (values.size() < 1) { return false; }
    id.id_ = values.front().raw_value();
    return true;
  }

  template <typename H>
  friend H AbslHashValue(H h, ModuleId m) {
    return H::combine(std::move(h), m.id_);
  }

 private:
  uint32_t id_;
};

}  // namespace ic

#endif  // ICARUS_COMMON_MODULE_ID_H

#ifndef ICARUS_IR_FUNCTION_ID_H
#define ICARUS_IR_FUNCTION_ID_H

#include <cstdint>
#include <utility>

#include "ir/module_id.h"
#include "nth/strings/interpolate.h"

namespace ic {

struct LocalFunctionId {
  explicit constexpr LocalFunctionId() = default;
  explicit constexpr LocalFunctionId(uint32_t id) : id_(id) {}

  static constexpr LocalFunctionId Invalid() {
    return LocalFunctionId(std::numeric_limits<uint32_t>::max());
  }

  friend constexpr bool operator==(LocalFunctionId, LocalFunctionId) = default;
  friend constexpr bool operator!=(LocalFunctionId, LocalFunctionId) = default;

  constexpr uint32_t value() const { return id_; }

  friend void NthPrint(auto &p, auto &f, LocalFunctionId id) {
    p.write("local-fn.{}");
    f(p, id.id_);
  }

  template <typename H>
  friend H AbslHashValue(H h, LocalFunctionId m) {
    return H::combine(std::move(h), m.id_);
  }

 private:
  uint32_t id_;
};

struct FunctionId {
  explicit constexpr FunctionId() = default;
  explicit constexpr FunctionId(ModuleId m, LocalFunctionId f)
      : id_((static_cast<uint64_t>(m.value()) << 32) | f.value()) {}

  static constexpr FunctionId Invalid() {
    return FunctionId(std::numeric_limits<uint32_t>::max());
  }

  friend constexpr bool operator==(FunctionId, FunctionId) = default;
  friend constexpr bool operator!=(FunctionId, FunctionId) = default;

  ModuleId module() const { return ModuleId(id_ >> 32); }
  LocalFunctionId local_function() const {
    return LocalFunctionId(static_cast<uint32_t>(id_));
  }

  friend void NthPrint(auto &p, auto &f, FunctionId id) {
    if (id.module() == ModuleId::Builtin()) {
      nth::Interpolate<"fn.builtin.{}">(p, f, id.local_function().value());
    } else if (id.module() == ModuleId::Current()) {
      nth::Interpolate<"fn.current.{}">(p, f, id.local_function().value());
    } else {
      nth::Interpolate<"fn.{}.{}">(p, f, id.module().value(),
                                   id.local_function().value());
    }
  }

  constexpr uint64_t value() const { return id_; }

  template <typename H>
  friend H AbslHashValue(H h, FunctionId m) {
    return H::combine(std::move(h), m.id_);
  }

 private:
  constexpr FunctionId(uint64_t id) : id_(id) {}

  uint64_t id_;
};

}  // namespace ic

#endif  // ICARUS_IR_FUNCTION_ID_H

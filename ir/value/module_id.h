#ifndef ICARUS_IR_VALUE_MODULE_ID_H
#define ICARUS_IR_VALUE_MODULE_ID_H

#include <cstddef>
#include <memory>

#include "base/extend.h"
#include "base/extend/absl_format.h"
#include "base/extend/absl_hash.h"

namespace ir {
namespace internal_module {

inline constinit std::atomic<size_t> generator;

}  // namespace internal_module

// A value-type representing a module (unit of compilation).
struct ModuleId : base::Extend<ModuleId, 1>::With<base::AbslHashExtension,
                                                  base::AbslFormatExtension> {
  static constexpr std::string_view kAbslFormatString = "ModuleId(%u)";

  constexpr ModuleId() : id_(std::numeric_limits<size_t>::max()) {}
  constexpr explicit ModuleId(size_t n) : id_(n) {}

  static constexpr ModuleId Invalid() {
    return ModuleId(std::numeric_limits<size_t>::max());
  }

  static ModuleId New() {
    return ModuleId(
        internal_module::generator.fetch_add(1, std::memory_order_relaxed));
  }

 private:
  friend base::EnableExtensions;

  size_t id_;
};

}  // namespace ir

#endif  // ICARUS_IR_VALUE_MODULE_ID_H

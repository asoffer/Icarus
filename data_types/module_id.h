#ifndef ICARUS_DATA_TYPES_MODULE_ID_H
#define ICARUS_DATA_TYPES_MODULE_ID_H

#include <string_view>

#include "base/extend.h"
#include "base/extend/absl_format.h"
#include "base/extend/absl_hash.h"

namespace data_types {

// A value-type representing a module (unit of compilation).
struct ModuleId : base::Extend<ModuleId, 1>::With<base::AbslHashExtension,
                                                  base::AbslFormatExtension> {
  using underlying_type = uint32_t;

  static constexpr std::string_view kAbslFormatString = "ModuleId(%u)";

  constexpr ModuleId() : id_(std::numeric_limits<underlying_type>::max()) {}
  constexpr explicit ModuleId(underlying_type n) : id_(n) {}

  // An identifier for the module which holds all builtin data accessible
  // through the `builtin` keyword.
  static constexpr ModuleId Builtin() { return ModuleId(0); }

  // An identifier for a synthetic module where all foreign symbols live.
  static constexpr ModuleId Foreign() {
    return ModuleId(std::numeric_limits<underlying_type>::max() - 1);
  }

  // No module has this identifier.
  static constexpr ModuleId Invalid() { return ModuleId(); }

  underlying_type value() const { return id_; }

 private:
  friend base::EnableExtensions;

  underlying_type id_;
};

}  // namespace data_types

#endif  // ICARUS_DATA_TYPES_MODULE_ID_H

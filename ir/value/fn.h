#ifndef ICARUS_IR_VALUE_FN_H
#define ICARUS_IR_VALUE_FN_H

#include <cstring>
#include <ostream>

#include "base/extend.h"
#include "base/extend/absl_format.h"
#include "base/extend/absl_hash.h"
#include "base/extend/serialize.h"
#include "ir/value/module_id.h"

namespace ir {

// An identifier usable to find the byte code for a function within a given
// (implicit module).
struct LocalFnId
    : base::Extend<LocalFnId, 1>::With<base::AbslHashExtension,
                                       base::AbslFormatExtension,
                                       base::BaseSerializeExtension> {
  using underlying_type = uint32_t;

  static constexpr std::string_view kAbslFormatString = "LocalFnId(%u)";

  constexpr LocalFnId() : id_(std::numeric_limits<underlying_type>::max()) {}
  constexpr explicit LocalFnId(underlying_type n) : id_(n) {}

  // No module has this identifier.
  static constexpr LocalFnId Invalid() { return LocalFnId(); }

  underlying_type value() const { return id_; }

 private:
  friend base::EnableExtensions;

  underlying_type id_;
};

// An identifier usable to find the byte code for a function within an entire
// program.
struct Fn : base::Extend<Fn, 2>::With<base::AbslHashExtension,
                                      base::BaseSerializeExtension> {
  Fn() : Fn(ModuleId::Invalid(), LocalFnId::Invalid()) {}
  explicit Fn(ModuleId mod, LocalFnId fn) : module_id_(mod), function_id_(fn) {}

  enum class Kind { Native, Foreign };
  constexpr Kind kind() const {
    return module() == ModuleId::Foreign() ? Kind::Foreign : Kind::Native;
  }

  constexpr ModuleId module() const { return module_id_; }
  constexpr LocalFnId local() const { return function_id_; }

  friend std::ostream &operator<<(std::ostream &os, Fn f) {
    return os << "Fn(" << f.module_id_.value() << "." << f.function_id_.value()
              << ")";
  }

 private:
  friend base::EnableExtensions;

  ModuleId module_id_;
  LocalFnId function_id_;
};

}  // namespace ir

#endif  // ICARUS_IR_VALUE_FN_H

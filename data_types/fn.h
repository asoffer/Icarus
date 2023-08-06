#ifndef ICARUS_DATA_TYPES_FN_H
#define ICARUS_DATA_TYPES_FN_H

#include <cstring>
#include <ostream>

#include "base/extend.h"
#include "base/extend/absl_format.h"
#include "base/extend/absl_hash.h"
#include "module/unique_id.h"
#include "serialization/function_index.h"

namespace data_types {

// An identifier usable to find the byte code for a function within a given
// (implicit module).
using LocalFnId = serialization::FunctionIndex;

// An identifier usable to find the byte code for a function within an entire
// program.
struct Fn : base::Extend<Fn, 2>::With<base::AbslHashExtension> {
  Fn() : Fn(module::UniqueId::Invalid(), LocalFnId::Invalid()) {}
  explicit Fn(module::UniqueId mod, LocalFnId fn)
      : module_id_(mod), function_id_(fn) {}

  module::UniqueId module() const { return module_id_; }
  constexpr LocalFnId local() const { return function_id_; }

  friend std::ostream &operator<<(std::ostream &os, Fn f) {
    return os << "Fn(" << f.module_id_.value() << "." << f.function_id_ << ")";
  }

 private:
  friend base::EnableExtensions;

  module::UniqueId module_id_;
  LocalFnId function_id_;
};

}  // namespace data_types

#endif  // ICARUS_DATA_TYPES_FN_H

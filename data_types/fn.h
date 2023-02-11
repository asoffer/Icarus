#ifndef ICARUS_DATA_TYPES_FN_H
#define ICARUS_DATA_TYPES_FN_H

#include <cstring>
#include <ostream>

#include "base/extend.h"
#include "base/extend/absl_format.h"
#include "base/extend/absl_hash.h"
#include "serialization/function_index.h"
#include "serialization/module_index.h"

namespace data_types {

// An identifier usable to find the byte code for a function within a given
// (implicit module).
using LocalFnId = serialization::FunctionIndex;

// An identifier usable to find the byte code for a function within an entire
// program.
struct Fn : base::Extend<Fn, 2>::With<base::AbslHashExtension> {
  Fn() : Fn(serialization::ModuleIndex::Invalid(), LocalFnId::Invalid()) {}
  explicit Fn(serialization::ModuleIndex mod, LocalFnId fn)
      : module_index_(mod), function_id_(fn) {}

  constexpr serialization::ModuleIndex module() const { return module_index_; }
  constexpr LocalFnId local() const { return function_id_; }

  friend std::ostream &operator<<(std::ostream &os, Fn f) {
    return os << "Fn(" << f.module_index_.value() << "." << f.function_id_
              << ")";
  }

 private:
  friend base::EnableExtensions;

  serialization::ModuleIndex module_index_;
  LocalFnId function_id_;
};

}  // namespace data_types

#endif  // ICARUS_DATA_TYPES_FN_H

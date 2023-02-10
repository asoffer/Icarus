#ifndef ICARUS_DATA_TYPES_FN_H
#define ICARUS_DATA_TYPES_FN_H

#include <cstring>
#include <ostream>

#include "base/extend.h"
#include "base/extend/absl_format.h"
#include "base/extend/absl_hash.h"
#include "serialization/module_index.h"

namespace data_types {

// An identifier usable to find the byte code for a function within a given
// (implicit module).
struct LocalFnId {
  using underlying_type = uint32_t;

 public:
  constexpr explicit LocalFnId() = default;

  explicit LocalFnId(underlying_type n) : id_(n) {
    ASSERT(n < ((underlying_type{1} << IdWidth) - 1));
  }

  static LocalFnId Foreign(underlying_type n) {
    LocalFnId l(n);
    l.foreign_ = 1;
    return l;
  }

  friend bool operator==(LocalFnId, LocalFnId) = default;
  friend bool operator!=(LocalFnId, LocalFnId) = default;

  template <typename H>
  friend H AbslHashValue(H h, LocalFnId id) {
    return H::combine(std::move(h),
                      (underlying_type{id.foreign_} << IdWidth) | id.id_);
  }

  friend std::ostream &operator<<(std::ostream &os, LocalFnId l) {
    if (l == Invalid()) {
      return os << "invalid";
    } else if (l.foreign()) {
      return os << "foreign." << l.value();
    } else {
      return os << "local." << l.value();
    }
  }

  // No module has this identifier.
  static constexpr LocalFnId Invalid() { return LocalFnId(); }

  underlying_type value() const { return id_; }
  bool foreign() const { return foreign_; }

 private:
  static constexpr size_t IdWidth = sizeof(underlying_type) * CHAR_BIT - 1;
  friend base::EnableExtensions;
  underlying_type foreign_ : 1  = 0;
  underlying_type id_ : IdWidth = (underlying_type{1} << IdWidth) - 1;
};

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

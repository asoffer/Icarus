#ifndef ICARUS_IR_VALUE_MODULE_ID_H
#define ICARUS_IR_VALUE_MODULE_ID_H

#include <cstddef>

#include "frontend/source/file_name.h"

namespace ir {

// A value-type representing a module (unit of compilation).
struct ModuleId {
  static ModuleId FromFile(frontend::CanonicalFileName const& filename);

  frontend::CanonicalFileName const& filename() const;

  template <typename H>
  friend H AbslHashValue(H h, ModuleId m) {
    return H::combine(std::move(h), m.id_);
  }

  friend constexpr bool operator==(ModuleId lhs, ModuleId rhs) {
    return lhs.id_ == rhs.id_;
  }

  friend constexpr bool operator!=(ModuleId lhs, ModuleId rhs) {
    return not(lhs == rhs);
  }

  friend std::ostream& operator<<(std::ostream& os, ModuleId m) {
    return os << "ModuleId(" << m.id_ << ")";
  }

 private:
  constexpr explicit ModuleId(size_t n) : id_(n) {}

  size_t id_;
};

}  // namespace ir

#endif  // ICARUS_IR_VALUE_MODULE_ID_H

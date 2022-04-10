#ifndef ICARUS_IR_VALUE_FN_H
#define ICARUS_IR_VALUE_FN_H

#include <cstring>
#include <ostream>

#include "base/extend.h"
#include "base/extend/absl_hash.h"
#include "ir/value/module_id.h"

namespace ir {

struct Fn : base::Extend<Fn, 2>::With<base::AbslHashExtension> {
  Fn() : Fn(ModuleId::Invalid(), std::numeric_limits<uint32_t>::max()) {}
  explicit Fn(ModuleId mod, uint32_t fn) : module_id_(mod), function_id_(fn) {}

  enum class Kind { Native, Foreign };
  constexpr Kind kind() const {
    return module() == ir::ModuleId::Builtin() ? Kind::Foreign : Kind::Native;
  }

  constexpr ModuleId module() const { return module_id_; }
  constexpr uint32_t index() const { return function_id_; }

  friend std::ostream &operator<<(std::ostream &os, Fn f) {
    return os << "Fn(" << f.module_id_ << ", " << f.function_id_ << ")";
  }

 private:
  friend base::EnableExtensions;

  ir::ModuleId module_id_;
  uint32_t function_id_;
};

}  // namespace ir

#endif  // ICARUS_IR_VALUE_FN_H

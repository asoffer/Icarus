#ifndef ICARUS_IR_VALUE_SCOPE_H
#define ICARUS_IR_VALUE_SCOPE_H

#include "base/extend.h"
#include "base/extend/absl_format.h"
#include "base/extend/absl_hash.h"

namespace ir {
struct CompiledScope;

struct Scope : base::Extend<Scope, 1>::With<base::AbslFormatExtension,
                                            base::AbslHashExtension> {
  static constexpr std::string_view kAbslFormatString = "Scope(%p)";

  constexpr Scope() : Scope(nullptr) {}
  explicit constexpr Scope(CompiledScope *scope) : scope_(scope) {}

 private:
  friend CompiledScope;
  friend base::EnableExtensions;

  CompiledScope *scope_;
};

}  // namespace ir

#endif  // ICARUS_IR_VALUE_SCOPE_H

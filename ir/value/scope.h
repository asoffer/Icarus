#ifndef ICARUS_IR_VALUE_SCOPE_H
#define ICARUS_IR_VALUE_SCOPE_H

#include "base/extend.h"
#include "base/extend/absl_format.h"
#include "base/extend/absl_hash.h"
#include "ir/blocks/group.h"
#include "type/scope.h"

namespace ir {

using CompiledScope = BlockGroup<type::Scope>;

struct Scope : base::Extend<Scope, 1>::With<base::AbslFormatExtension,
                                            base::AbslHashExtension> {
  static constexpr std::string_view kAbslFormatString = "Scope(%p)";

  struct Data {
    CompiledScope *scope;
    type::Scope const *type;
    base::untyped_buffer::const_iterator byte_code;
  };

  constexpr Scope() : Scope(nullptr) {}
  explicit constexpr Scope(Scope::Data const *data) : data_(data) {}

  CompiledScope *operator->() { return data_->scope; }
  CompiledScope &operator*() { return *data_->scope; }

 private:
  friend CompiledScope;
  friend base::EnableExtensions;

  Data const *data_;
};

}  // namespace ir

#endif  // ICARUS_IR_VALUE_SCOPE_H

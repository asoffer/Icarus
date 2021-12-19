#ifndef ICARUS_IR_VALUE_SCOPE_H
#define ICARUS_IR_VALUE_SCOPE_H

#include <string>
#include <vector>

#include "absl/types/span.h"
#include "base/any_invocable.h"
#include "base/extend.h"
#include "base/extend/absl_format.h"
#include "base/extend/absl_hash.h"
#include "base/extend/serialize.h"
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

  type::Scope const *type() const { return data_->type; }

 private:
  friend CompiledScope;
  friend base::EnableExtensions;

  Data const *data_;
};

struct ScopeContext
    : base::Extend<ScopeContext, 1>::With<base::BaseSerializeExtension> {
  explicit ScopeContext(std::vector<std::string> block_names)
      : block_names_(std::move(block_names)) {}

  absl::Span<std::string const> blocks() const { return block_names_; }

 private:
  friend base::EnableExtensions;
  std::vector<std::string> block_names_;
};

struct UnboundScope
    : base::Extend<UnboundScope, 1>::With<base::AbslFormatExtension,
                                          base::AbslHashExtension> {
  static constexpr std::string_view kAbslFormatString = "UnboundScope(%p)";

  explicit UnboundScope(
      base::any_invocable<std::optional<Scope>(ScopeContext const &)> *f =
          nullptr)
      : f_(f) {}

  std::optional<Scope> operator()(ScopeContext const &ctx) const {
    return (*f_)(ctx);
  }

 private:
  friend base::EnableExtensions;

  base::any_invocable<std::optional<Scope>(ScopeContext const &)> *f_;
};

}  // namespace ir

#endif  // ICARUS_IR_VALUE_SCOPE_H

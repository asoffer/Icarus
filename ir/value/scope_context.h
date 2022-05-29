#ifndef ICARUS_IR_VALUE_SCOPE_CONTEXT_H
#define ICARUS_IR_VALUE_SCOPE_CONTEXT_H

#include <string>
#include <vector>

#include "absl/types/span.h"
#include "base/any_invocable.h"
#include "base/extend.h"
#include "base/extend/absl_format.h"
#include "base/extend/absl_hash.h"
#include "base/extend/serialize.h"
#include "base/ptr_union.h"
#include "compiler/work_resources.h"
#include "core/parameters.h"
#include "ir/subroutine.h"
#include "ir/value/block.h"
#include "ir/value/reg.h"
#include "ir/value/result_buffer.h"
#include "ir/value/scope.h"
#include "type/block.h"
#include "type/generic.h"
#include "type/scope.h"

namespace ir {

struct ScopeContext
    : base::Extend<ScopeContext, 1>::With<base::BaseSerializeExtension> {
  // TODO: Rather than storing the parameters, we should store the actual
  // block-type, or, if it's generic, the generic that could be used to
  // instantiate a block-type.
  struct block_type : base::Extend<block_type>::With<base::AbslHashExtension> {
    std::string_view name;
    base::PtrUnion<type::Block const, type::Generic<type::Block> const> type;
    ast::BlockNode const *node;
  };

  ScopeContext() : block_names_(nullptr) {}
  explicit ScopeContext(std::vector<block_type> const *block_names)
      : block_names_(ASSERT_NOT_NULL(block_names)) {}

  absl::Span<block_type const> blocks() const {
    return *ASSERT_NOT_NULL(block_names_);
  }

  size_t size() const { return ASSERT_NOT_NULL(block_names_)->size(); }

  Block find(std::string_view name) const {
    absl::Span block_names = blocks();
    for (size_t i = 0; i < block_names.size(); ++i) {
      if (block_names[i].name == name) { return Block(i); }
    }
    return Block::Invalid();
  }

  auto const &operator[](Block b) const {
    ASSERT(b != Block::Invalid());
    ASSERT(b.value() < block_names_->size());
    return blocks()[b.value()];
  }

  friend bool operator==(ScopeContext const &lhs, ScopeContext const &rhs) {
    return *ASSERT_NOT_NULL(lhs.block_names_) ==
           *ASSERT_NOT_NULL(rhs.block_names_);
  }

  friend bool operator!=(ScopeContext const &lhs, ScopeContext const &rhs) {
    return not(lhs == rhs);
  }

  void const *data() { return block_names_; }

  friend std::ostream &operator<<(std::ostream &os, ScopeContext ctx) {
    os << "ScopeContext(";
    if (not ctx.block_names_) { return os << "null)"; }
    std::string_view separator = "";
    for (auto const &b : ctx.blocks()) {
      os << std::exchange(separator, ", ") << b.name;
    }
    return os << ")";
  }

 private:
  friend base::EnableExtensions;
  std::vector<block_type> const *block_names_;
};

struct UnboundScope
    : base::Extend<UnboundScope, 1>::With<base::AbslFormatExtension,
                                          base::AbslHashExtension,
                                          base::BaseSerializeExtension> {
  static constexpr std::string_view kAbslFormatString = "UnboundScope(%p)";

  struct Data {
    // TODO: Storing the literal here is in general a bad idea, but helps us
    // move forward with generics as a short-term solution. Long term, we need
    // tohave a better solution.
    ast::ScopeLiteral const *literal;

    base::any_invocable<Scope(
        compiler::WorkResources const &, ScopeContext const &,
        core::Arguments<type::Typed<CompleteResultRef>> const &)>
        f;
  };

  explicit UnboundScope(Data *data = nullptr) : data_(data) {}

  Scope bind(
      compiler::WorkResources const &wr, ScopeContext const &ctx,
      core::Arguments<type::Typed<CompleteResultRef>> const &arguments) const {
    ASSERT(static_cast<bool>(data_->f) == true);
    return data_->f(wr, ctx, arguments);
  }

  ast::ScopeLiteral const *literal() const { return data_->literal; }

 private:
  friend base::EnableExtensions;

  Data *data_;
};

}  // namespace ir

#endif  // ICARUS_IR_VALUE_SCOPE_CONTEXT_H

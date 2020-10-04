#ifndef ICARUS_IR_COMPILED_SCOPE_H
#define ICARUS_IR_COMPILED_SCOPE_H

#include <string_view>
#include <utility>

#include "absl/container/flat_hash_set.h"
#include "absl/container/flat_hash_map.h"
#include "ir/value/block.h"
#include "ir/value/jump.h"
#include "ir/value/overload_set.h"
#include "type/type.h"

namespace ir {

struct CompiledScope {
  static constexpr CompiledScope *From(Scope s) { return s.scope_; }

  explicit CompiledScope(type::Type const *state = nullptr) : state_(state) {}

  // TODO: I'm not sure we need two-step initialization here.
  void Initialize(absl::flat_hash_set<Jump> enter, OverloadSet exit,
                  absl::flat_hash_map<std::string_view, Block> blocks) {
    enter_  = std::move(enter);
    exit_   = std::move(exit);
    blocks_ = std::move(blocks);
  }

  type::Type const *state_type() const { return state_; }

 private:
  type::Type const *state_ = nullptr;
  // Entries in this map are created from declarations in a `scope { ... }`
  // node. Because a block is only added here if it is associated with a
  // declaration, we can reuse the declaration's `id()` field which provides a
  // stable string_view on which to key the block values.
  absl::flat_hash_map<std::string_view, Block> blocks_;
  absl::flat_hash_set<Jump> enter_;
  OverloadSet exit_;
};

}  // namespace ir

#endif  // ICARUS_IR_COMPILED_SCOPE_H

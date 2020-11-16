#ifndef ICARUS_IR_COMPILED_SCOPE_H
#define ICARUS_IR_COMPILED_SCOPE_H

#include <optional>
#include <string_view>
#include <utility>

#include "absl/container/flat_hash_map.h"
#include "absl/container/flat_hash_set.h"
#include "ir/value/block.h"
#include "ir/value/jump.h"
#include "ir/value/overload_set.h"
#include "type/type.h"

namespace ir {

struct CompiledScope {
  static constexpr CompiledScope *From(Scope s) { return s.scope_; }

  explicit CompiledScope(type::Type state = nullptr) : state_(state) {}

  // TODO: I'm not sure we need two-step initialization here.
  void Initialize(absl::flat_hash_set<Jump> init, OverloadSet exit,
                  absl::flat_hash_map<std::string_view, Block> blocks) {
    init_   = std::move(init);
    exit_   = std::move(exit);
    blocks_ = std::move(blocks);
  }

  Block block(std::string_view name) const { return blocks_.at(name); }

  type::Type state_type() const { return state_; }

  absl::flat_hash_set<Jump> const &inits() const { return init_; }
  OverloadSet const &exit() const { return exit_; }
  OverloadSet &exit() { return exit_; }

 private:
  type::Type state_ = nullptr;
  // Entries in this map are created from declarations in a `scope { ... }`
  // node. Because a block is only added here if it is associated with a
  // declaration, we can reuse the declaration's `id()` field which provides a
  // stable string_view on which to key the block values.
  absl::flat_hash_map<std::string_view, Block> blocks_;
  absl::flat_hash_set<Jump> init_;
  OverloadSet exit_;
};

}  // namespace ir

#endif  // ICARUS_IR_COMPILED_SCOPE_H

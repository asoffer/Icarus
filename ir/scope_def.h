#ifndef ICARUS_IR_SCOPE_DEF_H
#define ICARUS_IR_SCOPE_DEF_H

#include <string_view>

#include "core/scope.h"
#include "ir/any_func.h"
#include "ir/block.h"

struct Module;

namespace ir {
// TODO Calls to EvaluateAs should probably take this as const, so we can be
// sure no one modifies blocks_ and invalidates pointers.
struct ScopeDef {
  explicit ScopeDef(Module const *mod): mod_(mod) {}

  void AddInit(AnyFunc f) { inits_.push_back(f); }
  void AddDone(AnyFunc f) { dones_.push_back(f); }
  void AddBlockDef(std::string_view name, BlockDef &&block) {
    blocks_.emplace(name, std::move(block));
  }

  Module const *module() const { return mod_; }

  Module const *mod_ = nullptr;
  std::vector<AnyFunc> inits_, dones_;
  absl::flat_hash_map<std::string_view, BlockDef> blocks_;
  std::function<void()> *work_item = nullptr;
};
}  // namespace ir

#endif  // ICARUS_IR_SCOPE_DEF_H

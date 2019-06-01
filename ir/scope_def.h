#ifndef ICARUS_IR_SCOPE_DEF_H
#define ICARUS_IR_SCOPE_DEF_H

#include <string_view>

#include "core/scope.h"
#include "ir/any_func.h"
#include "ir/block.h"

struct Module;

namespace ir {
struct ScopeDef {
  explicit ScopeDef(Module const *mod): mod_(mod) {}

  void AddInit(AnyFunc f) { inits_.push_back(f); }
  void AddDone(AnyFunc f) { dones_.push_back(f); }
  void AddBlockDef(std::string_view name, BlockDef block_def) {
    blocks_.emplace(name, block_def);
  }

  Module const *module() const { return mod_; }

  Module const *mod_ = nullptr;
  std::vector<AnyFunc> inits_, dones_;
  absl::flat_hash_map<std::string_view, BlockDef> blocks_;
};
}  // namespace ir

#endif  // ICARUS_IR_SCOPE_DEF_H

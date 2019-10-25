#ifndef ICARUS_IR_SCOPE_DEF_H
#define ICARUS_IR_SCOPE_DEF_H

#include <string_view>

#include "ast/scope/scope.h"
#include "base/move_func.h"
#include "ir/any_func.h"
#include "ir/block_def.h"
#include "module/module.h"

namespace ir {
struct JumpHandler;

// TODO Calls to EvaluateAs should probably take this as const, so we can be
// sure no one modifies blocks_ and invalidates pointers.
struct ScopeDef {
  explicit ScopeDef(module::BasicModule const *mod,
                    std::vector<JumpHandler const *> inits,
                    std::vector<AnyFunc> dones,
                    absl::flat_hash_map<std::string_view, BlockDef *> blocks)
      : mod_(mod),
        inits_(std::move(inits)),
        dones_(std::move(dones)),
        blocks_(std::move(blocks)) {}

  module::BasicModule const *module() const { return mod_; }

  module::BasicModule const *mod_ = nullptr;
  std::vector<JumpHandler const *> inits_;
  std::vector<AnyFunc> dones_;
  absl::flat_hash_map<std::string_view, BlockDef *> blocks_;
  base::move_func<void()> *work_item = nullptr;
};
}  // namespace ir

#endif  // ICARUS_IR_SCOPE_DEF_H

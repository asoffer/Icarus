#ifndef ICARUS_IR_SCOPE_DEF_H
#define ICARUS_IR_SCOPE_DEF_H

#include <memory>
#include <string_view>
#include <vector>

#include "ast/scope/scope.h"
#include "base/move_func.h"
#include "base/no_destructor.h"
#include "ir/block_def.h"
#include "ir/value/fn.h"
#include "ir/value/overload_set.h"
#include "module/module.h"
#include "type/type.h"

namespace ir {
struct Jump;

inline CompiledFn &TrivialFunction() {
  static base::NoDestructor<CompiledFn> f = [] {
    CompiledFn f(type::Func({}, {}),
                 core::Params<type::Typed<ast::Declaration const *>>{});
    f.entry()->set_jump(JumpCmd::Return());
    f.WriteByteCode();
    return f;
  }();
  return *f;
}

// TODO Calls to EvaluateAs should probably take this as const, so we can be
// sure no one modifies blocks_ and invalidates pointers.
struct ScopeDef {
  explicit ScopeDef(module::BasicModule const *mod,
                    type::Type const *state_type = nullptr)
      : mod_(mod),
        state_type_(state_type),
        start_(std::make_unique<BlockDef>()),
        exit_(std::make_unique<BlockDef>()) {
    blocks_.emplace("start", start_.get());
    blocks_.emplace("exit", exit_.get());
    start_->before_ = OverloadSet({Fn(&TrivialFunction())});
  }

  module::BasicModule const *module() const { return mod_; }

  // TODO This is const-incorrect. We need it to be mutable. Probably have two
  // overloads here.
  BlockDef *block(std::string_view name) const {
    if (auto iter = blocks_.find(name); iter != blocks_.end()) {
      return iter->second;
    }
    return nullptr;
  }

  module::BasicModule const *mod_ = nullptr;
  type::Type const * state_type_;
  absl::flat_hash_map<std::string_view, BlockDef *> blocks_;
  // TODO figure out move/lifetime for this so we don't need separate
  // allocations.
  std::unique_ptr<BlockDef> start_;
  std::unique_ptr<BlockDef> exit_;
  base::move_func<void()> *work_item = nullptr;
};
}  // namespace ir

#endif  // ICARUS_IR_SCOPE_DEF_H

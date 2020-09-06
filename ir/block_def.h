#ifndef ICARUS_IR_BLOCK_H
#define ICARUS_IR_BLOCK_H

#include <iostream>

#include "absl/container/flat_hash_set.h"
#include "base/debug.h"
#include "ir/value/overload_set.h"
#include "type/jump.h"

namespace ir {
struct Jump;

// TODO remove this. We don't need the instruction set at all here because the
// only content is the return.
struct Inst;
struct SuperGrossHack {
  static cmd_index_t Index(Inst const &inst) { return 0; }
};

inline CompiledFn &TrivialFunction() {
  static base::NoDestructor<CompiledFn> f = [] {
    CompiledFn f(type::Func({}, {}),
                 core::Params<type::Typed<ast::Declaration const *>>{});
    f.entry()->set_jump(JumpCmd::Return());
    f.WriteByteCode<SuperGrossHack>();
    return f;
  }();
  return *f;
}

struct BlockDef {
  BlockDef() = default;
  explicit BlockDef(absl::flat_hash_set<Jump const *> after)
      : before_({Fn(&TrivialFunction())}), after_(std::move(after)) {}
  inline friend std::ostream &operator<<(std::ostream &os, BlockDef const &b) {
    return os << "blockdef";
  }

  type::Jump const *type() const { return type_; }
  absl::flat_hash_set<Jump const *> const &after() const { return after_; }

  ir::OverloadSet before_;
 private:
  absl::flat_hash_set<Jump const *> after_;

  type::Jump const *type_ = nullptr;
};

}  // namespace ir

#endif  // ICARUS_IR_BLOCK_H

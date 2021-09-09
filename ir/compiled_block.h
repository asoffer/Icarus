#ifndef ICARUS_IR_COMPILED_BLOCK_H
#define ICARUS_IR_COMPILED_BLOCK_H

#include "absl/container/flat_hash_set.h"
#include "ast/ast.h"
#include "ir/instruction/op_codes.h"
#include "ir/instruction/set.h"
#include "ir/value/block.h"
#include "ir/value/jump.h"
#include "ir/value/overload_set.h"
#include "type/function.h"
#include "type/typed_value.h"

namespace ir {

inline NativeFn TrivialFunction() {
  // TODO: Avoid the delayed static here.
  static base::NoDestructor<base::untyped_buffer> byte_code = [] {
    base::untyped_buffer result;
    result.append(internal::kReturnInstruction);
    return result;
  }();
  static base::NoDestructor<NativeFn::Data> data = [] {
    auto fn_type = type::Func({}, {});
    auto *f      = new CompiledFn(fn_type);
    f->entry()->set_jump(JumpCmd::Return());
    return NativeFn::Data{
        .fn        = f,
        .type      = fn_type,
        .byte_code = byte_code->begin(),
    };
  }();
  return NativeFn(&*data);
}

struct CompiledBlock {
  static constexpr CompiledBlock *From(Block b) { return b.block_; }

  CompiledBlock() = default;
  explicit CompiledBlock(absl::flat_hash_set<Jump> after)
      : before_({Fn(TrivialFunction())}), after_(std::move(after)) {}
  explicit CompiledBlock(OverloadSet os, absl::flat_hash_set<Jump> after)
      : before_(std::move(os)), after_(std::move(after)) {}

  OverloadSet const &before() const { return before_; }
  OverloadSet &before() { return before_; }
  absl::flat_hash_set<Jump> const &after() const { return after_; }

 private:
  friend struct ScopeDef;

  OverloadSet before_;
  // TODO: `after_` should be an overload set of jumps.
  absl::flat_hash_set<Jump> after_;
};

}  // namespace ir

#endif  // ICARUS_IR_COMPILED_BLOCK_H

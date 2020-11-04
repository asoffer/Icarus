#ifndef ICARUS_AST_JUMP_OPTIONS_H
#define ICARUS_AST_JUMP_OPTIONS_H

#include <string>
#include <string_view>

#include "ast/expression.h"
#include "core/arguments.h"

namespace ast {
// A jump option is a collection of blocks that may be jumped to and the
// arguments to pass to such a block. When evaluating jump options, the option
// is chose if the collection of blocks refers to a block that is present on
// the scope node. In that case, the arguments are evaluated and passed to it.
// Otherwise, the option is discarded and the next option in the `options_`
// container is chosen.
struct JumpOption {
  explicit JumpOption(std::string name,
                      core::Arguments<std::unique_ptr<Expression>> a)
      : block_(std::move(name)), args_(std::move(a)) {}
  JumpOption(JumpOption const &)     = default;
  JumpOption(JumpOption &&) noexcept = default;
  JumpOption &operator=(JumpOption const &) = default;
  JumpOption &operator=(JumpOption &&) noexcept = default;

  std::string_view block() const { return block_; }
  core::Arguments<std::unique_ptr<Expression>> const &args() const {
    return args_;
  }

 private:
  friend struct UnconditionalGoto;
  friend struct ConditionalGoto;
  std::string block_;
  core::Arguments<std::unique_ptr<Expression>> args_;
};

}  // namespace ast

#endif  // ICARUS_AST_JUMP_OPTIONS_H

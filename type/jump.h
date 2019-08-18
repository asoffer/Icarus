#ifndef ICARUS_TYPE_JUMP_H
#define ICARUS_TYPE_JUMP_H

#include <vector>

#include "absl/types/span.h"
#include "type.h"
#include "function.h"

namespace type {
struct Jump : public Type {
  TYPE_FNS(Jump);
  Jump(std::vector<Type const *> ts) : args_(std::move(ts)) {}

#include "visitor/type_visitors.xmacro.h"

  absl::Span<type::Type const *const> args() const { return args_; }

  template <typename H>
  friend H AbslHashValue(H h, Jump const& j) {
    return H::combine(std::move(h), j.args_);
  }

  type::Function const *ToFunction() const { return type::Func(args_, {}); }

  friend bool operator==(Jump const &lhs, Jump const &rhs) {
    return lhs.args_ == rhs.args_;
  }

 private:
  std::vector<Type const *> args_;
};

Jump const *Jmp(std::vector<Type const *> const &args);
}  // namespace type
#endif  // ICARUS_TYPE_JUMP_H
#ifndef ICARUS_TYPE_JUMP_H
#define ICARUS_TYPE_JUMP_H

#include "absl/types/span.h"
#include "core/fn_params.h"
#include "function.h"
#include "type.h"

namespace type {
struct Jump : public Type {
  TYPE_FNS(Jump);
  Jump(core::FnParams<Type const *> const &ts) : args_(std::move(ts)) {}

  void Accept(VisitorBase *visitor, void *ret, void *arg_tuple) const override {
    visitor->ErasedVisit(this, ret, arg_tuple);
  }

  // TODO rename to params.
  core::FnParams<type::Type const *> const &args() const { return args_; }

  template <typename H>
  friend H AbslHashValue(H h, Jump const &j) {
    return H::combine(std::move(h), j.args_);
  }

  type::Function const *ToFunction() const { return type::Func(args_, {}); }

  bool is_big() const override { return false; }

  friend bool operator==(Jump const &lhs, Jump const &rhs) {
    return lhs.args_ == rhs.args_;
  }

 private:
  core::FnParams<Type const *> args_;
};

Jump const *Jmp(core::FnParams<Type const *> const &args);
}  // namespace type
#endif  // ICARUS_TYPE_JUMP_H

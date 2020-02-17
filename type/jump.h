#ifndef ICARUS_TYPE_JUMP_H
#define ICARUS_TYPE_JUMP_H

#include "absl/types/span.h"
#include "core/params.h"
#include "function.h"
#include "type.h"

namespace type {
struct Jump : public Type {
  TYPE_FNS(Jump);
  Jump(core::Params<Type const *> const &ts) : params_(std::move(ts)) {}

  void Accept(VisitorBase *visitor, void *ret, void *arg_tuple) const override {
    visitor->ErasedVisit(this, ret, arg_tuple);
  }

  // TODO rename to `params()`.
  core::Params<type::Type const *> const &args() const { return params_; }
  core::Params<type::Type const *> const &params() const { return params_; }

  template <typename H>
  friend H AbslHashValue(H h, Jump const &j) {
    return H::combine(std::move(h), j.params_);
  }

  type::Function const *ToFunction() const { return type::Func(params_, {}); }

  bool is_big() const override { return false; }

  friend bool operator==(Jump const &lhs, Jump const &rhs) {
    return lhs.params_ == rhs.params_;
  }

 private:
  core::Params<Type const *> params_;
};

Jump const *Jmp(core::Params<Type const *> const &args);
}  // namespace type
#endif  // ICARUS_TYPE_JUMP_H

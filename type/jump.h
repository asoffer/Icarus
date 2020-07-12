#ifndef ICARUS_TYPE_JUMP_H
#define ICARUS_TYPE_JUMP_H

#include "absl/types/span.h"
#include "core/params.h"
#include "type.h"

namespace type {

// `Jump` is a type representing something callable which is similar to a
// function. However unlike a function, a jump does return to the calling
// context. `Jump`s are one of the primary building-block types for user-defined
// scopes.
struct Jump : Type {
  friend Jump const *Jmp(type::Type const *state,
                         core::Params<Type const *> const &params);

  ~Jump() override {}

  void Accept(VisitorBase *visitor, void *ret, void *arg_tuple) const override {
    visitor->ErasedVisit(this, ret, arg_tuple);
  }

  void WriteTo(std::string *buf) const override;
  core::Bytes bytes(core::Arch const &arch) const override;
  core::Alignment alignment(core::Arch const &arch) const override;

  bool is_big() const override { return false; }

  Completeness completeness() const override { return Completeness::Complete; }

  // Returns the parameters with which a a value of this type can be called.
  core::Params<type::Type const *> const &params() const { return params_; }

  // Returns the state type, if this jump is stateful, or a null-pointer
  // otherwise.
  type::Type const *state() const { return state_; }

  template <typename H>
  friend H AbslHashValue(H h, Jump const &j) {
    return H::combine(std::move(h), j.state_, j.params_);
  }

  friend bool operator==(Jump const &lhs, Jump const &rhs) {
    return lhs.state_ == rhs.state_ and lhs.params_ == rhs.params_;
  }

  friend bool operator!=(Jump const &lhs, Jump const &rhs) {
    return not(lhs == rhs);
  }

 private:
  Jump(type::Type const *state, core::Params<Type const *> const &ts)
      : Type(Type::Flags{.is_default_initializable = 0,
                         .is_copyable              = 0,
                         .is_movable               = 0,
                         .has_destructor           = 0}),
        state_(state),
        params_(std::move(ts)) {}

  type::Type const *state_;
  core::Params<Type const *> params_;
};

// Constructs a jump type with the state and parameters, or retrieves an
// equivalent one from the cachec if necessary.
Jump const *Jmp(type::Type const *state,
                core::Params<Type const *> const &params);

}  // namespace type
#endif  // ICARUS_TYPE_JUMP_H

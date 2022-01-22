#ifndef ICARUS_TYPE_GENERIC_H
#define ICARUS_TYPE_GENERIC_H

#include <string>
#include <vector>

#include "base/any_invocable.h"
#include "compiler/work_resources.h"
#include "core/arch.h"
#include "core/arguments.h"
#include "type/type.h"
#include "type/type_fwd.h"
#include "type/typed_value.h"

namespace type {

template <typename, typename>
struct Generic;

template <typename T>
struct InstantiatedGeneric : T {
  using instantiation_type = T;

  template <typename... Args>
  InstantiatedGeneric(Generic<instantiation_type> const *g, Args &&... args)
      : T(std::forward<Args>(args)...), generic_(ASSERT_NOT_NULL(g)) {}

  Generic<instantiation_type> const &generic() const { return *generic_; }

  void set_arguments(core::Arguments<Typed<ir::CompleteResultBuffer>> args) {
    arguments_ = std::move(args);
  }

  // Returns the arguments used to instantiate this type.
  core::Arguments<Typed<ir::CompleteResultBuffer>> const &arguments() const {
    return arguments_;
  }

 private:
  Generic<instantiation_type> const *generic_;
  core::Arguments<Typed<ir::CompleteResultBuffer>> arguments_;
};

template <typename T, typename InstantiationType>
struct Generic : LegacyType {
  using type               = T;
  using instantiation_type = InstantiationType;

  explicit Generic()
      : LegacyType(IndexOf<Generic<T, InstantiationType>>(),
                   LegacyType::Flags{.is_default_initializable = 0,
                                     .is_copyable              = 1,
                                     .is_movable               = 1,
                                     .has_destructor           = 0}) {}

  explicit Generic(base::any_invocable<instantiation_type const *(
                       compiler::WorkResources const &,
                       core::Arguments<Typed<ir::CompleteResultRef>> const &)>
                       fn)
      : LegacyType(IndexOf<Generic<T, InstantiationType>>(),
                   LegacyType::Flags{.is_default_initializable = 0,
                                     .is_copyable              = 1,
                                     .is_movable               = 1,
                                     .has_destructor           = 0}),
        gen_(std::move(fn)) {}

  void WriteTo(std::string *result) const override {
    result->append("generic");
  }

  bool is_big() const override { return false; }

  instantiation_type const *Instantiate(
      compiler::WorkResources const &wr,
      core::Arguments<Typed<ir::CompleteResultRef>> const &args) const {
    return gen_(wr, args);
  }

  Completeness completeness() const override { return Completeness::Complete; }

  core::Bytes bytes(core::Arch const &) const override {
    return core::Host.pointer().bytes();
  }

  core::Alignment alignment(core::Arch const &) const override {
    return core::Host.pointer().alignment();
  }

  void set_invocable(base::any_invocable<instantiation_type const *(
                         compiler::WorkResources const &,
                         core::Arguments<Typed<ir::CompleteResultRef>> const &)>
                         gen) {
    gen_ = std::move(gen);
  }

 private:
  // TODO: Eventually we will want a serializable version of this.
  base::any_invocable<instantiation_type const *(
      compiler::WorkResources const &,
      core::Arguments<Typed<ir::CompleteResultRef>> const &)>
      gen_;
};

}  // namespace type

#endif  // ICARUS_TYPE_GENERIC_H

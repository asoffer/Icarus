#ifndef ICARUS_IR_VALUE_GENERIC_FN_H
#define ICARUS_IR_VALUE_GENERIC_FN_H

#include <functional>
#include <iostream>

#include "absl/functional/any_invocable.h"
#include "base/extend.h"
#include "base/extend/absl_format.h"
#include "base/extend/absl_hash.h"
#include "base/extend/equality.h"
#include "compiler/work_resources.h"
#include "core/arguments.h"
#include "ir/module.h"
#include "ir/value/result_buffer.h"
#include "type/typed_value.h"

namespace ir {

// A `GenericFn` is a callable object which either requires some of the
// arguments passed in to be known at compile-time and/or deduces the types of
// parameters from the types of arguments.
//
// TODO: There is an important distinction to be made here: Some functions
// require arguments to be known at compile-time but the type may not depend on
// this. One possible interpretation is that this is just extra type-checking.
// Another is that we should instantiate a new version of this function. We need
// to decide if we intend these to be distinguishable in the language, and if
// not what approach we intend to take.
struct GenericFn : base::Extend<GenericFn, 1>::With<base::EqualityExtension,
                                                    base::AbslHashExtension,
                                                    base::AbslFormatExtension> {
  static constexpr std::string_view kAbslFormatString = "GenericFn(id = %u)";
  explicit GenericFn()                                = default;
  explicit GenericFn(
      absl::AnyInvocable<
          Fn(compiler::WorkResources const &wr,
             core::Arguments<type::Typed<CompleteResultRef>> const &)>
          gen);

  Fn concrete(
      compiler::WorkResources const &wr,
      core::Arguments<type::Typed<CompleteResultRef>> const &args) const;

 private:
  friend base::EnableExtensions;

  uintptr_t id_;
};

}  // namespace ir

#endif  // ICARUS_IR_VALUE_GENERIC_FN_H

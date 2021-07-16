#ifndef ICARUS_IR_VALUE_FOREIGN_FN_H
#define ICARUS_IR_VALUE_FOREIGN_FN_H

#include <cstring>
#include <iostream>

#include "base/extend.h"
#include "base/extend/absl_format.h"
#include "base/extend/equality.h"
#include "type/function.h"

namespace ir {
// `ForeignFn` represents a function callable in the language that is defined
// externally. New foreign functions can only be created in the intermediate
// representation by calling the `foreign` builtin.
struct ForeignFn : base::Extend<ForeignFn, 1>::With<base::AbslFormatExtension,
                                                    base::EqualityExtension> {
 private:
  using void_fn_ptr = void (*)();

 public:
  static constexpr std::string_view kAbslFormatString = "ForeignFn(id = %u)";
  using prefer_wrapper_for_type_erasure               = void;

  explicit ForeignFn(void (*fn)(), type::Function const *t);

  void_fn_ptr get() const;
  type::Function const *type() const;

 private:
  friend base::EnableExtensions;
  friend struct Fn;

  using id_t  = uintptr_t;
  ForeignFn() = default;
  ForeignFn(id_t id) : id_(id) {}

  id_t id_;
};

}  // namespace ir

#endif  // ICARUS_IR_VALUE_FOREIGN_FN_H

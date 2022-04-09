#ifndef ICARUS_IR_VALUE_FOREIGN_FN_H
#define ICARUS_IR_VALUE_FOREIGN_FN_H

#include <dlfcn.h>

#include <string>
#include <string_view>

#include "base/extend.h"
#include "base/extend/absl_format.h"
#include "base/extend/equality.h"
#include "base/flyweight_map.h"
#include "type/function.h"

namespace ir {

// `ForeignFn` represents a function callable in the language that is defined
// externally. New foreign functions can only be created in the intermediate
// representation by calling the `foreign` builtin.
struct ForeignFn : base::Extend<ForeignFn, 1>::With<base::AbslFormatExtension,
                                                    base::EqualityExtension> {
 private:
  using void_fn_ptr = void (*)();
  using representation_type =
      base::flyweight_map<std::pair<std::string, type::Function const*>,
                          void (*)()>::pointer;

 public:
  static constexpr std::string_view kAbslFormatString = "ForeignFn(%p)";
  using prefer_wrapper_for_type_erasure               = void;

  explicit ForeignFn(representation_type value) : value_(value) {}

  void_fn_ptr get() {
    auto& fn_ptr = value_->second;
    if (not fn_ptr) {
      dlerror();  // Clear previous errors.
      fn_ptr = reinterpret_cast<void_fn_ptr>(
          dlsym(RTLD_DEFAULT, value_->first.first.c_str()));
      char const* err = dlerror();
      ASSERT(err == nullptr);
    }
    return fn_ptr;
  }

  type::Function const* type() const { return value_->first.second; }
  std::string_view name() const { return value_->first.first; }

  friend base::EnableExtensions;
  friend struct Fn;

  explicit ForeignFn(uintptr_t n)
      : value_(reinterpret_cast<representation_type>(n)) {}

  representation_type value_;
};

}  // namespace ir

#endif  // ICARUS_IR_VALUE_FOREIGN_FN_H

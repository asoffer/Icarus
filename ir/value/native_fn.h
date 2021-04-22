#ifndef ICARUS_IR_VALUE_NATIVE_FN_H
#define ICARUS_IR_VALUE_NATIVE_FN_H

#include <cstring>
#include <memory>
#include <string_view>
#include <vector>

#include "base/extend.h"
#include "ir/compiled_fn.h"
#include "type/function.h"

namespace ir {

// `NativeFn` represents a function callable in the language that is defined
// internally. These are "just normal functions" in the language. The are
// distinguished from foreign functions.
struct NativeFn : base::Extend<NativeFn, 1>::With<base::AbslFormatExtension,
                                                  base::AbslHashExtension> {
  static constexpr std::string_view kAbslFormatString = "NativeFn(data = %p)";

  struct Data {
    CompiledFn *fn;
    type::Function const *type;
    base::untyped_buffer::const_iterator byte_code;
  };

  explicit NativeFn(Data const *data = nullptr);

  explicit operator bool() const { return data_; }

  type::Function const *type() const;

  base::untyped_buffer::const_iterator byte_code_iterator() const {
    return data_->byte_code;
  }

  CompiledFn *operator->() { return data_->fn; }
  CompiledFn &operator*() { return *data_->fn; }

 private:
  friend base::EnableExtensions;
  friend struct Fn;

  Data const *data_;
};

}  // namespace ir

#endif  // ICARUS_IR_VALUE_NATIVE_FN_H

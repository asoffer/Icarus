#ifndef ICARUS_IR_VALUE_NATIVE_FN_H
#define ICARUS_IR_VALUE_NATIVE_FN_H

#include <cstring>
#include <memory>
#include <string_view>
#include <vector>

#include "absl/strings/str_format.h"
#include "base/extend.h"
#include "base/extend/absl_hash.h"
#include "ir/subroutine.h"
#include "type/function.h"

namespace ir {

struct NativeFunctionInformation {
  type::Function const *type() const {
    return &fn.type()->as<type::Function>();
  }
  Subroutine fn;
  ByteCode byte_code;
};

// `NativeFn` represents a function callable in the language that is defined
// internally. These are "just normal functions" in the language. The are
// distinguished from foreign functions.
struct NativeFn : base::Extend<NativeFn, 1>::With<base::AbslHashExtension> {
  static constexpr std::string_view kAbslFormatString = "NativeFn(data = %p)";
  using prefer_wrapper_for_type_erasure               = void;

  using Data = NativeFunctionInformation;

  explicit NativeFn(NativeFunctionInformation *data = nullptr);

  explicit operator bool() const { return data_; }

  type::Function const *type() const;

  ByteCode &byte_code() { return data_->byte_code; }
  ByteCode const &byte_code() const { return data_->byte_code; }

  Subroutine *operator->() { return &data_->fn; }
  Subroutine &operator*() { return data_->fn; }

  friend absl::FormatConvertResult<absl::FormatConversionCharSet::kString>
  AbslFormatConvert(NativeFn fn, const absl::FormatConversionSpec &spec,
                    absl::FormatSink *s) {
    s->Append(absl::StrFormat("NativeFn(fn = %p)", &fn.data_->fn));
    return {true};
  }

  friend std::ostream &operator<<(std::ostream &os, NativeFn f) {
    absl::Format(&os, "%s", f);
    return os;
  }

  friend base::EnableExtensions;
  friend struct Fn;

  NativeFunctionInformation *data_;
};

}  // namespace ir

#endif  // ICARUS_IR_VALUE_NATIVE_FN_H

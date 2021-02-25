#ifndef ICARUS_BASE_EXTEND_ABSL_FORMAT_H
#define ICARUS_BASE_EXTEND_ABSL_FORMAT_H

#include <functional>
#include <iostream>

#include "base/extend.h"
#include "absl/strings/str_format.h"

namespace base {

template <typename T>
struct AbslFormatExtension {
  friend absl::FormatConvertResult<absl::FormatConversionCharSet::kString>
  AbslFormatConvert(T const &t, const absl::FormatConversionSpec &spec,
                    absl::FormatSink *s) {
    std::apply(
        [&](auto const &... fields) {
          s->Append(absl::StrFormat(T::kAbslFormatString, fields...));
        },
        t.field_refs());
    return {true};
  }

  friend std::ostream &operator<<(std::ostream &os, T const &t) {
    absl::Format(&os, "%s", t);
    return os;
  }
};

}  // namespace base

#endif  // ICARUS_BASE_EXTEND_ABSL_FORMAT_H

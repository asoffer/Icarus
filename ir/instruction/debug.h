#ifndef ICARUS_IR_INSTRUCTION_DEBUG_H
#define ICARUS_IR_INSTRUCTION_DEBUG_H

#include "absl/strings/str_format.h"
#include "base/extend.h"
#include "base/stringify.h"

namespace ir {

// A struct extension that looks for a constexpr std::string_view static member
// of the type named `kDebugFormat`, and uses it to build a string formatting
// the contents appropriately.
template <typename T>
struct DebugFormatExtension {
  // TODO: Use AbslFormatSink
  std::string to_string() const {
    return std::apply(
        [](auto const&... args) {
          using base::stringify;
          return absl::StrFormat(T::kDebugFormat, stringify(args)...);
        },
        static_cast<T const*>(this)->field_refs());
  }
};

}  // namespace ir

#endif  // ICARUS_IR_INSTRUCTION_DEBUG_H

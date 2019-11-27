#include "ir/cmd/scope.h"

#include "absl/strings/str_cat.h"
#include "absl/strings/str_join.h"
#include "base/stringify.h"

namespace ir {

std::string BlockCmd::DebugString(base::untyped_buffer::const_iterator *iter) {
  iter->read<BlockDef *>();
  std::vector<RegOr<AnyFunc>> before_vals =
      internal::Deserialize<uint16_t, AnyFunc>(
          iter, [](Reg reg) -> RegOr<AnyFunc> { return reg; });
  std::vector<RegOr<Jump const *>> after_vals =
      internal::Deserialize<uint16_t, Jump const *>(
          iter, [](Reg reg) -> RegOr<Jump const *> { return reg; });
  Reg result = iter->read<Reg>();

  using base::stringify;
  return absl::StrCat(
      stringify(result), " = before(",
      absl::StrJoin(before_vals, ", ",
                    [](std::string *out, RegOr<AnyFunc> f) {
                      return out->append(stringify(f));
                    }),
      ") after(",
      absl::StrJoin(after_vals, ", ",
                    [](std::string *out, RegOr<Jump const *> f) {
                      return out->append(stringify(f));
                    }),

      ")");
}

std::string ScopeCmd::DebugString(base::untyped_buffer::const_iterator *iter) {
  // TODO for this to be okay, you do need to iterate through everything.
  return "scope()";
}

}  // namespace ir

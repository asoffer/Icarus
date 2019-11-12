#include "ir/cmd/call.h"

#include "absl/strings/str_cat.h"
#include "absl/strings/str_join.h"
#include "ir/reg_or.h"

namespace ir {
std::string CallCmd::DebugString(base::untyped_buffer::const_iterator *iter) {
  bool fn_is_reg = iter->read<bool>();
  internal::ReadBits<uint16_t>(iter);

  using base::stringify;
  std::string result = fn_is_reg ? stringify(iter->read<Reg>())
                                 : stringify(iter->read<AnyFunc>());
  auto num_bytes_in_args = iter->read<core::Bytes>();
  iter->skip(num_bytes_in_args.value());
  uint16_t num_outs = iter->read<uint16_t>();
  std::vector<Reg> out_regs;
  out_regs.reserve(num_outs);
  for (uint16_t i = 0; i < num_outs; ++i) {
    out_regs.push_back(iter->read<Reg>());
  }
  absl::StrAppend(&result, " args[", stringify(num_bytes_in_args), "]: ",
                  absl::StrJoin(out_regs, ", ", [](std::string *out, Reg r) {
                    out->append(stringify(r));
                  }));
  return result;
}

}  // namespace ir

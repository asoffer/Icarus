#include "ir/reg.h"

#include "absl/strings/str_cat.h"

namespace ir {

std::string stringify(Reg r) {
  if (r.is_arg()) { return absl::StrCat("arg.", r.arg_value()); }
  if (r.is_out()) { return absl::StrCat("out.", r.out_value()); }
  return absl::StrCat("r.", r.value());
}

}  // namespace ir

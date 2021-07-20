#include "ir/instruction/core.h"

namespace ir {

std::string CallInstruction::to_string() const {
  using base::stringify;

  std::string_view separator = "";
  std::string arg_str;
  for (size_t i = 0; i < args_.num_entries(); ++i) {
    absl::StrAppendFormat(
        &arg_str, "%s%s", std::exchange(separator, ", "),
        args_[i].is_register() ? stringify(args_[i].get<Reg>()) : [&] {
          std::stringstream ss;
          fn_type_->params()[i].value.type().ShowValue(ss,
                                                       args_[i].AsComplete());
          return ss.str();
        }());
  }

  return absl::StrFormat(
      "%scall %s: %s",
      fn_type_->output().empty()
          ? ""
          : absl::StrCat("(",
                         absl::StrJoin(outs_.regs(), ", ",
                                       [](std::string* s, auto const& out) {
                                         absl::StrAppend(s, stringify(out));
                                       }),
                         ") = "),
      stringify(fn_), arg_str);
}

}  // namespace ir

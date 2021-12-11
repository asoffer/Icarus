#include "ir/instruction/core.h"

#include "ir/value/slice.h"

namespace ir {
namespace {

std::string Representation(CompleteResultRef ref, type::Type t) {
  if (t.is_big()) {
    return t.Representation(CompleteResultRef(base::untyped_buffer_view(
        ref.get<addr_t>(), t.bytes(core::Host).value())));
  } else {
    return t.Representation(ref);
  }
}

}  // namespace

std::string CallInstruction::to_string() const {
  std::string_view separator = "";
  std::string arg_str;
  for (size_t i = 0; i < args_.num_entries(); ++i) {
    std::string a;
    if (args_[i].is_register()) {
      std::stringstream ss;
      ss << args_[i].get<Reg>();
      a = ss.str();
    } else {
      a = Representation(args_[i].AsComplete(),
                         fn_type_->params()[i].value.type());
    }
    absl::StrAppend(&arg_str, std::exchange(separator, ", "), a);
  }

  std::stringstream fn_ss;
  fn_ss << fn_;
  return absl::StrFormat(
      "%scall %s: %s",
      fn_type_->return_types().empty()
          ? ""
          : absl::StrCat("(",
                         absl::StrJoin(outs_.regs(), ", ",
                                       [](std::string* s, auto const& out) {
                                         std::stringstream ss;
                                         ss << out;
                                         absl::StrAppend(s, ss.str());
                                       }),
                         ") = "),
      fn_ss.str(), arg_str);
}

}  // namespace ir

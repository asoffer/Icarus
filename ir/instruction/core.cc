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
  using base::stringify;

  std::string_view separator = "";
  std::string arg_str;
  for (size_t i = 0; i < args_.num_entries(); ++i) {
    absl::StrAppendFormat(
        &arg_str, "%s%s", std::exchange(separator, ", "),
        args_[i].is_register()
            ? stringify(args_[i].get<Reg>())
            : Representation(args_[i].AsComplete(),
                             fn_type_->params()[i].value.type()));
    ;
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

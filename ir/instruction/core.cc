#include "ir/instruction/core.h"

#include "absl/strings/str_join.h"
#include "ir/value/slice.h"
#include "type/function.h"

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
                         fn_type_.as<type::Function>().parameters()[i].value.type());
    }
    absl::StrAppend(&arg_str, std::exchange(separator, ", "), a);
  }

  std::stringstream fn_ss;
  fn_ss << fn_;
  return absl::StrFormat(
      "%scall %s: %s",
      fn_type_.as<type::Function>().return_types().empty()
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

bool InterpretInstruction(interpreter::Interpreter& interpreter,
                          CallInstruction const& inst) {
  ir::CompleteResultBuffer arguments;
  for (size_t i = 0; i < inst.arguments().num_entries(); ++i) {
    ir::PartialResultRef argument = inst.arguments()[i];
    base::untyped_buffer_view data =
        argument.is_register()
            ? interpreter.frame().raw(argument.get<ir::Reg>())
            : argument.raw();
    arguments.append_raw(data);
  }
  std::vector<addr_t> outputs;
  absl::Span returns = inst.fn_type_.as<type::Function>().return_types();
  for (Reg r : inst.outs_) {
    if (returns[outputs.size()].is_big()) {
      outputs.push_back(interpreter.frame().resolve<addr_t>(r));
    } else {
      outputs.push_back(interpreter.frame().find(r));
    }
  }
  return interpreter.push_frame(interpreter.frame().resolve(inst.func()),
                                arguments, outputs);
}

}  // namespace ir

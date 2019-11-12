#include "ir/cmd/misc.h"

namespace ir {

std::string SemanticCmd::DebugString(
    base::untyped_buffer::const_iterator *iter) {
  size_t num_args = 0;
  std::string_view cmd_name;
  switch (iter->read<Kind>()) {
    case Kind::Init: {
      num_args = 1;
      cmd_name = "init";
    } break;
    case Kind::Destroy: {
      num_args = 1;
      cmd_name = "destroy";
    } break;
    case Kind::Move: {
      num_args = 2;
      cmd_name = "move";
    } break;
    case Kind::Copy: {
      num_args = 2;
      cmd_name = "move";
    } break;
  }

  using base::stringify;
  switch (num_args) {
    case 1: {
      auto *t = iter->read<type::Type const *>();
      return absl::StrCat(cmd_name, " ", t->to_string(), " ",
                          stringify((iter->read<Reg>())));
    } break;
    case 2: {
      bool to_reg = iter->read<bool>();
      auto *t     = iter->read<type::Type const *>();
      auto reg    = iter->read<Reg>();
      return absl::StrCat(cmd_name, " ", stringify(reg), " ",
                          to_reg ? stringify(iter->read<Reg>())
                                 : stringify(iter->read<Addr>()));
    } break;
    default: UNREACHABLE();
  }
}

std::string LoadSymbolCmd::DebugString(
    base::untyped_buffer::const_iterator *iter) {
  auto name = iter->read<std::string_view>();
  auto type = iter->read<type::Type const *>();
  auto reg  = iter->read<Reg>();
  return absl::StrCat(stringify(reg), " = load-symbol(", name, ", ",
                      type->to_string(), ")");
}

std::string TypeInfoCmd::DebugString(
    base::untyped_buffer::const_iterator *iter) {
  auto ctrl_bits  = iter->read<uint8_t>();
  std::string arg = (ctrl_bits & 0x01)
                        ? stringify(iter->read<Reg>())
                        : iter->read<type::Type const *>()->to_string();
  auto reg = iter->read<Reg>();
  return absl::StrCat(stringify(reg),
                      (ctrl_bits & 0x02) ? " = alignment " : " = bytes ", arg);
}

std::string AccessCmd::DebugString(base::untyped_buffer::const_iterator *iter) {
  auto ctrl_bits   = iter->read<control_bits>();
  auto const *type = iter->read<type::Type const *>();

  auto addr = ctrl_bits.reg_ptr ? RegOr<Addr>(iter->read<Reg>())
                                : RegOr<Addr>(iter->read<Addr>());
  auto index = ctrl_bits.reg_index ? RegOr<int64_t>(iter->read<Reg>())
                                   : RegOr<int64_t>(iter->read<int64_t>());
  auto reg = iter->read<Reg>();
  return absl::StrCat(
      stringify(reg),
      ctrl_bits.is_array ? " = access-array " : " = access-index ",
      type->to_string(), " ", stringify(addr), " ", stringify(index));
}

std::string VariantAccessCmd::DebugString(
    base::untyped_buffer::const_iterator *iter) {
  bool get_val = iter->read<bool>();
  bool is_reg  = iter->read<bool>();
  auto addr =
      is_reg ? RegOr<Addr>(iter->read<Reg>()) : RegOr<Addr>(iter->read<Addr>());
  if (get_val) {
    auto const *variant = iter->read<type::Variant const *>();
    Reg reg             = iter->read<Reg>();
    return absl::StrCat(stringify(reg), "variant-value ", variant->to_string(),
                        " ", stringify(addr));
  } else {
    Reg reg = iter->read<Reg>();
    return absl::StrCat(stringify(reg), " = variant-type ", stringify(addr));
  }
}

}  // namespace ir

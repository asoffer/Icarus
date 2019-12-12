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
      type::Type const *t = iter->read<type::Type const *>();
      return absl::StrCat(cmd_name, " ", t->to_string(), " ",
                          stringify((iter->read<Reg>())));
    } break;
    case 2: {
      bool to_reg         = iter->read<bool>();
      type::Type const *t = iter->read<type::Type const *>();
      Reg reg             = iter->read<Reg>();
      return absl::StrCat(cmd_name, " ", stringify(reg), " ",
                          to_reg ? stringify(iter->read<Reg>())
                                 : stringify(iter->read<Addr>()));
    } break;
    default: UNREACHABLE();
  }
}

std::string LoadSymbolCmd::DebugString(
    base::untyped_buffer::const_iterator *iter) {
  std::string_view name  = iter->read<std::string_view>();
  type::Type const *type = iter->read<type::Type const *>();
  Reg reg                = iter->read<Reg>();
  return absl::StrCat(stringify(reg), " = load-symbol(", name, ", ",
                      type->to_string(), ")");
}

std::string TypeInfoCmd::DebugString(
    base::untyped_buffer::const_iterator *iter) {
  uint8_t ctrl_bits  = iter->read<uint8_t>();
  std::string arg =
      (ctrl_bits & 0x01)
          ? stringify(iter->read<Reg>())
          : static_cast<type::Type const *>(iter->read<type::Type const *>())
                ->to_string();
  Reg reg = iter->read<Reg>();
  return absl::StrCat(stringify(reg),
                      (ctrl_bits & 0x02) ? " = alignment " : " = bytes ", arg);
}

std::string AccessCmd::DebugString(base::untyped_buffer::const_iterator *iter) {
  control_bits ctrl_bits = iter->read<control_bits>();
  type::Type const *type = iter->read<type::Type const *>();

  RegOr<Addr> addr = ctrl_bits.reg_ptr ? RegOr<Addr>(iter->read<Reg>())
                                       : RegOr<Addr>(iter->read<Addr>());
  RegOr<int64_t> index = ctrl_bits.reg_index
                             ? RegOr<int64_t>(iter->read<Reg>())
                             : RegOr<int64_t>(iter->read<int64_t>());
  Reg reg = iter->read<Reg>();
  return absl::StrCat(
      stringify(reg),
      ctrl_bits.is_array ? " = access-array " : " = access-index ",
      type->to_string(), " ", stringify(addr), " ", stringify(index));
}

std::string VariantAccessCmd::DebugString(
    base::untyped_buffer::const_iterator *iter) {
  bool get_val = iter->read<bool>();
  DEBUG_LOG("VariantAccessCmd")("get_val = ", get_val);
  bool is_reg  = iter->read<bool>();
  DEBUG_LOG("VariantAccessCmd")("is_reg = ", is_reg);
  auto addr =
      is_reg ? RegOr<Addr>(iter->read<Reg>()) : RegOr<Addr>(iter->read<Addr>());
  DEBUG_LOG("VariantAccessCmd")("addr = ", stringify(addr));
  if (get_val) {
    Reg reg = iter->read<Reg>();
    return absl::StrCat(stringify(reg), " = variant-value ", stringify(addr));
  } else {
    Reg reg = iter->read<Reg>();
    return absl::StrCat(stringify(reg), " = variant-type ", stringify(addr));
  }
}

}  // namespace ir

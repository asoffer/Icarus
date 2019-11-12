#include "ir/cmd/types.h"

namespace ir {

std::string EnumerationCmd::DebugString(
    base::untyped_buffer::const_iterator *iter) {
  bool is_enum_not_flags   = iter->read<bool>();
  uint16_t num_enumerators = iter->read<uint16_t>();
  uint16_t num_specified   = iter->read<uint16_t>();
  iter->read<module::BasicModule *>();
  std::vector<std::pair<uint16_t, std::string_view>> names;
  names.reserve(num_enumerators);
  for (uint16_t i = 0; i < num_enumerators; ++i) {
    names.emplace_back(i, iter->read<std::string_view>());
  }

  absl::flat_hash_map<uint64_t, RegOr<EnumerationCmd::enum_t>> specified_vals;
  for (uint16_t i = 0; i < num_specified; ++i) {
    uint64_t index = iter->read<uint64_t>();
    bool is_reg    = iter->read<bool>();
    specified_vals.emplace(
        index, is_reg ? RegOr<uint64_t>(iter->read<Reg>())
                      : RegOr<uint64_t>(iter->read<EnumerationCmd::enum_t>()));
  }

  Reg r = iter->read<Reg>();

  return absl::StrCat(
      stringify(r), is_enum_not_flags ? " = enum (" : " = flags (",
      absl::StrJoin(names, ", ",
                    [&](std::string *out,
                        std::pair<uint16_t, std::string_view> const &s) {
                      auto iter = specified_vals.find(s.first);
                      out->append(s.second);
                      if (iter != specified_vals.end()) {
                        out->append("=");
                        out->append(stringify(iter->second));
                      }
                    }),
      ")");
}

std::string StructCmd::DebugString(base::untyped_buffer::const_iterator *iter) {
  return "NOT_YET";
}

std::string OpaqueTypeCmd::DebugString(
    base::untyped_buffer::const_iterator *iter) {
  return absl::StrCat(stringify(iter->read<Reg>()), " = opaque");
}

std::string ArrayCmd::DebugString(base::untyped_buffer::const_iterator *iter) {
  auto ctrl_bits = iter->read<control_bits>();
  auto len       = ctrl_bits.length_is_reg ? RegOr<length_t>(iter->read<Reg>())
                                     : RegOr<length_t>(iter->read<length_t>());
  auto data_type =
      ctrl_bits.type_is_reg
          ? RegOr<type::Type const *>(iter->read<Reg>())
          : RegOr<type::Type const *>(iter->read<type::Type const *>());

  return absl::StrCat(stringify(iter->read<Reg>()), " = [", stringify(len),
                      "; ", stringify(data_type), ")");
}

}  // namespace ir

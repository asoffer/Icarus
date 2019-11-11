#include "ir/cmd/types.h"

namespace ir {
namespace {

template <bool IsEnumNotFlags>
Reg EnumerationImpl(
    module::BasicModule *mod, absl::Span<std::string_view const> names,
    absl::flat_hash_map<uint64_t, RegOr<EnumerationCmd::enum_t>> const
        &specified_values) {
  auto &blk = *GetBuilder().CurrentBlock();
  blk.cmd_buffer_.append_index<EnumerationCmd>();
  blk.cmd_buffer_.append(IsEnumNotFlags);
  blk.cmd_buffer_.append<uint16_t>(names.size());
  blk.cmd_buffer_.append<uint16_t>(specified_values.size());
  blk.cmd_buffer_.append(mod);
  for (auto name : names) { blk.cmd_buffer_.append(name); }

  for (auto const &[index, val] : specified_values) {
    // TODO these could be packed much more efficiently.
    blk.cmd_buffer_.append(index);
    blk.cmd_buffer_.append<bool>(val.is_reg());
    val.apply([&](auto v) { blk.cmd_buffer_.append(v); });
  }

  Reg result =
      MakeResult<std::conditional_t<IsEnumNotFlags, EnumVal, FlagsVal>>();
  blk.cmd_buffer_.append(result);
  DEBUG_LOG("enum")(blk.cmd_buffer_.buf_.to_string());
  DEBUG_LOG("enum")(blk.cmd_buffer_.to_string());
  return result;
}
}  // namespace

Reg Enum(module::BasicModule *mod, absl::Span<std::string_view const> names,
         absl::flat_hash_map<uint64_t, RegOr<EnumerationCmd::enum_t>> const
             &specified_values) {
  return EnumerationImpl<true>(mod, names, specified_values);
}

Reg Flags(module::BasicModule *mod, absl::Span<std::string_view const> names,
          absl::flat_hash_map<uint64_t, RegOr<EnumerationCmd::enum_t>> const
              &specified_values) {
  return EnumerationImpl<false>(mod, names, specified_values);
}

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

Reg Struct(ast::Scope const *scope, module::BasicModule *mod,
           std::vector<std::tuple<std::string_view, RegOr<type::Type const *>>>
               fields) {
  auto &blk = *GetBuilder().CurrentBlock();
  blk.cmd_buffer_.append_index<StructCmd>();
  blk.cmd_buffer_.append<uint16_t>(fields.size());
  blk.cmd_buffer_.append(scope);
  blk.cmd_buffer_.append(mod);
  // TODO determine if order randomization makes sense here. Or perhaps you want
  // to do it later? Or not at all?
  std::shuffle(fields.begin(), fields.end(), absl::BitGen{});
  for (auto &[name, t] : fields) { blk.cmd_buffer_.append(name); }

  // TODO performance: Serialize requires an absl::Span here, but we'd love to
  // not copy out the elements of `fields`.
  std::vector<RegOr<type::Type const *>> types;
  types.reserve(fields.size());
  for (auto &[name, t] : fields) { types.push_back(t); }
  internal::Serialize<uint16_t>(&blk.cmd_buffer_, absl::MakeConstSpan(types));

  Reg result = MakeResult<type::Type const *>();
  blk.cmd_buffer_.append(result);
  DEBUG_LOG("struct")(blk.cmd_buffer_.buf_.to_string());
  DEBUG_LOG("struct")(blk.cmd_buffer_.to_string());
  return result;
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

RegOr<type::Function const *> Arrow(
    absl::Span<RegOr<type::Type const *> const> ins,
    absl::Span<RegOr<type::Type const *> const> outs) {
  if (absl::c_all_of(
          ins, [](RegOr<type::Type const *> r) { return not r.is_reg(); }) and
      absl::c_all_of(
          outs, [](RegOr<type::Type const *> r) { return not r.is_reg(); })) {
    std::vector<type::Type const *> in_vec, out_vec;
    in_vec.reserve(ins.size());
    for (auto in : ins) { in_vec.push_back(in.value()); }
    out_vec.reserve(outs.size());
    for (auto out : outs) { out_vec.push_back(out.value()); }
    return type::Func(std::move(in_vec), std::move(out_vec));
  }

  auto &blk = *GetBuilder().CurrentBlock();
  blk.cmd_buffer_.append_index<ArrowCmd>();
  internal::Serialize<uint16_t>(&blk.cmd_buffer_, ins);
  internal::Serialize<uint16_t>(&blk.cmd_buffer_, outs);

  Reg result = MakeResult<type::Type const *>();
  blk.cmd_buffer_.append(result);
  return RegOr<type::Function const *>{result};
}

Reg OpaqueType(module::BasicModule const *mod) {
  auto &blk = *GetBuilder().CurrentBlock();
  blk.cmd_buffer_.append_index<OpaqueTypeCmd>();
  blk.cmd_buffer_.append(mod);
  Reg result = MakeResult<type::Type const *>();
  blk.cmd_buffer_.append(result);
  return result;
}

RegOr<type::Type const *> Array(RegOr<ArrayCmd::length_t> len,
                                RegOr<type::Type const *> data_type) {
  if (not len.is_reg() and data_type.is_reg()) {
    return type::Arr(len.value(), data_type.value());
  }

  auto &blk = *GetBuilder().CurrentBlock();
  blk.cmd_buffer_.append_index<ArrayCmd>();
  blk.cmd_buffer_.append(
      ArrayCmd::MakeControlBits(len.is_reg(), data_type.is_reg()));

  len.apply([&](auto v) { blk.cmd_buffer_.append(v); });
  data_type.apply([&](auto v) { blk.cmd_buffer_.append(v); });
  Reg result = MakeResult<type::Type const *>();
  blk.cmd_buffer_.append(result);
  return result;
}

}  // namespace ir

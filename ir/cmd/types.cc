#include "ir/cmd/types.h"

namespace ir {
namespace {

template <bool IsEnumNotFlags>
Reg EnumerationImpl(
    Module *mod, absl::Span<std::string_view const> names,
    absl::flat_hash_map<uint64_t, RegOr<EnumerationCmd::enum_t>> const
        &specified_values) {
  auto &blk = GetBuilder().function()->block(GetBuilder().CurrentBlock());
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

Reg Enum(::Module *mod, absl::Span<std::string_view const> names,
         absl::flat_hash_map<uint64_t, RegOr<EnumerationCmd::enum_t>> const
             &specified_values) {
  return EnumerationImpl<true>(mod, names, specified_values);
}

Reg Flags(::Module *mod, absl::Span<std::string_view const> names,
          absl::flat_hash_map<uint64_t, RegOr<EnumerationCmd::enum_t>> const
              &specified_values) {
  return EnumerationImpl<false>(mod, names, specified_values);
}

std::optional<BlockIndex> EnumerationCmd::Execute(
    base::untyped_buffer::iterator *iter, std::vector<Addr> const &ret_slots,
    backend::ExecContext *ctx) {
  bool is_enum_not_flags   = iter->read<bool>();
  uint16_t num_enumerators = iter->read<uint16_t>();
  uint16_t num_specified   = iter->read<uint16_t>();
  ::Module *mod            = iter->read<::Module *>();
  std::vector<std::pair<std::string_view, std::optional<enum_t>>> enumerators;
  enumerators.reserve(num_enumerators);
  for (uint16_t i = 0; i < num_enumerators; ++i) {
    enumerators.emplace_back(iter->read<std::string_view>(), std::nullopt);
  }

  absl::flat_hash_set<enum_t> vals;

  for (uint16_t i = 0; i < num_specified; ++i) {
    uint64_t index = iter->read<uint64_t>();
    enum_t val = iter->read<bool>() ? ctx->resolve<enum_t>(iter->read<Reg>())
                                    : iter->read<enum_t>();
    enumerators[i].second = val;
    vals.insert(val);
  }

  type::Type *result = nullptr;
  absl::BitGen gen;

  if (is_enum_not_flags) {
    for (auto& [name, maybe_val] : enumerators) {
      DEBUG_LOG("enum")(name, " => ", maybe_val);

      if (!maybe_val.has_value()) {
        bool success;
        enum_t x;
        do {
          x       = absl::Uniform<enum_t>(gen);
          success = vals.insert(x).second;
          DEBUG_LOG("enum")("Adding value ", x, " for ", name);
          maybe_val = x;
        } while (!success);
      }
    }
    absl::flat_hash_map<std::string, EnumVal> mapping;

    for (auto [name, maybe_val] : enumerators) {
      ASSERT(maybe_val.has_value() == true);
      mapping.emplace(std::string(name), EnumVal{maybe_val.value()});
    }
    DEBUG_LOG("enum")(vals, ", ", mapping);
    result = new type::Enum(mod, std::move(mapping));
  } else {
    for (auto& [name, maybe_val] : enumerators) {
      DEBUG_LOG("flags")(name, " => ", maybe_val);

      if (!maybe_val.has_value()) {
        bool success;
        enum_t x;
        do {
          x       = absl::Uniform<enum_t>(absl::IntervalClosedOpen, gen, 0,
                                    std::numeric_limits<enum_t>::digits);
          success = vals.insert(x).second;
          DEBUG_LOG("enum")("Adding value ", x, " for ", name);
          maybe_val = x;
        } while (!success);
      }
    }

    absl::flat_hash_map<std::string, FlagsVal> mapping;

    for (auto [name, maybe_val] : enumerators) {
      ASSERT(maybe_val.has_value() == true);
      mapping.emplace(std::string(name),
                      FlagsVal{enum_t{1} << maybe_val.value()});
    }

    DEBUG_LOG("flags")(vals, ", ", mapping);
    result = new type::Flags(mod, std::move(mapping));
  }

  auto &frame = ctx->call_stack.top();
  frame.regs_.set(GetOffset(frame.fn_, iter->read<Reg>()), result);
  return std::nullopt;
}

std::string EnumerationCmd::DebugString(
    base::untyped_buffer::const_iterator *iter) {
  bool is_enum_not_flags   = iter->read<bool>();
  uint16_t num_enumerators = iter->read<uint16_t>();
  uint16_t num_specified   = iter->read<uint16_t>();
  iter->read<::Module *>();
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

void EnumerationCmd::UpdateForInlining(base::untyped_buffer::iterator *iter,
                                       Inliner const &inliner) {
  iter->read<bool>();
  uint16_t num_enumerators = iter->read<uint16_t>();
  uint16_t num_specified   = iter->read<uint16_t>();
  iter->read<::Module *>();
  for (uint16_t i = 0; i < num_enumerators; ++i) {
    // TODO jump ahead.
    iter->read<std::string_view>();
  }

  for (uint16_t i = 0; i < num_specified; ++i) {
    iter->read<uint64_t>();  // index
    bool is_reg = iter->read<bool>();
    if (is_reg) {
      inliner.Inline(&iter->read<Reg>());
    } else {
      iter->read<EnumerationCmd::enum_t>();
    }
  }

  iter->read<Reg>();
}

std::optional<BlockIndex> StructCmd::Execute(
    base::untyped_buffer::iterator *iter, std::vector<Addr> const &ret_slots,
    backend::ExecContext *ctx) {
  std::vector<std::tuple<std::string_view, type::Type const *>> fields;
  auto num = iter->read<uint16_t>();
  fields.reserve(num);
  auto *scope = iter->read<core::Scope const *>();
  auto *mod   = iter->read<::Module *>();
  for (uint16_t i = 0; i < num; ++i) {
    fields.emplace_back(iter->read<std::string_view>(), nullptr);
  }

  size_t index = 0;
  internal::Deserialize<uint16_t, type::Type const *>(iter, [&](Reg &reg) {
    std::get<1>(fields[index++]) = ctx->resolve<type::Type const *>(reg);
  });

  auto &frame = ctx->call_stack.top();
  frame.regs_.set(GetOffset(frame.fn_, iter->read<Reg>()),
                  new type::Struct(scope, mod, fields));
  return std::nullopt;
}

std::string StructCmd::DebugString(base::untyped_buffer::const_iterator *iter) {
  return "NOT_YET";
}

void StructCmd::UpdateForInlining(base::untyped_buffer::iterator *iter,
                                  Inliner const &inliner) {
  auto num = iter->read<uint16_t>();
  iter->read<core::Scope*>();
  iter->read<::Module *>();
  for (uint16_t i = 0; i < num; ++i) { iter->read<std::string_view>(); }
  internal::Deserialize<uint16_t, type::Type const *>(
      iter, [&inliner](Reg &reg) { inliner.Inline(&reg); });
  inliner.Inline(&iter->read<Reg>(), ::type::Type_);
}

Reg Struct(core::Scope const *scope, ::Module *mod,
           std::vector<std::tuple<std::string_view, RegOr<type::Type const *>>>
               fields) {
  auto &blk = GetBuilder().function()->block(GetBuilder().CurrentBlock());
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

std::optional<BlockIndex> OpaqueTypeCmd::Execute(
    base::untyped_buffer::iterator *iter, std::vector<Addr> const &ret_slots,
    backend::ExecContext *ctx) {
  auto &frame = ctx->call_stack.top();
  auto *mod   = iter->read<::Module const *>();
  frame.regs_.set(GetOffset(frame.fn_, iter->read<Reg>()),
                  new type::Opaque(mod));
  return std::nullopt;

}

std::string OpaqueTypeCmd::DebugString(
    base::untyped_buffer::const_iterator *iter) {
  return absl::StrCat(stringify(iter->read<Reg>()), " = opaque");
}

void OpaqueTypeCmd::UpdateForInlining(base::untyped_buffer::iterator *iter,
                                      Inliner const &inliner) {
  iter->read<::Module const *>();
  inliner.Inline(&iter->read<Reg>());
}

std::optional<BlockIndex> ArrayCmd::Execute(
    base::untyped_buffer::iterator *iter, std::vector<Addr> const &ret_slots,
    backend::ExecContext *ctx) {
  auto &frame    = ctx->call_stack.top();
  auto ctrl_bits = iter->read<control_bits>();
  length_t len   = ctrl_bits.length_is_reg
                     ? ctx->resolve<length_t>(iter->read<Reg>())
                     : iter->read<length_t>();
  type::Type const *data_type =
      ctrl_bits.type_is_reg
          ? ctx->resolve<type::Type const *>(iter->read<Reg>())
          : iter->read<type::Type const *>();

  frame.regs_.set(GetOffset(frame.fn_, iter->read<Reg>()),
                  type::Arr(len, data_type));
  return std::nullopt;
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

void ArrayCmd::UpdateForInlining(base::untyped_buffer::iterator *iter,
                                 Inliner const &inliner) {
  auto ctrl_bits = iter->read<control_bits>();
  if (ctrl_bits.length_is_reg) {
    inliner.Inline(&iter->read<Reg>());
  } else {
    iter->read<length_t>();
  }

  if (ctrl_bits.type_is_reg) {
    inliner.Inline(&iter->read<Reg>());
  } else {
    iter->read<type::Type const *>();
  }
}

RegOr<type::Function const *> Arrow(
    absl::Span<RegOr<type::Type const *> const> ins,
    absl::Span<RegOr<type::Type const *> const> outs) {
  if (absl::c_all_of(ins,
                     [](RegOr<type::Type const *> r) { return !r.is_reg(); }) &&
      absl::c_all_of(outs,
                     [](RegOr<type::Type const *> r) { return !r.is_reg(); })) {
    std::vector<type::Type const *> in_vec, out_vec;
    in_vec.reserve(ins.size());
    for (auto in : ins) { in_vec.push_back(in.value()); }
    out_vec.reserve(outs.size());
    for (auto out : outs) { out_vec.push_back(out.value()); }
    return type::Func(std::move(in_vec), std::move(out_vec));
  }

  auto &blk = GetBuilder().function()->block(GetBuilder().CurrentBlock());
  blk.cmd_buffer_.append_index<ArrowCmd>();
  internal::Serialize<uint16_t>(&blk.cmd_buffer_, ins);
  internal::Serialize<uint16_t>(&blk.cmd_buffer_, outs);

  Reg result = MakeResult<type::Type const *>();
  blk.cmd_buffer_.append(result);
  return RegOr<type::Function const *>{result};
}

Reg OpaqueType(::Module const *mod) {
  auto &blk = GetBuilder().function()->block(GetBuilder().CurrentBlock());
  blk.cmd_buffer_.append_index<OpaqueTypeCmd>();
  blk.cmd_buffer_.append(mod);
  Reg result = MakeResult<type::Type const *>();
  blk.cmd_buffer_.append(result);
  return result;
}

RegOr<type::Type const *> Array(RegOr<ArrayCmd::length_t> len,
                                RegOr<type::Type const *> data_type) {
  if (!len.is_reg() && data_type.is_reg()) {
    return type::Arr(len.value(), data_type.value());
  }
  auto &blk = GetBuilder().function()->block(GetBuilder().CurrentBlock());
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

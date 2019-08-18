#include "ir/cmd/types.h"

namespace ir {
namespace {

template <bool IsEnumNotFlags>
Reg EnumerationImpl(
    Module *mod, absl::Span<std::string_view const> names,
    absl::flat_hash_map<uint64_t, RegOr<EnumerationCmd::enum_t>> const
        &specified_values) {
  auto &blk = GetBlock();
  blk.cmd_buffer_.append_index<EnumerationCmd>();
  blk.cmd_buffer_.append(IsEnumNotFlags);
  blk.cmd_buffer_.append<uint16_t>(names.size());
  blk.cmd_buffer_.append<uint16_t>(specified_values.size());
  blk.cmd_buffer_.append(mod);
  for (auto name : names) { blk.cmd_buffer_.append(name); }

  for (auto const &[index, val] : specified_values) {
    // TODO these could be packed much more efficiently.
    blk.cmd_buffer_.append(index);
    blk.cmd_buffer_.append<bool>(val.is_reg_);
    if (val.is_reg_) {
      blk.cmd_buffer_.append(val.reg_);
    } else {
      blk.cmd_buffer_.append(val.val_);
    }
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

}  // namespace ir

#ifndef ICARUS_TYPE_FLAGS_H
#define ICARUS_TYPE_FLAGS_H

#include <optional>
#include <string>
#include <string_view>

#include "absl/container/flat_hash_map.h"
#include "absl/random/distributions.h"
#include "absl/random/random.h"
#include "base/debug.h"
#include "base/extend.h"
#include "ir/instruction/base.h"
#include "ir/instruction/debug.h"
#include "ir/instruction/inliner.h"
#include "ir/interpretter/execution_context.h"
#include "ir/value/enum_and_flags.h"
#include "module/module.h"
#include "type/type.h"
#include "type/typed_value.h"

namespace type {
struct Flags : public type::LegacyType {
  TYPE_FNS(Flags);

  Flags(module::BasicModule const *mod)
      : LegacyType(LegacyType::Flags{.is_default_initializable = 1,
                                     .is_copyable              = 1,
                                     .is_movable               = 1,
                                     .has_destructor           = 0}),
        mod_(mod) {}

  void SetMembers(absl::flat_hash_map<std::string, ir::FlagsVal> vals) {
    vals_ = std::move(vals);
    for (auto &[name, val] : vals_) {
      All |= val.value;
      members_.emplace(val, name);
    }
  }

  bool is_big() const override { return false; }

  Completeness completeness() const override { return completeness_; }
  void complete() { completeness_ = Completeness::Complete; }

  void Accept(VisitorBase *visitor, void *ret, void *arg_tuple) const override {
    visitor->ErasedVisit(this, ret, arg_tuple);
  }

  std::optional<ir::FlagsVal> Get(std::string_view name) const;
  Typed<ir::FlagsVal, Flags> EmitLiteral(std::string_view member_name) const;

  std::optional<std::string_view> name(ir::FlagsVal v) const {
    LOG("flags", "%s", v);
    auto it = members_.find(v);
    if (it == members_.end()) return std::nullopt;
    return it->second;
  }

  bool IsDefaultInitializable() const { return false; }

  uint64_t All = 0;

  Completeness completeness_;
  module::BasicModule const *mod_;

  ICARUS_PRIVATE
  // TODO combine these into a single bidirectional map?
  absl::flat_hash_map<std::string, ir::FlagsVal> vals_;
  absl::flat_hash_map<ir::FlagsVal, std::string> members_;
};

struct FlagsInstruction
    : base::Extend<FlagsInstruction>::With<ir::ByteCodeExtension,
                                           ir::InlineExtension> {
  void Apply(interpretter::ExecutionContext &ctx) const {
    using flags_t = ir::FlagsVal::underlying_type;

    absl::flat_hash_set<flags_t> used_vals;

    for (auto const &[index, reg_or_value] : specified_values_) {
      used_vals.insert(ctx.resolve(reg_or_value));
    }

    absl::BitGen gen;

    absl::flat_hash_map<std::string, ir::FlagsVal> mapping;

    for (size_t i = 0; i < names_.size(); ++i) {
      auto iter = specified_values_.find(i);
      if (iter != specified_values_.end()) {
        mapping.emplace(names_[i], ctx.resolve(iter->second));
        continue;
      }

      bool success;
      flags_t proposed_value;
      do {
        proposed_value = flags_t{1} << absl::Uniform<flags_t>(
                             absl::IntervalClosedOpen, gen, 0,
                             std::numeric_limits<flags_t>::digits);
        success = used_vals.insert(proposed_value).second;
      } while (not success);
      mapping.try_emplace(std::string(names_[i]), proposed_value);
    }

    type->SetMembers(std::move(mapping));
    type->complete();
    ctx.current_frame().regs_.set(result, type::Type(type));
  }

  std::string to_string() const {
    using base::stringify;
    return absl::StrCat(stringify(result), " = flags (",
                        absl::StrJoin(names_, ", "), ")");
  }

  type::Flags *type;
  std::vector<std::string_view> names_;
  absl::flat_hash_map<uint64_t, ir::RegOr<uint64_t>> specified_values_;
  ir::Reg result;
};

struct XorFlagsInstruction
    : base::Extend<XorFlagsInstruction>::With<ir::ByteCodeExtension,
                                              ir::InlineExtension,
                                              ir::DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "%3$s = xor-flags %1$s %2$s";

  void Apply(interpretter::ExecutionContext &ctx) const {
    ctx.current_frame().regs_.set(result,
                                  Apply(ctx.resolve(lhs), ctx.resolve(rhs)));
  }
  static ir::FlagsVal Apply(ir::FlagsVal lhs, ir::FlagsVal rhs) {
    return lhs ^ rhs;
  }

  ir::RegOr<ir::FlagsVal> lhs;
  ir::RegOr<ir::FlagsVal> rhs;
  ir::Reg result;
};

struct AndFlagsInstruction
    : base::Extend<AndFlagsInstruction>::With<ir::ByteCodeExtension,
                                              ir::InlineExtension,
                                              ir::DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "%3$s = and-flags %1$s %2$s";

  void Apply(interpretter::ExecutionContext &ctx) const {
    ctx.current_frame().regs_.set(result,
                                  Apply(ctx.resolve(lhs), ctx.resolve(rhs)));
  }
  static ir::FlagsVal Apply(ir::FlagsVal lhs, ir::FlagsVal rhs) {
    return lhs & rhs;
  }

  ir::RegOr<ir::FlagsVal> lhs;
  ir::RegOr<ir::FlagsVal> rhs;
  ir::Reg result;
};

struct OrFlagsInstruction
    : base::Extend<OrFlagsInstruction>::With<ir::ByteCodeExtension,
                                             ir::InlineExtension,
                                             ir::DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "%3$s = or-flags %1$s %2$s";

  void Apply(interpretter::ExecutionContext &ctx) const {
    ctx.current_frame().regs_.set(result,
                                  Apply(ctx.resolve(lhs), ctx.resolve(rhs)));
  }
  static ir::FlagsVal Apply(ir::FlagsVal lhs, ir::FlagsVal rhs) {
    return lhs | rhs;
  }

  ir::RegOr<ir::FlagsVal> lhs;
  ir::RegOr<ir::FlagsVal> rhs;
  ir::Reg result;
};

}  // namespace type

#endif  // ICARUS_TYPE_FLAGS_H

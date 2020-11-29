#ifndef ICARUS_TYPE_ENUM_H
#define ICARUS_TYPE_ENUM_H

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
#include "module/module.h"
#include "type/type.h"
#include "type/typed_value.h"

namespace type {
struct Enum : type::LegacyType {
  using underlying_type = uint64_t;

  explicit Enum(module::BasicModule const *mod)
      : LegacyType(LegacyType::Flags{.is_default_initializable = 0,
                                     .is_copyable              = 1,
                                     .is_movable               = 1,
                                     .has_destructor           = 0}),
        mod_(mod) {}

  void SetMembers(absl::flat_hash_map<std::string, underlying_type> vals) {
    vals_ = std::move(vals);
    for (auto &[name, val] : vals_) { members_.emplace(val, name); }
  }

  void WriteTo(std::string *buf) const override;
  core::Bytes bytes(core::Arch const &arch) const override;
  core::Alignment alignment(core::Arch const &arch) const override;

  bool is_big() const override { return false; }

  Completeness completeness() const override { return completeness_; }
  void complete() { completeness_ = Completeness::Complete; }

  void Accept(VisitorBase *visitor, void *ret, void *arg_tuple) const override {
    visitor->ErasedVisit(this, ret, arg_tuple);
  }

  std::optional<underlying_type> Get(std::string_view name) const;
  Typed<underlying_type, Enum> EmitLiteral(std::string_view member_name) const;

  std::optional<std::string_view> name(underlying_type v) const {
    auto it = members_.find(v);
    if (it == members_.end()) return std::nullopt;
    return it->second;
  }

 private:
  // TODO: Use module or drop it.
  [[maybe_unused]] module::BasicModule const *mod_;
  Completeness completeness_;
  absl::flat_hash_map<std::string, underlying_type> vals_;
  absl::flat_hash_map<underlying_type, std::string_view> members_;
};

struct EnumInstruction
    : base::Extend<EnumInstruction>::With<ir::ByteCodeExtension,
                                          ir::InlineExtension> {
  void Apply(interpretter::ExecutionContext &ctx) const {
    absl::flat_hash_set<Enum::underlying_type> used_vals;

    for (auto const &[index, reg_or_value] : specified_values_) {
      used_vals.insert(ctx.resolve(reg_or_value));
    }

    absl::BitGen gen;

    absl::flat_hash_map<std::string, Enum::underlying_type> mapping;

    for (size_t i = 0; i < names_.size(); ++i) {
      auto iter = specified_values_.find(i);
      if (iter != specified_values_.end()) {
        mapping.emplace(names_[i], ctx.resolve(iter->second));
        continue;
      }

      bool success;
      Enum::underlying_type proposed_value;
      do {
        proposed_value = absl::Uniform<Enum::underlying_type>(gen);
        success        = used_vals.insert(proposed_value).second;
      } while (not success);
      mapping.try_emplace(std::string(names_[i]), proposed_value);
    }

    type->SetMembers(std::move(mapping));
    type->complete();
    ctx.current_frame().regs_.set(result, type::Type(type));
  }

  std::string to_string() const {
    using base::stringify;
    return absl::StrCat(stringify(result), " = enum (",
                        absl::StrJoin(names_, ", "), ")");
  }

  type::Enum *type;
  std::vector<std::string_view> names_;
  absl::flat_hash_map<uint64_t, ir::RegOr<uint64_t>> specified_values_;
  ir::Reg result;
};

}  // namespace type

#endif  // ICARUS_TYPE_ENUM_H

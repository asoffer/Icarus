#ifndef ICARUS_TYPE_FLAGS_H
#define ICARUS_TYPE_FLAGS_H

#include <optional>
#include <ostream>
#include <sstream>
#include <string>
#include <string_view>

#include "absl/container/flat_hash_map.h"
#include "base/debug.h"
#include "base/extend.h"
#include "base/extend/serialize.h"
#include "base/extend/traverse.h"
#include "base/traverse.h"
#include "ir/instruction/base.h"
#include "ir/instruction/debug.h"
#include "ir/instruction/inliner.h"
#include "module/module.h"
#include "type/primitive.h"
#include "type/type.h"
#include "type/typed_value.h"

namespace type {
struct Flags : public type::LegacyType {
  using underlying_type = uint64_t;
  static type::Type UnderlyingType() { return type::U64; }

  Flags(module::BasicModule const *mod)
      : LegacyType(LegacyType::Flags{.is_default_initializable = 1,
                                     .is_copyable              = 1,
                                     .is_movable               = 1,
                                     .has_destructor           = 0}),
        mod_(mod) {}

  void SetMembers(absl::flat_hash_map<std::string, underlying_type> vals) {
    vals_ = std::move(vals);
    for (auto &[name, value] : vals_) {
      All |= value;
      members_.emplace(value, name);
    }
  }

  bool is_big() const override { return false; }

  void WriteTo(std::string *buf) const override;
  core::Bytes bytes(core::Arch const &arch) const override;
  core::Alignment alignment(core::Arch const &arch) const override;

  Completeness completeness() const override { return completeness_; }
  void complete() { completeness_ = Completeness::Complete; }

  void Accept(VisitorBase *visitor, void *ret, void *arg_tuple) const override {
    visitor->ErasedVisit(this, ret, arg_tuple);
  }

  std::optional<underlying_type> Get(std::string_view name) const;
  Typed<underlying_type, Flags> EmitLiteral(std::string_view member_name) const;

  std::optional<std::string_view> name(underlying_type v) const {
    auto it = members_.find(v);
    if (it == members_.end()) return std::nullopt;
    return it->second;
  }

  module::BasicModule const *defining_module() const { return mod_; }

  bool IsDefaultInitializable() const { return false; }

  underlying_type All = 0;

  Completeness completeness_;

 private:
  module::BasicModule const *mod_;
  // TODO combine these into a single bidirectional map?
  absl::flat_hash_map<std::string, underlying_type> vals_;
  absl::flat_hash_map<underlying_type, std::string> members_;
};

struct FlagsInstruction
    : base::Extend<FlagsInstruction>::With<base::BaseSerializeExtension> {
  Type Resolve() const;

  friend std::ostream &operator<<(std::ostream &os, FlagsInstruction const &f) {
    return os << f.result << " = flags(" << absl::StrJoin(f.names_, ", ")
              << ")";
  }

  std::string to_string() const {
    std::stringstream ss;
    ss << *this;
    return std::move(ss).str();
  }

  friend void BaseTraverse(ir::Inliner &inliner, FlagsInstruction &inst) {
    for (auto &[unused, value] : inst.specified_values_) {
      base::Traverse(inliner, value);
    }
  }

  type::Flags *type;
  std::vector<std::string_view> names_;
  absl::flat_hash_map<uint64_t, ir::RegOr<uint64_t>> specified_values_;
  ir::Reg result;
};

struct XorFlagsInstruction
    : base::Extend<XorFlagsInstruction>::With<base::BaseSerializeExtension,
                                              base::BaseTraverseExtension,
                                              ir::DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "%3$s = xor-flags %1$s %2$s";

  Flags::underlying_type Resolve() const {
    return Apply(lhs.value(), rhs.value());
  }
  static Flags::underlying_type Apply(Flags::underlying_type lhs,
                                      Flags::underlying_type rhs) {
    return lhs ^ rhs;
  }

  ir::RegOr<Flags::underlying_type> lhs;
  ir::RegOr<Flags::underlying_type> rhs;
  ir::Reg result;
};

struct AndFlagsInstruction
    : base::Extend<AndFlagsInstruction>::With<base::BaseSerializeExtension,
                                              base::BaseTraverseExtension,
                                              ir::DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "%3$s = and-flags %1$s %2$s";

  Flags::underlying_type Resolve() const {
    return Apply(lhs.value(), rhs.value());
  }
  static Flags::underlying_type Apply(Flags::underlying_type lhs,
                                      Flags::underlying_type rhs) {
    return lhs & rhs;
  }

  ir::RegOr<Flags::underlying_type> lhs;
  ir::RegOr<Flags::underlying_type> rhs;
  ir::Reg result;
};

struct OrFlagsInstruction
    : base::Extend<OrFlagsInstruction>::With<base::BaseSerializeExtension,
                                             base::BaseTraverseExtension,
                                             ir::DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "%3$s = or-flags %1$s %2$s";

  Flags::underlying_type Resolve() const {
    return Apply(lhs.value(), rhs.value());
  }
  static Flags::underlying_type Apply(Flags::underlying_type lhs,
                                      Flags::underlying_type rhs) {
    return lhs | rhs;
  }

  ir::RegOr<Flags::underlying_type> lhs;
  ir::RegOr<Flags::underlying_type> rhs;
  ir::Reg result;
};

}  // namespace type

#endif  // ICARUS_TYPE_FLAGS_H

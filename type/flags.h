#ifndef ICARUS_TYPE_FLAGS_H
#define ICARUS_TYPE_FLAGS_H

#include <optional>
#include <string>
#include <string_view>

#include "absl/container/flat_hash_map.h"
#include "base/debug.h"
#include "base/extend.h"
#include "base/extend/serialize.h"
#include "ir/instruction/base.h"
#include "ir/instruction/debug.h"
#include "ir/instruction/inliner.h"
#include "ir/interpreter/byte_code_reader.h"
#include "ir/interpreter/byte_code_writer.h"
#include "module/module.h"
#include "type/type.h"
#include "type/typed_value.h"

namespace type {
struct Flags : public type::LegacyType {
  using underlying_type = uint64_t;

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
    : base::Extend<FlagsInstruction>::With<ir::InlineExtension> {
  Type Resolve() const;

  std::string to_string() const {
    using base::stringify;
    return absl::StrCat(stringify(result), " = flags (",
                        absl::StrJoin(names_, ", "), ")");
  }

  friend void BaseSerialize(interpreter::ByteCodeWriter &w,
                            FlagsInstruction const &inst) {
    std::vector<std::pair<uint64_t, ir::RegOr<uint64_t>>> const_avoiding_hack(
        inst.specified_values_.begin(), inst.specified_values_.end());
    base::Serialize(w, inst.type, inst.names_, const_avoiding_hack,
                    inst.result);
  }

  friend void BaseDeserialize(interpreter::ByteCodeReader &r,
                              FlagsInstruction &inst) {
    // TODO: When maps are fully supported, remove this hack and use the extension.
    std::vector<std::pair<uint64_t, ir::RegOr<uint64_t>>> const_avoiding_hack;
    base::Deserialize(r, inst.type, inst.names_, const_avoiding_hack,
                      inst.result);
    inst.specified_values_ = absl::flat_hash_map<uint64_t, ir::RegOr<uint64_t>>(
        const_avoiding_hack.begin(), const_avoiding_hack.end());
  }

  type::Flags *type;
  std::vector<std::string_view> names_;
  absl::flat_hash_map<uint64_t, ir::RegOr<uint64_t>> specified_values_;
  ir::Reg result;
};

struct XorFlagsInstruction
    : base::Extend<XorFlagsInstruction>::With<base::BaseSerializeExtension,
                                              ir::InlineExtension,
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
                                              ir::InlineExtension,
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
                                             ir::InlineExtension,
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

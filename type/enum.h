#ifndef ICARUS_TYPE_ENUM_H
#define ICARUS_TYPE_ENUM_H

#include <optional>
#include <string>
#include <string_view>

#include "absl/container/flat_hash_map.h"
#include "base/debug.h"
#include "base/extend.h"
#include "ir/byte_code_writer.h"
#include "ir/instruction/base.h"
#include "ir/instruction/debug.h"
#include "ir/instruction/inliner.h"
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

  module::BasicModule const *defining_module() const { return mod_; }

 private:
  module::BasicModule const *mod_;
  Completeness completeness_;
  absl::flat_hash_map<std::string, underlying_type> vals_;
  absl::flat_hash_map<underlying_type, std::string_view> members_;
};

struct EnumInstruction
    : base::Extend<EnumInstruction>::With<ir::ByteCodeExtension,
                                          ir::InlineExtension> {
  Type Resolve() const;

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

#ifndef ICARUS_TYPE_ENUM_H
#define ICARUS_TYPE_ENUM_H

#include <optional>
#include <ostream>
#include <sstream>
#include <string>
#include <string_view>

#include "absl/container/flat_hash_map.h"
#include "absl/strings/str_join.h"
#include "base/debug.h"
#include "base/extend.h"
#include "base/extend/serialize.h"
#include "base/iterator.h"
#include "base/traverse.h"
#include "ir/instruction/base.h"
#include "ir/instruction/debug.h"
#include "ir/instruction/inliner.h"
#include "module/module.h"
#include "type/primitive.h"
#include "type/type.h"
#include "type/typed_value.h"

namespace type {
struct Enum : type::LegacyType {
  using underlying_type = uint64_t;
  static type::Type UnderlyingType() { return type::U64; }

  explicit Enum(ir::ModuleId mod);

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

  std::optional<underlying_type> Get(std::string_view name) const;
  Typed<underlying_type, Enum> EmitLiteral(std::string_view member_name) const;

  std::optional<std::string_view> name(underlying_type v) const {
    auto it = members_.find(v);
    if (it == members_.end()) return std::nullopt;
    return it->second;
  }

  auto values() const {
    return base::iterator_range(vals_.begin(), vals_.end());
  }

  ir::ModuleId defining_module() const { return mod_; }

 private:
  ir::ModuleId mod_;
  Completeness completeness_;
  absl::flat_hash_map<std::string, underlying_type> vals_;
  absl::flat_hash_map<underlying_type, std::string_view> members_;
};

struct EnumInstruction
    : base::Extend<EnumInstruction>::With<base::BaseSerializeExtension> {
  Type Resolve() const;

  friend std::ostream &operator<<(std::ostream &os, EnumInstruction const &e) {
    return os << e.result << " = enum(" << absl::StrJoin(e.names_, ", ") << ")";
  }

  std::string to_string() const {
    std::stringstream ss;
    ss << *this;
    return std::move(ss).str();
  }

  friend void BaseTraverse(ir::Inliner &inliner, EnumInstruction &inst) {
    for (auto &[unused, value] : inst.specified_values_) {
      base::Traverse(inliner, value);
    }
  }

  type::Enum *type;
  std::vector<std::string_view> names_;
  absl::flat_hash_map<uint64_t, ir::RegOr<uint64_t>> specified_values_;
  ir::Reg result;
};

}  // namespace type

#endif  // ICARUS_TYPE_ENUM_H

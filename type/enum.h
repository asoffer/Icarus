#ifndef ICARUS_TYPE_ENUM_H
#define ICARUS_TYPE_ENUM_H

#include <optional>
#include <string>
#include <string_view>

#include "absl/container/flat_hash_map.h"
#include "base/debug.h"
#include "ir/value/enum_and_flags.h"
#include "module/module.h"
#include "type/type.h"
#include "type/typed_value.h"

namespace type {
struct Enum : type::LegacyType {
  explicit Enum(module::BasicModule const *mod)
      : LegacyType(LegacyType::Flags{.is_default_initializable = 0,
                                     .is_copyable              = 1,
                                     .is_movable               = 1,
                                     .has_destructor           = 0}),
        mod_(mod) {}

  ~Enum() override {}

  void SetMembers(absl::flat_hash_map<std::string, ir::EnumVal> vals) {
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

  std::optional<ir::EnumVal> Get(std::string_view name) const;
  Typed<ir::EnumVal, Enum> EmitLiteral(std::string_view member_name) const;

  std::optional<std::string_view> name(ir::EnumVal v) const {
    auto it = members_.find(v);
    if (it == members_.end()) return std::nullopt;
    return it->second;
  }

 private:
  // TODO: Use module or drop it.
  [[maybe_unused]] module::BasicModule const *mod_;
  Completeness completeness_;
  absl::flat_hash_map<std::string, ir::EnumVal> vals_;
  absl::flat_hash_map<ir::EnumVal, std::string_view> members_;
};
}  // namespace type

#endif  // ICARUS_TYPE_ENUM_H

#ifndef ICARUS_TYPE_ENUM_H
#define ICARUS_TYPE_ENUM_H

#include <optional>
#include <string_view>

#include "absl/container/flat_hash_map.h"
#include "base/debug.h"
#include "ir/values.h"
#include "type/type.h"
#include "type/typed_value.h"

namespace type {
struct Enum : public type::Type {
  TYPE_FNS(Enum);

  Enum(module::BasicModule const *mod,
       absl::flat_hash_map<std::string, ir::EnumVal> vals)
      : mod_(mod), vals_(std::move(vals)) {
    for (auto &[name, val] : vals_) { members_.emplace(val, name); }
  }

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

  bool IsDefaultInitializable() const { return false; }

  module::BasicModule const *mod_;

  ICARUS_PRIVATE
  // TODO combine these into a single bidirectional map?
  absl::flat_hash_map<std::string, ir::EnumVal> vals_;
  absl::flat_hash_map<ir::EnumVal, std::string_view> members_;
};
}  // namespace type

#endif  // ICARUS_TYPE_ENUM_H

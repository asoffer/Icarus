#ifndef ICARUS_TYPE_FLAGS_H
#define ICARUS_TYPE_FLAGS_H

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
struct Flags : public type::Type {
  TYPE_FNS(Flags);

  Flags(module::BasicModule const *mod)
      : Type(Type::Flags{.is_default_initializable = 1,
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
    DEBUG_LOG("flags")(v);
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
}  // namespace type

#endif  // ICARUS_TYPE_FLAGS_H

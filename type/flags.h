#ifndef ICARUS_TYPE_FLAGS_H
#define ICARUS_TYPE_FLAGS_H

#include <optional>
#include <string>
#include <string_view>

#include "absl/container/flat_hash_map.h"
#include "base/debug.h"
#include "ir/flags_val.h"
#include "type/type.h"
#include "type/typed_value.h"

namespace type {
struct Flags : public type::Type {
  TYPE_FNS(Flags);

  Flags(::Module const* mod,
        absl::flat_hash_map<std::string, ir::FlagsVal> vals)
      : mod_(mod), vals_(std::move(vals)) {
    for (auto& [name, val] : vals_) {
      All |= val.value;
      members_.emplace(val, name);
    }
    DEBUG_LOG("flags")(vals_);
    DEBUG_LOG("flags")(members_);
  }

#include "visitor/type_visitors.xmacro.h"

  std::optional<ir::FlagsVal> Get(std::string_view name) const;
  Typed<ir::FlagsVal, Flags> EmitLiteral(std::string_view member_name) const;

  std::optional<std::string_view> name(ir::FlagsVal v) const {
    DEBUG_LOG("flags")(v);
    auto it = members_.find(v);
    if (it == members_.end()) return std::nullopt;
    return it->second;
  }

  size_t All = 0;

  ICARUS_PRIVATE
  ::Module const* mod_;
  // TODO combine these into a single bidirectional map?
  absl::flat_hash_map<std::string, ir::FlagsVal> vals_;
  absl::flat_hash_map<ir::FlagsVal, std::string> members_;
};
}  // namespace type

#endif  // ICARUS_TYPE_FLAGS_H

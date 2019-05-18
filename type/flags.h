#ifndef ICARUS_TYPE_FLAGS_H
#define ICARUS_TYPE_FLAGS_H

#include <optional>
#include <random>
#include <string>

#include "absl/container/flat_hash_map.h"
#include "ir/flags_val.h"
#include "type/type.h"
#include "type/typed_value.h"

namespace type {
struct Flags : public type::Type {
  TYPE_FNS(Flags);

  Flags(::Module const* mod) : mod_(mod) {}

  Flags(
      absl::flat_hash_map<std::string, std::optional<int32_t>> const& members);

#include "visitor/type_visitors.xmacro.h"

  std::optional<ir::FlagsVal> Get(const std::string& str) const;
  Typed<ir::FlagsVal, Flags> EmitLiteral(std::string const& member_name) const;

  // TODO privatize
  absl::flat_hash_map<size_t, std::string> members_;

  size_t All = 0;

 private:
  friend struct IncompleteFlags;

  ::Module const *mod_;
  // TODO combine these into a single bidirectional map?
  absl::flat_hash_map<std::string, ir::FlagsVal> vals_;
};
}  // namespace type

#endif  // ICARUS_TYPE_FLAGS_H

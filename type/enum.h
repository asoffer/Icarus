#ifndef ICARUS_TYPE_ENUM_H
#define ICARUS_TYPE_ENUM_H

#include <unordered_set>

#include "base/container/unordered_map.h"
#include "type.h"
#include "typed_value.h"

namespace type {
struct Enum : public type::Type {
  TYPE_FNS(Enum);

  Enum(::Module const* mod) : mod_(mod) {}

  bool IsDefaultInitializable() const override { return false; }

  std::optional<ir::EnumVal> Get(const std::string& str) const;
  Typed<ir::EnumVal, Enum> EmitLiteral(std::string const& member_name) const ;

  // TODO privatize
  base::unordered_map<i32, std::string> members_;

 private:
  friend struct IncompleteEnum;
  ::Module const *mod_;
  // TODO combine these into a single bidirectional map?
  base::unordered_map<std::string, ir::EnumVal> vals_;
};
}  // namespace type

#endif  // ICARUS_TYPE_ENUM_H

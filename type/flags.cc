#include "type/flags.h"

#include <unordered_set>

#include "ir/addr.h"
#include "ir/cmd.h"
#include "ir/flags_val.h"
#include "ir/register.h"
#include "ir/val.h"

struct Module;
struct Context;

namespace type {

void Flags::EmitInit(ir::Register id_reg, Context *) const {
  ir::Store(ir::FlagsVal{0}, id_reg);
}

Flags::Flags(
    std::unordered_map<std::string, std::optional<int32_t>> const &members) {
  std::unordered_set<int32_t> taken;
  for (auto const &[s, v] : members) {
    if (v.has_value()) {
      vals_.emplace(s, ir::FlagsVal(size_t{1} << *v));
      members_.emplace(size_t{1} << *v, s);
      All |= (size_t{1} << *v);
    }
    taken.insert(*v);
  }
  // TODO we can highly optimize this in a number of ways. One simple thing is
  // removing members as we used them above.
  for (auto const &[s, v] : members) {
    if (v.has_value()) { continue; }
    std::random_device rd;
    std::uniform_int_distribution<int> dist(0, 31);
    int32_t x;
    {
    try_again:
      x            = dist(rd);
      bool success = taken.insert(x).second;
      if (!success) { goto try_again; }
    }
    vals_.emplace(s, ir::FlagsVal(size_t{1} << x));
    All |= (size_t{1} << x);
    members_.emplace(size_t{1} << x, s);
  }
}

std::optional<ir::FlagsVal> Flags::Get(const std::string &str) const {
  if (auto iter = vals_.find(str); iter != vals_.end()) { return iter->second; }
  return std::nullopt;
}

Typed<ir::FlagsVal, Flags> Flags::EmitLiteral(
    std::string const &member_name) const {
  return Typed<ir::FlagsVal, Flags>(vals_.at(member_name), this);
}

void Flags::EmitCopyAssign(Type const *from_type, ir::Results const &from,
                           ir::RegisterOr<ir::Addr> to, Context *) const {
  ASSERT(this == from_type);
  ir::Store(from.get<ir::FlagsVal>(0), to);
}

void Flags::EmitMoveAssign(Type const *from_type, ir::Results const &from,
                           ir::RegisterOr<ir::Addr> to, Context *) const {
  ASSERT(this == from_type);
  ir::Store(from.get<ir::FlagsVal>(0), to);
}

void Flags::defining_modules(
    std::unordered_set<::Module const *> *modules) const {
  NOT_YET();
}

void Flags::EmitRepr(ir::Results const &val, Context *) const {
  ir::Print(val.get<ir::FlagsVal>(0), this);
}

void Flags::WriteTo(std::string *result) const {
  result->append("flags.");
  result->append(std::to_string(reinterpret_cast<uintptr_t>(this)));
}

ir::Results Flags::PrepareArgument(Type const *from, ir::Results const &val,
                                   Context *ctx) const {
  ASSERT(from == this);
  return val;
}

// TODO make this the smallest size that fits.
layout::Bytes Flags::bytes(layout::Arch const &a) const {
  return layout::Bytes{8};
}

// TODO make this the smallest size that fits.
layout::Alignment Flags::alignment(layout::Arch const &a) const {
  return layout::Alignment{8};
}

Cmp Flags::Comparator() const { return Cmp::Order; }

}  // namespace type

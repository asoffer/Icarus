#include "type/enum.h"


#include "absl/container/flat_hash_set.h"

namespace type {

std::optional<ir::EnumVal> Enum::Get(std::string_view name) const {
  if (auto iter = vals_.find(name); iter != vals_.end()) {
    return iter->second;
  }
  return std::nullopt;
}

Typed<ir::EnumVal, Enum> Enum::EmitLiteral(std::string_view member_name) const {
  return Typed<ir::EnumVal, Enum>(vals_.at(member_name), this);
}

void Enum::WriteTo(std::string *result) const {
  result->append("enum.");
  result->append(std::to_string(reinterpret_cast<uintptr_t>(this)));
}

// TODO make this the smallest size that fits.
core::Bytes Enum::bytes(core::Arch const &a) const {
  return core::Bytes{8};
}

// TODO make this the smallest size that fits.
core::Alignment Enum::alignment(core::Arch const &a) const {
  return core::Alignment{8};
}

}  // namespace type

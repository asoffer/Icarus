#include "type/flags.h"

#include "absl/random/distributions.h"
#include "absl/random/random.h"

namespace type {

std::optional<Flags::underlying_type> Flags::Get(std::string_view name) const {
  if (auto iter = vals_.find(name); iter != vals_.end()) {
    return iter->second;
  }
  return std::nullopt;
}

Typed<Flags::underlying_type, Flags> Flags::EmitLiteral(
    std::string_view member_name) const {
  return Typed<underlying_type, Flags>(vals_.at(member_name), this);
}

void Flags::WriteTo(std::string *result) const {
  result->append("flags.");
  result->append(std::to_string(reinterpret_cast<uintptr_t>(this)));
}

// TODO make this the smallest size that fits.
core::Bytes Flags::bytes(core::Arch const &a) const {
  return core::Bytes::Get<underlying_type>();
}

// TODO make this the smallest size that fits.
core::Alignment Flags::alignment(core::Arch const &a) const {
  return core::Alignment::Get<underlying_type>();
}

}  // namespace type

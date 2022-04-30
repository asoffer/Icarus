#include "type/enum.h"

#include "absl/container/flat_hash_set.h"
#include "absl/random/distributions.h"
#include "absl/random/random.h"
#include "type/system.h"

namespace type {

std::optional<Enum::underlying_type> Enum::Get(std::string_view name) const {
  if (auto iter = vals_.find(name); iter != vals_.end()) {
    return iter->second;
  }
  return std::nullopt;
}

Typed<Enum::underlying_type, Enum> Enum::EmitLiteral(
    std::string_view member_name) const {
  return Typed<underlying_type, Enum>(vals_.at(member_name), this);
}

void Enum::WriteTo(std::string *result) const {
  result->append("enum.");
  result->append(std::to_string(reinterpret_cast<uintptr_t>(this)));
}

// TODO make this the smallest size that fits.
core::Bytes Enum::bytes(core::Arch const &a) const {
  return core::Bytes::Get<underlying_type>();
}

// TODO make this the smallest size that fits.
core::Alignment Enum::alignment(core::Arch const &a) const {
  return core::Alignment::Get<underlying_type>();
}

bool InterpretInstruction(ir::interpreter::Interpreter &interpreter,
                          EnumInstruction const &inst) {
  absl::flat_hash_set<Enum::underlying_type> used_vals;

  for (auto const &[index, reg_or_value] : inst.specified_values_) {
    used_vals.insert(reg_or_value.value());
  }

  absl::BitGen gen;

  absl::flat_hash_map<std::string, Enum::underlying_type> mapping;

  for (size_t i = 0; i < inst.names_.size(); ++i) {
    auto iter = inst.specified_values_.find(i);
    if (iter != inst.specified_values_.end()) {
      mapping.emplace(inst.names_[i], iter->second.value());
      continue;
    }

    bool success;
    Enum::underlying_type proposed_value;
    do {
      proposed_value = absl::Uniform<Enum::underlying_type>(gen);
      success        = used_vals.insert(proposed_value).second;
    } while (not success);
    mapping.try_emplace(std::string(inst.names_[i]), proposed_value);
  }

  inst.type->SetMembers(std::move(mapping));
  inst.type->complete();
  return true;
}

Enum::Enum(ir::ModuleId mod)
    : LegacyType(IndexOf<Enum>(),
                 LegacyType::Flags{.is_default_initializable = 0,
                                   .is_copyable              = 1,
                                   .is_movable               = 1,
                                   .has_destructor           = 0}),
      mod_(mod) {
  GlobalTypeSystem.insert(Type(this));
}

}  // namespace type

#include "type/flags.h"

#include "absl/random/distributions.h"
#include "absl/random/random.h"
#include "type/system.h"

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

bool InterpretInstruction(ir::interpreter::Interpreter &interpreter,
                          FlagsInstruction const &inst) {
  absl::flat_hash_set<Flags::underlying_type> used_vals;

  for (auto const &[index, reg_or_value] : inst.specified_values_) {
    used_vals.insert(interpreter.frame().resolve(reg_or_value));
  }

  absl::BitGen gen;

  absl::flat_hash_map<std::string, Flags::underlying_type> mapping;

  for (size_t i = 0; i < inst.names_.size(); ++i) {
    auto iter = inst.specified_values_.find(i);
    if (iter != inst.specified_values_.end()) {
      mapping.emplace(inst.names_[i],
                      interpreter.frame().resolve(iter->second));
      continue;
    }

    bool success;
    Flags::underlying_type proposed_value;
    do {
      proposed_value =
          Flags::underlying_type{1} << absl::Uniform<Flags::underlying_type>(
              absl::IntervalClosedOpen, gen, 0,
              std::numeric_limits<Flags::underlying_type>::digits);
      success = used_vals.insert(proposed_value).second;
    } while (not success);
    mapping.try_emplace(std::string(inst.names_[i]), proposed_value);
  }

  inst.type->SetMembers(std::move(mapping));
  inst.type->complete();
  return true;
}

Flags::Flags(ir::ModuleId mod)
    : LegacyType(IndexOf<Flags>(),
                 LegacyType::Flags{.is_default_initializable = 1,
                                   .is_copyable              = 1,
                                   .is_movable               = 1,
                                   .has_destructor           = 0}),
      mod_(mod) {
  GlobalTypeSystem.insert(Type(this));
}

}  // namespace type

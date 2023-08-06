#include "unique_type_table.h"

#include "nth/debug/debug.h"

namespace serialization {

void UniqueTypeTable::insert_enums(
    module::UniqueId module_id,
    google::protobuf::RepeatedPtrField<TypeSystem::EnumType> const& enums) {
  for (size_t i = 0; i < enums.size(); ++i) {
    enums_.emplace(std::pair<module::UniqueId, size_t>(module_id, i),
                   &enums[i]);
  }
}

UniqueTypeTable::EnumEntry UniqueTypeTable::find_enum(
    module::UniqueId module_id, size_t enum_index) {
  if (module_id == module::UniqueId::Self()) {
    NTH_ASSERT(local_enums_.size() > enum_index);
    return EnumEntry(&local_enums_[enum_index]);
  } else {
    auto iter = enums_.find(std::pair(module_id, enum_index));
    NTH_ASSERT(iter != enums_.end());
    return EnumEntry(iter->second);
  }
}

std::optional<uint64_t> UniqueTypeTable::EnumEntry::value(
    std::string_view name) const {
  auto iter = ptr_->enumerator().find(name);
  if (iter == ptr_->enumerator().end()) { return std::nullopt; }
  return iter->second;
}

std::pair<size_t, TypeSystem::EnumType const*> UniqueTypeTable::insert_enum(
    TypeSystem::EnumType e) {
  size_t module_id = local_enums_.size();
  auto* ptr        = local_enums_.Add();
  *ptr             = std::move(e);
  return std::pair(module_id, ptr);
}

}  // namespace serialization

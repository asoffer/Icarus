#include "unique_type_table.h"

#include "base/debug.h"

namespace serialization {

void UniqueTypeTable::insert_enums(
    ModuleIndex index,
    google::protobuf::RepeatedPtrField<TypeSystem::EnumType> const& enums) {
  for (size_t i = 0; i < enums.size(); ++i) {
    enums_.emplace(std::pair<ModuleIndex, size_t>(index, i), &enums[i]);
  }
}

UniqueTypeTable::EnumEntry UniqueTypeTable::find_enum(ModuleIndex index,
                                                      size_t enum_index) {
  if (index == ModuleIndex::Self()) {
    ASSERT(local_enums_.size() > enum_index);
    return EnumEntry(&local_enums_[enum_index]);
  } else {
    auto iter = enums_.find(std::pair(index, enum_index));
    ASSERT(iter != enums_.end());
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
  size_t index = local_enums_.size();
  auto* ptr = local_enums_.Add();
  *ptr = std::move(e);
  return std::pair(index, ptr);
}

}  // namespace serialization

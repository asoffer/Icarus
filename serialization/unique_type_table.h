#ifndef ICARUS_SERIALIZATION_UNIQUE_TYPE_TABLE_H
#define ICARUS_SERIALIZATION_UNIQUE_TYPE_TABLE_H

#include <deque>
#include <optional>
#include <string_view>
#include <utility>

#include "absl/container/flat_hash_map.h"
#include "serialization/module_index.h"
#include "serialization/type_system.pb.h"

namespace serialization {

struct UniqueTypeTable {
  void insert_enums(
      ModuleIndex index,
      google::protobuf::RepeatedPtrField<TypeSystem::EnumType> const& enums);

  std::pair<size_t, TypeSystem::EnumType const*> insert_enum(
      TypeSystem::EnumType e);

  google::protobuf::RepeatedPtrField<TypeSystem::EnumType> const& local_enums()
      const {
    return local_enums_;
  }

  struct EnumEntry {
    EnumEntry(EnumEntry const&)            = default;
    EnumEntry& operator=(EnumEntry const&) = default;

    std::optional<uint64_t> value(std::string_view name) const;

   private:
    friend UniqueTypeTable;

    EnumEntry() = delete;
    EnumEntry(TypeSystem::EnumType const* ptr) : ptr_(ptr) {}
    TypeSystem::EnumType const* ptr_;
  };

  EnumEntry find_enum(ModuleIndex index, size_t enum_index);

 private:
  absl::flat_hash_map<std::pair<ModuleIndex, size_t>,
                      TypeSystem::EnumType const*>
      enums_;
  google::protobuf::RepeatedPtrField<TypeSystem::EnumType> local_enums_;
};

}  // namespace serialization

#endif  // ICARUS_SERIALIZATION_UNIQUE_TYPE_TABLE_H

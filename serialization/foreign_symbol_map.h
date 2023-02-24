#ifndef ICARUS_SERIALIZATION_FOREIGN_SYMBOL_MAP_H
#define ICARUS_SERIALIZATION_FOREIGN_SYMBOL_MAP_H

#include <string>
#include <type_traits>

#include "core/type_system/type.h"
#include "nth/container/flyweight_map.h"
#include "serialization/function_index.h"
#include "serialization/proto/foreign_symbol_map.pb.h"

namespace serialization {

struct ForeignSymbol {
  core::Type type;
  std::string name;

  friend bool operator==(ForeignSymbol const&, ForeignSymbol const&) = default;
  friend bool operator!=(ForeignSymbol const&, ForeignSymbol const&) = default;

  template <typename H>
  friend H AbslHashValue(H h, ForeignSymbol const& symbol) {
    return H::combine(std::move(h), symbol.type, symbol.name);
  }
};

struct ForeignSymbolMap {
  explicit ForeignSymbolMap(void* type_system) : type_system_(type_system) {}

  std::pair<uint32_t, bool> insert(ForeignSymbol const& symbol);
  std::pair<uint32_t, bool> insert(ForeignSymbol&& symbol);

  uint32_t index(ForeignSymbol const&) const;
  uint32_t index(core::Type t, void (*fn_ptr)()) const;

  ForeignSymbol const& symbol(uint32_t index) const;

  std::type_identity_t<void (*)()> function_pointer(uint32_t index);

  static void Serialize(ForeignSymbolMap const& from,
                        proto::ForeignSymbolMap& to);
  static bool Deserialize(proto::ForeignSymbolMap const& from,
                          ForeignSymbolMap& to);

  template <typename TypeSystem>
  TypeSystem& get() {
    return *static_cast<TypeSystem*>(type_system_);
  }

 private:
  void Populate(nth::flyweight_map<ForeignSymbol, void (*)()>::iterator iter);

  nth::flyweight_map<ForeignSymbol, void (*)()> data_;
  absl::flat_hash_map<std::pair<core::Type, void (*)()>, uint32_t> index_;

  // While not impossible depending on the type system directly will hurt
  // compile-times sufficiently that it's worth avoiding.
  void* type_system_;
};

}  // namespace serialization

#endif  // ICARUS_SERIALIZATION_FOREIGN_SYMBOL_MAP_H

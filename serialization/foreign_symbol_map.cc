#include "serialization/foreign_symbol_map.h"

#include <dlfcn.h>

#include <utility>

#include "base/debug.h"

namespace serialization {

std::pair<uint32_t, bool> ForeignSymbolMap::insert(
    ForeignSymbol const& symbol) {
  auto [iter, inserted] = data_.try_emplace(symbol);
  if (inserted) { Populate(iter); }
  return std::pair(data_.index(iter), inserted);
}

std::pair<uint32_t, bool> ForeignSymbolMap::insert(ForeignSymbol&& symbol) {
  auto [iter, inserted] = data_.try_emplace(std::move(symbol));
  if (inserted) { Populate(iter); }
  return std::pair(data_.index(iter), inserted);
}

void ForeignSymbolMap::Serialize(ForeignSymbolMap const& from,
                                 proto::ForeignSymbolMap& to) {
  for (auto const& [symbol, ptr] : from.data_) {
    auto& sym = *to.add_symbols();
    sym.set_name(symbol.name);
    auto& type = *sym.mutable_type();
    type.set_index(symbol.type.index());
    type.set_category(symbol.type.category());
  }
}

bool ForeignSymbolMap::Deserialize(proto::ForeignSymbolMap const& from,
                                   ForeignSymbolMap& to) {
  for (auto const& s : from.symbols()) {
    auto [iter, inserted] = to.data_.try_emplace(ForeignSymbol{
        .type = core::Type(s.type().category(), s.type().index()),
        .name = s.name(),
    });
    if (inserted) {
      to.Populate(iter);
    } else {
      return false;
    }
  }
  return true;
}

uint32_t ForeignSymbolMap::index(void (*fn_ptr)()) const {
  LOG("", "%p %u %u", this, index_.size(), data_.size());
  auto iter = index_.find(fn_ptr);
  ASSERT(iter != index_.end());
  return iter->second;
}

uint32_t ForeignSymbolMap::index(ForeignSymbol const& s) const {
  auto iter = data_.find(s);
  ASSERT(iter != data_.end());
  return data_.index(iter);
}

ForeignSymbol const& ForeignSymbolMap::symbol(uint32_t index) const {
  ASSERT(index < data_.size());
  return data_.from_index(index).first;
}

std::type_identity_t<void (*)()> ForeignSymbolMap::function_pointer(
    uint32_t index) {
  return data_.from_index(index).second;
}

void ForeignSymbolMap::Populate(
    nth::flyweight_map<ForeignSymbol, void (*)()>::iterator iter) {
  dlerror();  // Clear previous errors.
  // NOTE: This reinterpret_cast is not allowed according to the C++ Standard,
  // but is guaranteed to be correct on POSIX compliant systems.
  iter->second = reinterpret_cast<void (*)()>(
      dlsym(RTLD_DEFAULT, iter->first.name.c_str()));
  char const* error = dlerror();

  index_.emplace(iter->second, data_.index(iter));

  // TODO: Handle errors.
  if (error != nullptr) { NOT_YET(iter->first.name); }
}

}  // namespace serialization

#ifndef ICARUS_IR_DESERIALIZE_H
#define ICARUS_IR_DESERIALIZE_H

#include <cstddef>
#include <span>
#include <string>

#include "common/foreign_function.h"
#include "common/integer.h"
#include "common/string_literal.h"
#include "ir/module.h"
#include "jasmin/core/serialization.h"
#include "nth/container/black_hole.h"
#include "nth/io/serialize/deserialize.h"
#include "nth/io/serialize/reader.h"

namespace ic {

template <nth::io::reader R>
struct ModuleDeserializer : jasmin::ProgramDeserializer, R {
  explicit ModuleDeserializer(std::string_view content) : R(content) {}

  friend bool NthDeserialize(ModuleDeserializer& d, std::string& s) {
    return d.deserialize_as_string(s);
  }

  friend bool NthDeserialize(ModuleDeserializer& d,
                             std::floating_point auto& x) {
    return nth::io::deserialize_fixed(d, x);
  }

  friend bool NthDeserialize(ModuleDeserializer& d, std::integral auto& x) {
    return nth::io::deserialize_integer(d, x);
  }

  template <typename T>
  friend bool NthDeserialize(ModuleDeserializer& d,
                             nth::flyweight_set<T>& set) {
    return nth::io::deserialize_sequence(d, set);
  }

  friend bool NthDeserialize(ModuleDeserializer& d, Module::Entry& symbol) {
    return false;
  }

  friend bool NthDeserialize(
      ModuleDeserializer& d,
      absl::flat_hash_map<Identifier, Module::Entry>& exported_symbols) {
    uint32_t identifier_count, symbol_count;
    if (not nth::io::deserialize_integer(d, identifier_count)) { return false; }
    if (not nth::io::deserialize_integer(d, symbol_count)) { return false; }

    std::vector<Identifier> translation;
    translation.reserve(identifier_count);
    for (uint32_t i = 0; i < identifier_count; ++i) {
      std::string id;
      nth::io::deserialize(d, id);
      translation.emplace_back(id);
    }

    exported_symbols.reserve(symbol_count);

    for (size_t i = 0; i < symbol_count; ++i) {
      uint32_t index;
      if (not nth::io::deserialize_fixed(d, index)) { return false; }
      auto [iter, inserted] = exported_symbols.try_emplace(translation[index]);
      if (not nth::io::deserialize(d, iter->second)) { return false; }
    }
    return true;
  }

  friend bool NthDeserialize(ModuleDeserializer& d, type::Type& t) {
    uint64_t n;
    static_assert(sizeof(n) == sizeof(t));
    if (not nth::io::deserialize_fixed(d, n)) { return false; }
    std::memcpy(&t, &n, sizeof(t));
    return true;
  }

  friend bool NthDeserialize(ModuleDeserializer& d, nth::enumeration auto& e) {
    return nth::io::deserialize_fixed(d, e);
  }

  friend bool NthDeserialize(ModuleDeserializer& d,
                             type::ParametersType::Parameter& p) {
    return nth::io::deserialize(d, p.name, p.type);
  }

  template <nth::io::deserializable_with<ModuleDeserializer> X,
            nth::io::deserializable_with<ModuleDeserializer> Y>
  friend bool NthDeserialize(ModuleDeserializer& d, std::pair<X, Y>& pair) {
    return nth::io::deserialize(d, pair.first) and
           nth::io::deserialize(d, pair.second);
  }

  template <nth::io::deserializable_with<ModuleDeserializer>... Ts>
  friend bool NthDeserialize(ModuleDeserializer& d, std::tuple<Ts...>& tuple) {
    return std::apply(
        [&](auto&... elements) {
          return (nth::io::deserialize(d, elements) and ...);
        },
        tuple);
  }

  template <nth::io::deserializable_with<ModuleDeserializer> T>
  friend bool NthDeserialize(ModuleDeserializer& d, std::vector<T>& v) {
    return nth::io::deserialize_sequence(d, v);
  }

  friend bool NthDeserialize(ModuleDeserializer& d, type::TypeSystem& ts) {
    type::TypeSystem scratch;
    if (not(nth::io::deserialize(
            d,  //
            nth::io::as_sequence(scratch.parameters),
            nth::io::as_sequence(scratch.returns),
            nth::io::as_sequence(scratch.functions),
            nth::io::as_sequence(scratch.pointee_types),
            nth::io::as_sequence(scratch.buffer_pointee_types),
            nth::io::as_sequence(scratch.slice_element_types)))) {
      return false;
    }
    ts.merge_from(scratch);
    return true;
  }

  friend bool NthDeserialize(ModuleDeserializer& d, Module& m) {
    absl::flat_hash_map<Identifier, Module::Entry> entries;
    nth::black_hole<StringLiteral> str_lits;
    nth::black_hole<ForeignFunction> foreign_fns;
    return nth::io::deserialize(
        d, nth::io::as_sequence(str_lits), type::GlobalTypeSystem(),
        nth::io::as_sequence(foreign_fns), entries, global_program);
  }

 private:
  bool deserialize_as_string(std::string& content) {
    uint32_t size;
    if (not nth::io::deserialize_integer(*this, size)) { return false; }
    content.resize(size, '\0');
    return this->read(std::span<std::byte>(
        reinterpret_cast<std::byte*>(content.data()), size));
  }
};

}  // namespace ic

#endif  // ICARUS_IR_DESERIALIZE_H

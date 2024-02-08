#ifndef ICARUS_IR_SERIALIZE_H
#define ICARUS_IR_SERIALIZE_H

#include <span>
#include <string_view>

#include "common/foreign_function.h"
#include "common/identifier.h"
#include "common/string_literal.h"
#include "ir/global_function_registry.h"
#include "ir/module.h"
#include "jasmin/core/serialization.h"
#include "nth/io/serialize/serialize.h"
#include "nth/io/serialize/writer.h"

namespace ic {

template <nth::io::writer W>
struct ModuleSerializer : jasmin::ProgramSerializer, W {
  explicit ModuleSerializer(std::string& s) : W(s) {}

  friend bool NthSerialize(ModuleSerializer& s, std::floating_point auto x) {
    return nth::io::serialize_fixed(s, x);
  }

  friend bool NthSerialize(ModuleSerializer& s, std::integral auto x) {
    return nth::io::serialize_integer(s, x);
  }

  friend bool NthSerialize(ModuleSerializer& s, nth::enumeration auto e) {
    return nth::io::serialize_fixed(s, e);
  }

  friend bool NthSerialize(ModuleSerializer& s, std::string_view lit) {
    return s.serialize_as_string(lit);
  }

  friend bool NthSerialize(ModuleSerializer& s, Module::Entry const& entry) {
    return false;
  }

  friend bool NthSerialize(
      ModuleSerializer& s,
      absl::flat_hash_map<Identifier, Module::Entry> const& exported_symbols) {
    // TODO: Container that supports multiplicity?
    uint32_t size = exported_symbols.size();
    if (not nth::io::serialize_integer(s, size)) { return false; }
    if (not nth::io::serialize_integer(s, size)) { return false; }

    std::vector<std::pair<Identifier const, Module::Entry> const*> pairs;
    pairs.reserve(size);
    for (auto const& pair : exported_symbols) { pairs.push_back(&pair); }
    std::sort(pairs.begin(), pairs.end(), [](auto const* lhs, auto const* rhs) {
      return static_cast<std::string_view>(lhs->first) <
             static_cast<std::string_view>(rhs->first);
    });
    for (auto const* pair : pairs) {
      std::string_view str = static_cast<std::string_view>(pair->first);
      if (not nth::io::serialize(s, str)) { return false; }
    }
    for (auto const* pair : pairs) {
      if (not nth::io::serialize(s, pair->second)) { return false; }
    }

    return true;
  }

  friend bool NthSerialize(ModuleSerializer& s, type::Type const& t) {
    uint64_t n;
    static_assert(sizeof(n) == sizeof(t));
    std::memcpy(&n, &t, sizeof(n));
    return nth::io::serialize_fixed(s, n);
  }

  friend bool NthSerialize(ModuleSerializer& s,
                           type::ParametersType::Parameter const& p) {
    return nth::io::serialize(s, p.name, p.type);
  }

  template <nth::io::serializable_with<ModuleSerializer> X,
            nth::io::serializable_with<ModuleSerializer> Y>
  friend bool NthSerialize(ModuleSerializer& s, std::pair<X, Y> const& pair) {
    return nth::io::serialize(s, pair.first, pair.second);
  }

  template <nth::io::serializable_with<ModuleSerializer>... Ts>
  friend bool NthSerialize(ModuleSerializer& s,
                           std::tuple<Ts...> const& tuple) {
    return std::apply(
        [&](auto&... elements) { return (nth::io::serialize(s, elements...)); },
        tuple);
  }

  template <nth::io::serializable_with<ModuleSerializer> T>
  friend bool NthSerialize(ModuleSerializer& s, std::vector<T> const& v) {
    return nth::io::serialize_sequence(s, v);
  }

  friend bool NthSerialize(ModuleSerializer& s, type::TypeSystem const& ts) {
    return nth::io::serialize(s,  //
                              nth::io::as_sequence(ts.parameters),
                              nth::io::as_sequence(ts.returns),
                              nth::io::as_sequence(ts.functions),
                              nth::io::as_sequence(ts.pointee_types),
                              nth::io::as_sequence(ts.buffer_pointee_types),
                              nth::io::as_sequence(ts.slice_element_types));
  }

  friend bool NthSerialize(ModuleSerializer& s, Module const&) {
    absl::flat_hash_map<Identifier, Module::Entry> entries;
    return nth::io::serialize(
        s, nth::io::as_sequence(StringLiteral::All()), type::GlobalTypeSystem(),
        nth::io::as_sequence(ForeignFunction::All()), entries, global_program);
  }

 private:
  bool serialize_as_string(std::string_view content) {
    std::span<std::byte const> span(
        reinterpret_cast<std::byte const*>(content.data()), content.size());
    return nth::io::serialize_integer(*this, content.size()) and
           this->write(span);
  }
};

}  // namespace ic

#endif  // ICARUS_IR_SERIALIZE_H

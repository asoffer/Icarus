#ifndef ICARUS_IR_DESERIALIZE_H
#define ICARUS_IR_DESERIALIZE_H

#include <cstddef>
#include <span>
#include <string>

#include "common/foreign_function.h"
#include "common/integer.h"
#include "common/string_literal.h"
#include "ir/foreign_function.h"
#include "ir/module.h"
#include "jasmin/core/serialization.h"
#include "nth/container/black_hole.h"
#include "nth/io/reader/reader.h"
#include "nth/io/serialize/deserialize.h"

namespace ic {

template <nth::io::reader R>
struct ModuleDeserializer : jasmin::ProgramFragmentDeserializer, R {
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

  friend bool NthDeserialize(ModuleDeserializer& d, Module::Entry& entry) {
    type::Type t;
    if (not nth::io::deserialize(d, t)) { return false; }
    entry.qualified_type = type::QualifiedType::Constant(t);
    switch (t.kind()) {
      case type::Type::Kind::Primitive: {
        auto p = t.AsPrimitive();
        switch (p.kind()) {
          case type::PrimitiveType::Kind::Bool: {
            bool x;
            if (not nth::io::deserialize(d, x)) { return false; }
            entry.value.emplace_back(x);
          } break;
          case type::PrimitiveType::Kind::Char: {
            char x;
            if (not nth::io::deserialize(d, x)) { return false; }
            entry.value.emplace_back(x);
          } break;
          case type::PrimitiveType::Kind::Byte: {
            std::byte x;
            if (not nth::io::deserialize(d, x)) { return false; }
            entry.value.emplace_back(x);
          } break;
          case type::PrimitiveType::Kind::I8: {
            int8_t x;
            if (not nth::io::deserialize(d, x)) { return false; }
            entry.value.emplace_back(x);
          } break;
          case type::PrimitiveType::Kind::I16: {
            int16_t x;
            if (not nth::io::deserialize(d, x)) { return false; }
            entry.value.emplace_back(x);
          } break;
          case type::PrimitiveType::Kind::I32: {
            int32_t x;
            if (not nth::io::deserialize(d, x)) { return false; }
            entry.value.emplace_back(x);
          } break;
          case type::PrimitiveType::Kind::I64: {
            int64_t x;
            if (not nth::io::deserialize(d, x)) { return false; }
            entry.value.emplace_back(x);
          } break;
          case type::PrimitiveType::Kind::U8: {
            uint8_t x;
            if (not nth::io::deserialize(d, x)) { return false; }
            entry.value.emplace_back(x);
          } break;
          case type::PrimitiveType::Kind::U16: {
            uint16_t x;
            if (not nth::io::deserialize(d, x)) { return false; }
            entry.value.emplace_back(x);
          } break;
          case type::PrimitiveType::Kind::U32: {
            uint32_t x;
            if (not nth::io::deserialize(d, x)) { return false; }
            entry.value.emplace_back(x);
          } break;
          case type::PrimitiveType::Kind::U64: {
            uint64_t x;
            if (not nth::io::deserialize(d, x)) { return false; }
            entry.value.emplace_back(x);
          } break;
          case type::PrimitiveType::Kind::Type: {
            type::Type x;
            if (not nth::io::deserialize(d, x)) { return false; }
            entry.value.emplace_back(x);
          } break;
          case type::PrimitiveType::Kind::Integer: {
            Integer x;
            if (not nth::io::deserialize(d, x)) { return false; }
            entry.value.emplace_back(x);
          } break;
          case type::PrimitiveType::Kind::Module: {
            ModuleId x;
            if (not nth::io::deserialize(d, x)) { return false; }
            entry.value.emplace_back(x);
          } break;
          case type::PrimitiveType::Kind::NullType: return true;
          default: NTH_UNIMPLEMENTED("{}") <<= {t};
        }
      } break;
      case type::Type::Kind::Function: {
        FunctionId id;
        if (not nth::io::deserialize(d, id)) { return false; }
        entry.value.emplace_back(&global_function_registry.function(id));
      } break;
      default: NTH_UNIMPLEMENTED("{}") <<= {t};
    }
    return true;
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
      auto [iter, inserted] = exported_symbols.try_emplace(translation[i]);
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
    nth::black_hole<StringLiteral> str_lits;
    nth::black_hole<ForeignFunction> foreign_fns;
    bool result = nth::io::deserialize(d, nth::io::as_sequence(str_lits),
                                       type::GlobalTypeSystem(),
                                       nth::io::as_sequence(foreign_fns));
    for (auto f : ForeignFunction::LatestGeneration()) {
      InsertForeignFunction(f.name(), f.type(), true);
    }

    return nth::io::deserialize(d, m.program(), m.entries());
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

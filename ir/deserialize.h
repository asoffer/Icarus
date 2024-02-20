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
#include "nth/numeric/integer.h"

namespace ic {

template <nth::io::reader R>
struct ModuleDeserializer : jasmin::ProgramFragmentDeserializer, R {
  explicit ModuleDeserializer(std::string_view content) : R(content) {}

  friend bool NthDeserialize(ModuleDeserializer& d, std::string& s) {
    s.clear();
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

  friend bool NthDeserialize(ModuleDeserializer& d, AnyValue& value) {
    type::Type t;
    if (not nth::io::deserialize(d, t)) { return false; }
    std::vector<jasmin::Value> values;
    t = d.reindexing_(t);
    switch (t.kind()) {
      case type::Type::Kind::Primitive: {
        auto p = t.AsPrimitive();
        switch (p.kind()) {
          case type::PrimitiveType::Kind::Bool: {
            bool x;
            if (not nth::io::deserialize(d, x)) { return false; }
            values.emplace_back(x);
          } break;
          case type::PrimitiveType::Kind::Char: {
            char x;
            if (not nth::io::deserialize(d, x)) { return false; }
            values.emplace_back(x);
          } break;
          case type::PrimitiveType::Kind::Byte: {
            std::byte x;
            if (not nth::io::deserialize(d, x)) { return false; }
            values.emplace_back(x);
          } break;
          case type::PrimitiveType::Kind::I8: {
            int8_t x;
            if (not nth::io::deserialize(d, x)) { return false; }
            values.emplace_back(x);
          } break;
          case type::PrimitiveType::Kind::I16: {
            int16_t x;
            if (not nth::io::deserialize(d, x)) { return false; }
            values.emplace_back(x);
          } break;
          case type::PrimitiveType::Kind::I32: {
            int32_t x;
            if (not nth::io::deserialize(d, x)) { return false; }
            values.emplace_back(x);
          } break;
          case type::PrimitiveType::Kind::I64: {
            int64_t x;
            if (not nth::io::deserialize(d, x)) { return false; }
            values.emplace_back(x);
          } break;
          case type::PrimitiveType::Kind::U8: {
            uint8_t x;
            if (not nth::io::deserialize(d, x)) { return false; }
            values.emplace_back(x);
          } break;
          case type::PrimitiveType::Kind::U16: {
            uint16_t x;
            if (not nth::io::deserialize(d, x)) { return false; }
            values.emplace_back(x);
          } break;
          case type::PrimitiveType::Kind::U32: {
            uint32_t x;
            if (not nth::io::deserialize(d, x)) { return false; }
            values.emplace_back(x);
          } break;
          case type::PrimitiveType::Kind::U64: {
            uint64_t x;
            if (not nth::io::deserialize(d, x)) { return false; }
            values.emplace_back(x);
          } break;
          case type::PrimitiveType::Kind::Type: {
            type::Type x;
            if (not nth::io::deserialize(d, x)) { return false; }
            values.emplace_back(x);
          } break;
          case type::PrimitiveType::Kind::Integer: {
            Integer x;
            if (not nth::io::deserialize(d, x)) { return false; }
            values.emplace_back(x);
          } break;
          case type::PrimitiveType::Kind::Module: {
            ModuleId x;
            if (not nth::io::deserialize(d, x)) { return false; }
            values.emplace_back(x);
          } break;
          case type::PrimitiveType::Kind::NullType: return true;
          default: NTH_UNIMPLEMENTED("{}") <<= {t};
        }
      } break;
      case type::Type::Kind::Function: {
        FunctionId id;
        if (not nth::io::deserialize(d, id)) { return false; }
        values.emplace_back(&global_function_registry.function(id));
      } break;
      case type::Type::Kind::Pointer: {
        // TODO: How do we serialize a pointer?
        return true;
      }
      default: NTH_UNIMPLEMENTED("{}") <<= {t};
    }
    value = AnyValue(t, values);
    return true;
  }

  friend bool NthDeserialize(
      ModuleDeserializer& d,
      absl::flat_hash_map<Identifier, AnyValue>& exported_symbols) {
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
      auto [iter, inserted] = exported_symbols.try_emplace(
          translation[i], AnyValue::JustType(type::Error));
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
    v.clear();
    return nth::io::deserialize_sequence(d, v);
  }

  friend bool NthDeserialize(ModuleDeserializer& d, type::TypeSystem& ts) {
    d.reindexing_.clear();
    type::TypeSystem scratch;
    if (not nth::io::deserialize(
            d,  //
            nth::io::as_sequence(scratch.parameters),
            nth::io::as_sequence(scratch.returns),
            nth::io::as_sequence(scratch.functions),
            nth::io::as_sequence(scratch.pointee_types),
            nth::io::as_sequence(scratch.buffer_pointee_types),
            nth::io::as_sequence(scratch.slice_element_types))) {
      return false;
    }
    d.reindexing_ = ts.merge_from(scratch);
    return true;
  }

  struct StringLiteralReindexTable : std::vector<StringLiteral> {};

  friend bool NthDeserialize(ModuleDeserializer& d,
                             StringLiteralReindexTable& table) {
    uint32_t size;
    if (not nth::io::deserialize_integer(d, size)) { return false; }
    std::string s;
    table.reserve(size);
    for (uint32_t i = 0; i < size; ++i) {
      if (not nth::io::deserialize(d, s)) { return false; }
      table.push_back(StringLiteral(std::move(s)));
    }
    return true;
  }

  friend bool NthDeserialize(ModuleDeserializer& d, ForeignFunction& f) {
    StringLiteral name;
    type::Type t;
    if (not nth::io::deserialize(d, name, t)) { return false; }
    f = ForeignFunction(name, d.reindexing_(t).AsFunction());
    return true;
  }

  friend bool NthDeserialize(ModuleDeserializer& d, Module& m) {
    d.str_lits_.clear();
    d.reindexing_.clear();

    nth::black_hole<ForeignFunction> foreign_fns;
    if (not nth::io::deserialize(d,  //
                                 d.str_lits_, type::GlobalTypeSystem(),
                                 nth::io::as_sequence(foreign_fns))) {
      return false;
    }

    for (auto f : ForeignFunction::LatestGeneration()) {
      InsertForeignFunction(f.name(), f.type(), true);
    }

    return nth::io::deserialize(d, m.program(), m.entries());
  }

 private:
  StringLiteralReindexTable str_lits_;
  type::TypeSystem::ReindexTable reindexing_;

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

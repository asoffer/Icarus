#ifndef ICARUS_IR_DESERIALIZE_H
#define ICARUS_IR_DESERIALIZE_H

#include <cstddef>
#include <span>
#include <string>

#include "common/constants.h"
#include "common/foreign_function.h"
#include "common/integer.h"
#include "common/result.h"
#include "common/to_bytes.h"
#include "ir/module.h"
#include "jasmin/core/function_registry.h"
#include "nth/container/black_hole.h"
#include "nth/io/deserialize/deserialize.h"
#include "nth/io/reader/reader.h"
#include "nth/numeric/integer.h"

namespace ic {

template <nth::io::reader R>
struct ModuleDeserializer : R {
  using nth_deserializer_result_type = Result;

  explicit ModuleDeserializer(std::string_view content,
                              SharedContext& context
                                  NTH_ATTRIBUTE(lifetimebound))
      : R(content), context_(context) {}

  friend Result NthDeserialize(ModuleDeserializer& d, std::string& s) {
    s.clear();
    return d.read_as_string(s);
  }

  friend Result NthDeserialize(ModuleDeserializer& d,
                             std::floating_point auto& x) {
    return Result(nth::io::read_fixed(d, x));
  }

  friend Result NthDeserialize(ModuleDeserializer& d, std::integral auto& x) {
    return Result(nth::io::read_integer(d, x));
  }

  template <typename T>
  friend Result NthDeserialize(ModuleDeserializer& d,
                             nth::flyweight_set<T>& set) {
    return nth::io::deserialize(d, nth::io::as_sequence(set));
  }

  friend Result NthDeserialize(ModuleDeserializer& d, AnyValue& value) {
    type::Type t;
    co_await nth::io::deserialize(d, t);
    std::vector<jasmin::Value> values;
    switch (t.kind()) {
      case type::Type::Kind::Primitive: {
        auto p = t.AsPrimitive();
        switch (p.primitive_kind()) {
          case type::PrimitiveType::Kind::Bool: {
            bool x;
            co_await nth::io::deserialize(d, x);
            values.emplace_back(x);
          } break;
          case type::PrimitiveType::Kind::Char: {
            char x;
            co_await nth::io::deserialize(d, x);
            values.emplace_back(x);
          } break;
          case type::PrimitiveType::Kind::Byte: {
            std::byte x;
            co_await nth::io::deserialize(d, x);
            values.emplace_back(x);
          } break;
          case type::PrimitiveType::Kind::I8: {
            int8_t x;
            co_await nth::io::deserialize(d, x);
            values.emplace_back(x);
          } break;
          case type::PrimitiveType::Kind::I16: {
            int16_t x;
            co_await nth::io::deserialize(d, x);
            values.emplace_back(x);
          } break;
          case type::PrimitiveType::Kind::I32: {
            int32_t x;
            co_await nth::io::deserialize(d, x);
            values.emplace_back(x);
          } break;
          case type::PrimitiveType::Kind::I64: {
            int64_t x;
            co_await nth::io::deserialize(d, x);
            values.emplace_back(x);
          } break;
          case type::PrimitiveType::Kind::U8: {
            uint8_t x;
            co_await nth::io::deserialize(d, x);
            values.emplace_back(x);
          } break;
          case type::PrimitiveType::Kind::U16: {
            uint16_t x;
            co_await nth::io::deserialize(d, x);
            values.emplace_back(x);
          } break;
          case type::PrimitiveType::Kind::U32: {
            uint32_t x;
            co_await nth::io::deserialize(d, x);
            values.emplace_back(x);
          } break;
          case type::PrimitiveType::Kind::U64: {
            uint64_t x;
            co_await nth::io::deserialize(d, x);
            values.emplace_back(x);
          } break;
          case type::PrimitiveType::Kind::Type: {
            type::Type x;
            co_await nth::io::deserialize(d, x);
            values.emplace_back(x);
          } break;
          case type::PrimitiveType::Kind::Integer: {
            Integer x;
            co_await nth::io::deserialize(d, x);
            values.emplace_back(x);
          } break;
          case type::PrimitiveType::Kind::Module: {
            ModuleId x;
            co_await nth::io::deserialize(d, x);
            values.emplace_back(x);
          } break;
          case type::PrimitiveType::Kind::NullType: co_return Result::success();
          default: NTH_UNIMPLEMENTED("{}") <<= {t};
        }
      } break;
      case type::Type::Kind::Function: {
        IrFunction* f;
        co_await nth::io::deserialize(d, f);
        values.emplace_back(f);
      } break;
      case type::Type::Kind::Pointer: {
        // TODO: How do we serialize a pointer?
        co_return Result::success();
      }
      default: NTH_UNIMPLEMENTED("{}") <<= {t};
    }
    value = AnyValue(t, values);
    co_return Result::success();
  }

  friend Result NthDeserialize(
      ModuleDeserializer& d,
      absl::flat_hash_map<Identifier, AnyValue>& exported_symbols) {
    uint32_t identifier_count, symbol_count;
    if (not nth::io::read_integer(d, identifier_count)) {
      co_return Result(false);
    }
    if (not nth::io::read_integer(d, symbol_count)) { co_return Result(false); }

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
      co_await nth::io::deserialize(d, iter->second);
    }
    co_return Result::success();
  }

  friend Result NthDeserialize(ModuleDeserializer& d,
                               nth::enumeration auto& e) {
    return nth::io::read_fixed(d, e);
  }

  struct StringLiteralReindexTable : std::vector<StringLiteral> {};

  friend Result NthDeserialize(ModuleDeserializer& d,
                               StringLiteralReindexTable& table) {
    uint32_t size;
    if (not nth::io::read_integer(d, size)) { co_return Result(false); }
    std::string s;
    table.reserve(size);
    for (uint32_t i = 0; i < size; ++i) {
      co_await nth::io::deserialize(d, s);
      table.push_back(StringLiteral(std::move(s)));
    }
    co_return Result::success();
  }

  friend Result NthDeserialize(ModuleDeserializer& d, ForeignFunction& f) {
    StringLiteral name;
    type::Type t;
    co_await nth::io::deserialize(d, name, t);
    f = ForeignFunction(name, t.AsFunction());
    co_return Result::success();
  }

  friend Result NthDeserialize(ModuleDeserializer& d, Module& m) {
    co_await nth::io::deserialize(d, GlobalConstantTable());
    co_await nth::io::deserialize(d, d.context_.foreign);
    co_await nth::io::deserialize(d, m.program());
    m.set_initializer(m.program().function("~"));
    co_await nth::io::deserialize(d, m.entries());
    co_return Result::success();
  }

  jasmin::FunctionRegistry& context(
      decltype(nth::type<jasmin::FunctionRegistry>)) {
    return context_.registry;
  }
 private:
  SharedContext& context_;

  Result read_as_string(std::string& content) {
    uint32_t size;
    if (not nth::io::read_integer(*this, size)) { return false; }
    content.resize(size, '\0');
    return Result(this->read(ToBytes(content)));
  }
};

}  // namespace ic

#endif  // ICARUS_IR_read_H

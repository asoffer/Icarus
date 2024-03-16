#ifndef ICARUS_IR_SERIALIZE_H
#define ICARUS_IR_SERIALIZE_H

#include <span>
#include <string_view>

#include "common/foreign_function.h"
#include "common/identifier.h"
#include "common/result.h"
#include "common/constants.h"
#include "common/to_bytes.h"
#include "ir/module.h"
#include "jasmin/core/function_registry.h"
#include "nth/io/serialize/serialize.h"
#include "nth/io/writer/writer.h"
#include "nth/numeric/integer.h"

namespace ic {

template <nth::io::writer W>
struct ModuleSerializer : W {
  using nth_serializer_result_type = Result;

  explicit ModuleSerializer(std::string& s,
                            SharedContext& context NTH_ATTRIBUTE(lifetimebound))
      : W(s), context_(context) {}

  friend Result NthSerialize(ModuleSerializer& s, std::floating_point auto x) {
    return nth::io::write_fixed(s, x);
  }

  friend Result NthSerialize(ModuleSerializer& s, std::integral auto x) {
    return nth::io::write_integer(s, x);
  }

  friend Result NthSerialize(ModuleSerializer& s, nth::enumeration auto e) {
    return nth::io::write_fixed(s, e);
  }

  friend Result NthSerialize(ModuleSerializer& s, std::string_view lit) {
    return s.serialize_as_string(lit);
  }

  friend Result NthSerialize(ModuleSerializer& s, AnyValue const& value) {
    type::Type t = value.type();
    co_await nth::io::serialize(s, t);
    switch (t.kind()) {
      case type::Type::Kind::Primitive: {
        auto p = t.AsPrimitive();
        switch (p.primitive_kind()) {
          case type::PrimitiveType::Kind::Bool:
            co_return nth::io::serialize(s, value.value()[0].as<bool>());
          case type::PrimitiveType::Kind::Char:
            co_return nth::io::serialize(s, value.value()[0].as<char>());
          case type::PrimitiveType::Kind::Byte:
            co_return nth::io::serialize(s, value.value()[0].as<std::byte>());
          case type::PrimitiveType::Kind::I8:
            co_return nth::io::serialize(s, value.value()[0].as<int8_t>());
          case type::PrimitiveType::Kind::I16:
            co_return nth::io::serialize(s, value.value()[0].as<int16_t>());
          case type::PrimitiveType::Kind::I32:
            co_return nth::io::serialize(s, value.value()[0].as<int32_t>());
          case type::PrimitiveType::Kind::I64:
            co_return nth::io::serialize(s, value.value()[0].as<int64_t>());
          case type::PrimitiveType::Kind::U8:
            co_return nth::io::serialize(s, value.value()[0].as<uint8_t>());
          case type::PrimitiveType::Kind::U16:
            co_return nth::io::serialize(s, value.value()[0].as<uint16_t>());
          case type::PrimitiveType::Kind::U32:
            co_return nth::io::serialize(s, value.value()[0].as<uint32_t>());
          case type::PrimitiveType::Kind::U64:
            co_return nth::io::serialize(s, value.value()[0].as<uint64_t>());
          case type::PrimitiveType::Kind::Type:
            co_return nth::io::serialize(s, value.value()[0].as<type::Type>());
          case type::PrimitiveType::Kind::Integer:
            co_return nth::io::serialize(s, value.value()[0].as<Integer>());
          case type::PrimitiveType::Kind::Module:
            co_return nth::io::serialize(s, value.value()[0].as<ModuleId>());
          case type::PrimitiveType::Kind::NullType: co_return Result::success();
          default: NTH_UNIMPLEMENTED("{}") <<= {t};
        }
      } break;
      case type::Type::Kind::DependentFunction:
      case type::Type::Kind::Function: {
        co_return nth::io::serialize(s,
                                     value.value()[0].as<IrFunction const*>());
      } break;
      case type::Type::Kind::Pointer: {
        // TODO: How do we serialize a pointer?
        co_return Result::success();
      }
      case type::Type::Kind::Pattern: NTH_UNIMPLEMENTED("{}") <<= {t};
      case type::Type::Kind::Refinement: NTH_UNIMPLEMENTED("{}") <<= {t};
      case type::Type::Kind::Opaque: NTH_UNIMPLEMENTED("{}") <<= {t};
      default: NTH_UNIMPLEMENTED("{} (kind = {})") <<= {t, t.kind()};
    }
  }

  friend Result NthSerialize(
      ModuleSerializer& s,
      absl::flat_hash_map<Identifier, AnyValue> const& exported_symbols) {
    // TODO: Container that supports multiplicity?
    uint32_t size = exported_symbols.size();
    
    if (not nth::io::write_integer(s, size)) { co_return Result(false); }
    if (not nth::io::write_integer(s, size)) { co_return Result(false); }

    std::vector<std::pair<Identifier const, AnyValue> const*> pairs;
    pairs.reserve(size);
    for (auto const& pair : exported_symbols) { pairs.push_back(&pair); }
    std::sort(pairs.begin(), pairs.end(), [](auto const* lhs, auto const* rhs) {
      return Result(static_cast<std::string const&>(lhs->first) <
                    static_cast<std::string const&>(rhs->first));
    });
    for (auto const* pair : pairs) {
      co_await nth::io::serialize(s, pair->first);
    }
    for (auto const* pair : pairs) {
      co_await nth::io::serialize(s, pair->second);
    }

    co_return Result::success();
  }

  friend Result NthSerialize(ModuleSerializer& s, Module const& module) {
    co_await nth::io::serialize(s, GlobalConstantTable());
    co_await nth::io::serialize(s, s.context_.foreign);
    co_await nth::io::serialize(s, module.program());
    co_return nth::io::serialize(s, module.entries());
  }

  jasmin::FunctionRegistry& context(
      decltype(nth::type<jasmin::FunctionRegistry>)) {
    return context_.registry;
  }

 private:
  SharedContext& context_;

  Result serialize_as_string(std::string_view content) {
    return Result(nth::io::write_integer(*this, content.size()) and
                  this->write(ToBytes(content)));
  }
};

}  // namespace ic

#endif  // ICARUS_IR_SERIALIZE_H

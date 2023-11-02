#include "type/type.h"

#include <cstring>
#include <utility>
#include <vector>

#include "nth/debug/debug.h"
#include "nth/utility/no_destructor.h"

namespace ic::type {
namespace {

nth::NoDestructor<TypeSystem> type_system;

uint64_t opaque_count = 0;

}  // namespace

TypeSystem const& GlobalTypeSystem() { return *type_system; }

#define IC_XMACRO_TYPE_KIND(k)                                                 \
  static_assert(sizeof(Type) == sizeof(k##Type));                              \
  static_assert(alignof(Type) == alignof(k##Type));                            \
                                                                               \
  k##Type Type::As##k() const {                                                \
    NTH_REQUIRE((v.debug), kind() == Kind::k);                                 \
    k##Type t;                                                                 \
    std::memcpy(&t, this, sizeof(Type));                                       \
    return t;                                                                  \
  }
#include "common/language/type_kind.xmacro.h"

ParametersType Parameters(std::vector<ParametersType::Parameter> const& p) {
  return ParametersType(
      type_system->parameters.index(type_system->parameters.insert(p).first));
}
ParametersType Parameters(std::vector<ParametersType::Parameter>&& p) {
  return ParametersType(type_system->parameters.index(
      type_system->parameters.insert(std::move(p)).first));
}

FunctionType Function(ParametersType pt, std::vector<Type>&& r, Evaluation e) {
  uint64_t rt = type_system->returns.index(
      type_system->returns.insert(std::move(r)).first);
  return FunctionType(type_system->functions.index(
      type_system->functions.insert({pt, rt, e}).first));
}

FunctionType Function(ParametersType pt, std::vector<Type> const& r,
                      Evaluation e) {
  uint64_t rt =
      type_system->returns.index(type_system->returns.insert(r).first);
  return FunctionType(type_system->functions.index(
      type_system->functions.insert({pt, rt, e}).first));
}

SliceType Slice(Type t) {
  return SliceType(type_system->slice_element_types.index(
      type_system->slice_element_types.insert(t).first));
}

PointerType Ptr(Type t) {
  return PointerType(type_system->pointee_types.index(
      type_system->pointee_types.insert(t).first));
}

BufferPointerType BufPtr(Type t) {
  return BufferPointerType(type_system->buffer_pointee_types.index(
      type_system->buffer_pointee_types.insert(t).first));
}

PatternType Pattern(Type t) {
  return PatternType(type_system->pattern_types.index(
      type_system->pattern_types.insert(t).first));
}

GenericFunctionType GenericFunction(Evaluation e, void const* fn) {
  return GenericFunctionType(type_system->generic_function_types.index(
      type_system->generic_function_types.insert(std::pair(fn, e)).first));
}

Type SliceType::element_type() const {
  return type_system->slice_element_types.from_index(data());
}

Type PatternType::match_type() const {
  return type_system->pattern_types.from_index(data());
}

Type PointerType::pointee() const {
  return type_system->pointee_types.from_index(data());
}

Type BufferPointerType::pointee() const {
  return type_system->buffer_pointee_types.from_index(data());
}

ParametersType FunctionType::parameters() const {
  return std::get<0>(type_system->functions.from_index(data()));
}
Evaluation FunctionType::evaluation() const {
  return std::get<2>(type_system->functions.from_index(data()));
}

std::vector<ParametersType::Parameter> const& ParametersType::operator*()
    const {
  return type_system->parameters.from_index(data());
}

size_t ParametersType::size() const {
  return type_system->parameters.from_index(data()).size();
}

std::vector<Type> const& FunctionType::returns() const {
  return type_system->returns.from_index(
      std::get<1>(type_system->functions.from_index(data())));
}

void const* GenericFunctionType::data() const {
  return type_system->generic_function_types.from_index(BasicType::data())
      .first;
}

Evaluation GenericFunctionType::evaluation() const {
  return type_system->generic_function_types.from_index(BasicType::data())
      .second;
}

OpaqueType Opaque() { return OpaqueType(opaque_count++); }

size_t JasminSize(Type t) {
  switch (t.kind()) {
    case Type::Kind::Primitive: return 1;
    case Type::Kind::Parameters: NTH_UNREACHABLE("{}") <<= {t};
    case Type::Kind::Function: return 1;
    case Type::Kind::Slice: return 2;
    case Type::Kind::Pointer: return 1;
    case Type::Kind::BufferPointer: return 1;
    case Type::Kind::Pattern: return 1;
    case Type::Kind::GenericFunction: return 1;
    case Type::Kind::Opaque: NTH_UNREACHABLE("{}") <<= {t};
  }
}

TypeContour Contour(Type t) {
  switch (t.kind()) {
    case Type::Kind::Primitive:
      switch (t.AsPrimitive().kind()) {
        case PrimitiveType::Kind::Module:
        case PrimitiveType::Kind::Type:
        case PrimitiveType::Kind::Integer:
        case PrimitiveType::Kind::Error: NTH_UNREACHABLE();
        case PrimitiveType::Kind::Bool:
        case PrimitiveType::Kind::Char:
        case PrimitiveType::Kind::Byte:
        case PrimitiveType::Kind::I8:
        case PrimitiveType::Kind::U8:
          return TypeContour(ByteWidth(1), Alignment(1));
        case PrimitiveType::Kind::I16:
        case PrimitiveType::Kind::U16:
          return TypeContour(ByteWidth(2), Alignment(2));
        case PrimitiveType::Kind::I32:
        case PrimitiveType::Kind::U32:
        case PrimitiveType::Kind::F32:
          return TypeContour(ByteWidth(4), Alignment(4));
        case PrimitiveType::Kind::I64:
        case PrimitiveType::Kind::U64:
        case PrimitiveType::Kind::F64:
          return TypeContour(ByteWidth(8), Alignment(8));
      }
    case Type::Kind::Slice: return TypeContour(ByteWidth(16), Alignment(8));
    case Type::Kind::Function:
    case Type::Kind::Pointer: 
    case Type::Kind::BufferPointer:
      return TypeContour(ByteWidth(8), Alignment(8));
    case Type::Kind::Opaque: NTH_UNREACHABLE("{}") <<= {t};
    default: NTH_UNIMPLEMENTED("{}") <<= {t};
  }
}

}  // namespace ic::type

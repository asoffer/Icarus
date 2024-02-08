#include "type/type.h"

#include <cstring>
#include <utility>
#include <vector>

#include "common/any_value.h"
#include "common/pattern.h"
#include "nth/debug/debug.h"
#include "nth/utility/no_destructor.h"

namespace ic::type {
namespace {

nth::NoDestructor<TypeSystem> type_system;

uint64_t opaque_count = 0;

}  // namespace

#define IC_XMACRO_TYPE_KIND(kind)                                              \
  Type::Type(kind##Type t) : data_(t.data_) {}
#include "common/language/type_kind.xmacro.h"

TypeSystem& GlobalTypeSystem() { return *type_system; }

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
  return type_system->parameter_type(p);
}

ParametersType Parameters(std::vector<ParametersType::Parameter>&& p) {
  return type_system->parameter_type(std::move(p));
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

SliceType Slice(Type t) { return type_system->slice_type(t); }
PointerType Ptr(Type t) { return type_system->pointer_type(t); }
BufferPointerType BufPtr(Type t) { return type_system->buffer_pointer_type(t); }

PatternType Pattern(Type t) {
  return PatternType(type_system->pattern_match_types.index(
      type_system->pattern_match_types.insert(t).first));
}

Type SliceType::element_type() const {
  return type_system->slice_element_types.from_index(data());
}

Type PointerType::pointee() const {
  return type_system->pointee_types.from_index(data());
}

Type BufferPointerType::pointee() const {
  return type_system->buffer_pointee_types.from_index(data());
}

Type PatternType::match_type() const {
  return type_system->pattern_match_types.from_index(data());
}

ParametersType FunctionType::parameters() const {
  return std::get<0>(type_system->functions.from_index(data()));
}
Evaluation FunctionType::evaluation() const {
  return std::get<2>(type_system->functions.from_index(data()));
}

Type RefinementType::underlying() const {
  return type_system->refinements.from_index(data()).first;
}

bool RefinementType::operator()(AnyValue const& v) const {
  auto const& pattern = type_system->refinements.from_index(data()).second;
  return pattern(v);
}

RefinementType Refinement(Type t, ::ic::Pattern p) {
  return RefinementType(type_system->refinements.index(
      type_system->refinements.insert({t, p}).first));
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

OpaqueType Opaque() { return OpaqueType(opaque_count++); }

size_t JasminSize(Type t) {
  switch (t.kind()) {
    case Type::Kind::Primitive: return 1;
    case Type::Kind::Parameters: NTH_UNREACHABLE("{}") <<= {t};
    case Type::Kind::Pattern:
    case Type::Kind::Function: return 1;
    case Type::Kind::Slice: return 2;
    case Type::Kind::Pointer: return 1;
    case Type::Kind::BufferPointer: return 1;
    case Type::Kind::Opaque: NTH_UNREACHABLE("{}") <<= {t};
    case Type::Kind::DependentFunction: NTH_UNREACHABLE("{}") <<= {t};
    case Type::Kind::Refinement:
      return JasminSize(t.AsRefinement().underlying());
  }
}

TypeContour Contour(Type t) {
  switch (t.kind()) {
    case Type::Kind::Primitive:
      switch (t.AsPrimitive().kind()) {
        case PrimitiveType::Kind::Error:
        case PrimitiveType::Kind::Bottom:
        default: NTH_UNREACHABLE();
        case PrimitiveType::Kind::NullType:
        case PrimitiveType::Kind::Scope_:
          return TypeContour(ByteWidth(0), Alignment(1));
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
        case PrimitiveType::Kind::Module:
        case PrimitiveType::Kind::Type:
        case PrimitiveType::Kind::Integer:
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
    case Type::Kind::Refinement: return Contour(t.AsRefinement().underlying());
    default: NTH_UNIMPLEMENTED("{}") <<= {t.kind()};
  }
}

auto ToUnderlying(PrimitiveType::Kind k) {
  return static_cast<std::underlying_type_t<PrimitiveType::Kind>>(k);
}

#define IC_XMACRO_PRIMITIVE_TYPE_BEGIN_CATEGORY(category_name)                 \
  bool category_name(PrimitiveType p) {                                        \
    auto value = ToUnderlying(p.kind());                                       \
    constexpr auto begin =                                                     \
        PrimitiveType::Kind::InternalBeginCategory_##category_name;            \
    constexpr auto end =                                                       \
        PrimitiveType::Kind::InternalEndCategory_##category_name;              \
    return ToUnderlying(begin) < value and value < ToUnderlying(end);          \
  }
#include "common/language/primitive_types.xmacro.h"

DependentFunctionType Dependent(DependentTerm const& term,
                                DependentParameterMapping const& mapping) {
  size_t term_index = type_system->dependent_terms.index(
      type_system->dependent_terms.insert(term).first);
  size_t mapping_index = type_system->dependent_mapping.index(
      type_system->dependent_mapping.insert(mapping).first);
  size_t pair_index = type_system->dependent_term_mapping_pairs.index(
      type_system->dependent_term_mapping_pairs
          .insert({term_index, mapping_index})
          .first);
  return DependentFunctionType(pair_index);
}

std::pair<DependentTerm const&, DependentParameterMapping const&>
DependentFunctionType::components() const {
  auto [term_index, mapping_index] =
      type_system->dependent_term_mapping_pairs.from_index(data());
  return std::pair<DependentTerm const&, DependentParameterMapping const&>(
      type_system->dependent_terms.from_index(term_index),
      type_system->dependent_mapping.from_index(term_index));
}

std::optional<Type> DependentFunctionType::operator()(
    std::span<AnyValue const> values) const {
  auto [term, mapping] = components();
  auto term_copy       = term;
  for (auto index : mapping) {
    switch (index.kind()) {
      case DependentParameterMapping::Index::Kind::Type:
        if (not term_copy.bind(AnyValue(Type_, values[index.index()].type()))) {
          return std::nullopt;
        }
        break;
      case DependentParameterMapping::Index::Kind::Value:
        if (not term_copy.bind(values[index.index()])) {
          return std::nullopt;
        }
        break;
    }
  }
  if (auto* v = term_copy.evaluate()) { return v->value()[0].as<Type>(); }
  return std::nullopt;
}

}  // namespace ic::type

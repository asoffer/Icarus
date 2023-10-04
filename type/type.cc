#include "type/type.h"

#include <cstring>
#include <utility>
#include <vector>

#include "nth/container/flyweight_set.h"
#include "nth/debug/debug.h"
#include "nth/utility/no_destructor.h"

namespace ic::type {
namespace {

nth::NoDestructor<nth::flyweight_set<std::vector<ParametersType::Parameter>>>
    parameters;
nth::NoDestructor<nth::flyweight_set<std::vector<Type>>> returns;
nth::NoDestructor<nth::flyweight_set<std::pair<ParametersType, uint64_t>>>
    functions;
nth::NoDestructor<nth::flyweight_set<Type>> slice_element_types;
nth::NoDestructor<nth::flyweight_set<Type>> pointee_types;
nth::NoDestructor<nth::flyweight_set<Type>> buffer_pointee_types;
nth::NoDestructor<nth::flyweight_set<Type>> pattern_types;

}  // namespace

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
#include "type/type_kind.xmacro.h"

Type::Kind Type::kind() const {
  return static_cast<Kind>((data_ >> 48) & 0xff);
}

size_t Size(Type t) {
  // TODO: This is not correct.
  return 1;
}

ParametersType Parameters(std::vector<ParametersType::Parameter> const& p) {
  return ParametersType(parameters->index(parameters->insert(p).first));
}
ParametersType Parameters(std::vector<ParametersType::Parameter>&& p) {
  return ParametersType(
      parameters->index(parameters->insert(std::move(p)).first));
}

FunctionType Function(ParametersType pt, std::vector<Type>&& r) {
  uint64_t rt = returns->index(returns->insert(std::move(r)).first);
  return FunctionType(functions->index(functions->insert({pt, rt}).first));
}

FunctionType Function(ParametersType pt, std::vector<Type> const& r) {
  uint64_t rt = returns->index(returns->insert(r).first);
  return FunctionType(functions->index(functions->insert({pt, rt}).first));
}

SliceType Slice(Type t) {
  return SliceType(
      slice_element_types->index(slice_element_types->insert(t).first));
}

PointerType Ptr(Type t) {
  return PointerType(pointee_types->index(pointee_types->insert(t).first));
}

BufferPointerType BufPtr(Type t) {
  return BufferPointerType(
      buffer_pointee_types->index(buffer_pointee_types->insert(t).first));
}

PatternType Pattern(Type t) {
  return PatternType(pattern_types->index(pattern_types->insert(t).first));
}

Type SliceType::element_type() const {
  return slice_element_types->from_index(data());
}

Type PatternType::match_type() const {
  return pattern_types->from_index(data());
}

Type PointerType::pointee() const { return pointee_types->from_index(data()); }

Type BufferPointerType::pointee() const {
  return buffer_pointee_types->from_index(data());
}

ParametersType FunctionType::parameters() const {
  return functions->from_index(data()).first;
}

std::vector<ParametersType::Parameter> const& ParametersType::operator*()
    const {
  return parameters->from_index(data());
}

std::vector<Type> const& FunctionType::returns() const {
  return ic::type::returns->from_index(functions->from_index(data()).second);
}

}  // namespace ic::type

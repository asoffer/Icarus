#ifndef ICARUS_TYPE_TYPE_SYSTEM_H
#define ICARUS_TYPE_TYPE_SYSTEM_H

#include <cstdint>
#include <tuple>
#include <utility>
#include <vector>

#include "common/pattern.h"
#include "nth/container/flyweight_set.h"
#include "type/basic.h"
#include "type/dependent.h"
#include "type/function.h"
#include "type/opaque.h"
#include "type/parameters.h"
#include "type/pattern.h"
#include "type/pointer.h"
#include "type/primitive.h"
#include "type/refinement.h"
#include "type/type_contour.h"

namespace ic::type {

struct TypeSystem {
  PointerType pointer_type(Type t);
  BufferPointerType buffer_pointer_type(Type t);
  SliceType slice_type(Type t);
  ParametersType parameter_type(
      std::vector<ParametersType::Parameter> const& p);
  ParametersType parameter_type(std::vector<ParametersType::Parameter>&& p);

  nth::flyweight_set<std::vector<ParametersType::Parameter>> parameters;
  nth::flyweight_set<std::vector<Type>> returns;

  nth::flyweight_set<std::tuple<ParametersType, uint64_t, Evaluation>>
      functions;
  nth::flyweight_set<Type> slice_element_types;
  nth::flyweight_set<Type> pointee_types;
  nth::flyweight_set<Type> buffer_pointee_types;
  nth::flyweight_set<DependentTerm> dependent_terms;
  nth::flyweight_set<DependentParameterMapping> dependent_mapping;
  nth::flyweight_set<std::pair<size_t, size_t>> dependent_term_mapping_pairs;
  nth::flyweight_set<std::pair<Type, ::ic::Pattern>> refinements;
  nth::flyweight_set<Type> pattern_match_types;

  void merge_from(TypeSystem const& ts);
};

Type Reindex(Type t, TypeSystem const& from, TypeSystem& to);

}  // namespace ic::type

#endif  // ICARUS_TYPE_TYPE_SYSTEM_H

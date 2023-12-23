#ifndef ICARUS_TYPE_TYPE_H
#define ICARUS_TYPE_TYPE_H

#include <cstdint>
#include <tuple>
#include <utility>
#include <vector>

#include "ir/function.h"
#include "nth/container/flyweight_set.h"
#include "type/basic.h"
#include "type/dependent.h"
#include "type/function.h"
#include "type/opaque.h"
#include "type/parameters.h"
#include "type/pointer.h"
#include "type/primitive.h"
#include "type/type_contour.h"
#include "type/type_system.pb.h"

namespace ic::type {

void NthPrint(auto& p, auto& f, Type t) {
  switch (t.kind()) {
#define IC_XMACRO_TYPE_KIND(kind)                                              \
  case Type::Kind::kind:                                                       \
    f(p, t.As##kind());                                                        \
    return;
#include "common/language/type_kind.xmacro.h"
  }
}

struct TypeSystem {
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
};

TypeSystem const& GlobalTypeSystem();

Type Family(Type t);

}  // namespace ic::type

#endif  // ICARUS_TYPE_TYPE_H

#ifndef ICARUS_TYPE_DEPENDENT_H
#define ICARUS_TYPE_DEPENDENT_H

#include <cstdint>
#include <vector>

#include "ir/type_erased_value.h"
#include "nth/container/flyweight_set.h"
#include "nth/container/interval.h"
#include "type/basic.h"

namespace ic::type {
namespace internal_dependent {

struct Term {
  struct Node {
    enum class Kind : uint8_t { Function, FunctionCall, DeBruijnIndex, Value };
    Kind kind;
    uint16_t index;
    uint32_t subtree_size = 1;
  };

  static Term DeBruijnIndex(uint16_t index);
  static Term Function(Term const &type, Term term);
  static Term Value(TypeErasedValue const &value);
  static Term Call(Term const &type, Term f);

  void specialize(TypeErasedValue const &value);

  // Returns a pointer to a fully-evaluated value if the expression can be
  // completely evaluated, and null otherwise.
  TypeErasedValue const *evaluate() const;

 private:
  void Substitute(size_t index,
                  nth::interval<std::vector<Node>::reverse_iterator> range);

  static TypeErasedValue Call(TypeErasedValue const &f,
                              TypeErasedValue const &v);

  void PartiallyEvaluate();

  std::vector<Node> nodes_;
  nth::flyweight_set<TypeErasedValue> values_;
};

}  // namespace internal_dependent
}  // namespace ic::type

#endif  // ICARUS_TYPE_DEPENDENT_H

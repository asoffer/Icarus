#ifndef ICARUS_TYPE_DEPENDENT_H
#define ICARUS_TYPE_DEPENDENT_H

#include <cstdint>
#include <vector>

#include "common/any_value.h"
#include "nth/container/flyweight_set.h"
#include "nth/container/interval.h"
#include "type/basic.h"

namespace ic::type {

struct DependentTerm {
  struct Node {
    enum class Kind : uint8_t { Function, FunctionCall, DeBruijnIndex, Value };
    Kind kind;
    uint16_t index;
    uint32_t subtree_size = 1;
  };

  static DependentTerm DeBruijnIndex(uint16_t index);
  static DependentTerm Function(DependentTerm const &type, DependentTerm term);
  static DependentTerm Value(AnyValue const &value);
  static DependentTerm Call(DependentTerm const &type, DependentTerm f);

  template <typename H>
  friend H AbslHashValue(H h, DependentTerm const &term) {
    for (auto const &node : term.nodes_) {
      if (node.kind == Node::Kind::Value) {
        h = H::combine(std::move(h), term.values_.from_index(node.index));
      } else {
        h = H::combine(std::move(h), node.kind, node.index, node.subtree_size);
      }
    }
    return h;
  }

  friend bool operator==(DependentTerm const &lhs, DependentTerm const &rhs);

  // Partially evaluates the term (which must represent a function) at the given
  // `value`. If `bind` returns `false`, `*this` will not have been modified.
  bool bind(AnyValue const &value);

  // Returns a pointer to a fully-evaluated value if the expression can be
  // completely evaluated, and null otherwise.
  AnyValue const *evaluate() const;

 private:
  void Substitute(size_t index,
                  nth::interval<std::vector<Node>::reverse_iterator> range);

  static AnyValue Call(AnyValue const &f, AnyValue const &v);

  void PartiallyEvaluate();

  std::vector<Node> nodes_;
  nth::flyweight_set<AnyValue> values_;
};

struct DependentParameterMapping {
  struct Index {
    static Index Type(uint16_t n);
    static Index Value(uint16_t n);

    enum class Kind : uint8_t { Type, Value };

    constexpr Kind kind() const { return kind_; }
    constexpr uint16_t index() const { return index_; }

    friend bool operator==(Index, Index) = default;

    template <typename H>
    friend H AbslHashValue(H h, Index i) {
      return H::combine(std::move(h), i.kind_, i.index_);
    }

   private:
    explicit constexpr Index(Kind k, uint16_t value)
        : kind_(k), index_(value) {}

    Kind kind_;
    uint16_t index_;
  };

  auto begin() const { return indices_.begin(); }
  auto end() const { return indices_.end(); }

  explicit DependentParameterMapping(std::vector<Index> indices)
      : indices_(std::move(indices)) {}

  Index operator[](size_t n) const {
    NTH_REQUIRE((v.harden), n < indices_.size());
    return indices_[n];
  }

  friend bool operator==(DependentParameterMapping const &,
                         DependentParameterMapping const &) = default;
  template <typename H>
  friend H AbslHashValue(H h, DependentParameterMapping const &m) {
    return H::combine(std::move(h), m.indices_);
  }

 private:
  std::vector<Index> indices_;
};

struct DependentFunctionType : internal_type::BasicType {
  std::optional<Type> operator()(std::span<AnyValue const>) const;

 private:
  friend DependentFunctionType Dependent(DependentTerm const &,
                                         DependentParameterMapping const &);
  friend Type;
  explicit constexpr DependentFunctionType(size_t index = 0)
      : internal_type::BasicType(Type::Kind::DependentFunction, index) {}

  std::pair<DependentTerm const &, DependentParameterMapping const &>
  components() const;
};

DependentFunctionType Dependent(DependentTerm const &term,
                                DependentParameterMapping const &mapping);

}  // namespace ic::type

#endif  // ICARUS_TYPE_DEPENDENT_H

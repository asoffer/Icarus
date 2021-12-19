#ifndef ICARUS_COMPILER_BOUND_PARAMETERS_H
#define ICARUS_COMPILER_BOUND_PARAMETERS_H

#include <limits>
#include <utility>

#include "absl/container/btree_map.h"
#include "ast/declaration.h"
#include "ir/value/result_buffer.h"
#include "type/qual_type.h"

namespace compiler {
// Represents a set of parameters to a generic parameterized expression (e.g.,
// function or scope), some of which are compile-time constants and bound to a
// particular value. Bindings can be applied either to the type of a
// declaration-identifier, or to the value of a declaration-identifier itself.
// For example,
//
// ```
// f ::= (N :: integer, a: [N; i64]) -> () { ... }
// x ::= [1, 4, 9]
// f(3, x)
// ```
//
// In the call to `f` above, the bound parameters would represent the value `3`
// bound to the value of `N`, and the value `[3; i64]` bound to the type of `a`.
// Even though `x` is a constant, it is bound to the non-constant parameter `a`,
// so its value is not represented, as part of the constant-bindings, only its
// type.
//
// BoundParameters are often keys in associative containers, where the
// corresponding mapped value holds contextual information need to compile an
// instantiation of a generic expression.
struct BoundParameters {
 private:
  struct BindingData {
    // Type bound to the parameter.
    type::QualType parameter_type;

    // Index of the relevant value in `buffer_`, or the maximum possible value
    // of type `size_t` if no value is bound.
    size_t index = std::numeric_limits<size_t>::max();

    // Hash of the value at `index`, or -1 if no value is bound.
    size_t hash_value = 0;

    void bind(ir::CompleteResultBuffer &buffer,
              ir::CompleteResultRef const &ref) {
      index      = buffer.num_entries();
      hash_value = parameter_type.type().HashValue(ref);
      buffer.append(ref);
    }
  };

 public:
  // Binds a type to the given identifier. `bind_type` must not have been called
  // with the same `id` as its first argument previously on this object.
  void bind_type(ast::Declaration::Id const *id, type::QualType qt);

  // Binds a value to the given identifier. `bind_type` must have been
  // previously called with the same `id`, but `bind_value` must not have been
  // called with the same `id`. the value referenced by `ref` will be
  // interpreted as having the type passed to `bind_type`.
  void bind_value(ast::Declaration::Id const *id,
                  ir::CompleteResultRef const &ref);

  struct BoundParameterReference {
    explicit operator bool() const { return data_; }

    type::QualType qual_type() {
      return ASSERT_NOT_NULL(data_)->parameter_type;
    }
    ir::CompleteResultRef value() { return value_; }

   private:
    friend BoundParameters;

    BoundParameterReference(BindingData const *data,
                            ir::CompleteResultRef value)
        : data_(data), value_(value) {}

    BindingData const *data_;
    ir::CompleteResultRef value_;
  };
  BoundParameterReference binding(ast::Declaration::Id const *id) const;

  size_t size() const { return bindings_.size(); }

  friend bool operator==(BoundParameters const &lhs,
                         BoundParameters const &rhs);

  friend bool operator!=(BoundParameters const &lhs,
                         BoundParameters const &rhs) {
    return not(lhs == rhs);
  }

  template <typename H>
  friend H AbslHashValue(H h, BoundParameters const &b) {
    for (auto const &[id, data] : b.bindings_) {
      h = H::combine(std::move(h), id, data.parameter_type, data.hash_value);
    }
    return h;
  }

 private:
  absl::btree_map<ast::Declaration::Id const *, BindingData> bindings_;
  ir::CompleteResultBuffer buffer_;
};

}  // namespace compiler

#endif  // ICARUS_COMPILER_BOUND_PARAMETERS_H

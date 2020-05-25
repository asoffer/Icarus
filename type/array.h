#ifndef ICARUS_TYPE_ARRAY_H
#define ICARUS_TYPE_ARRAY_H

#include "absl/container/node_hash_set.h"
#include "base/lazy.h"
#include "core/arch.h"
#include "ir/value/native_fn.h"
#include "type/type.h"

namespace type {

// `Array` is a type representing a fixed number (the `length`) of contiguous
// values of a given type (the `data_type`).
struct Array : Type {
  // Construct a new array from the given parameters, or if one already exists
  // in the cache, return that.
  friend Array const *Arr(size_t len, Type const *t);

  ~Array() override {}

  bool DeepCompleteImpl(absl::flat_hash_set<Type const *> &ts) const override;

  void WriteTo(std::string *buf) const override;
  core::Bytes bytes(core::Arch const &arch) const override;
  core::Alignment alignment(core::Arch const &arch) const override;

  void Accept(VisitorBase *visitor, void *ret, void *arg_tuple) const override {
    visitor->ErasedVisit(this, ret, arg_tuple);
  }

  constexpr size_t length() const { return len_; }
  constexpr type::Type const *data_type() const { return data_type_; }

  template <typename H>
  friend H AbslHashValue(H h, Array a) {
    return H::combine(std::move(h), a.length(), a.data_type());
  }

  friend bool operator==(Array const &lhs, Array const &rhs) {
    return lhs.length() == rhs.length() and lhs.data_type() == rhs.data_type();
  }

  friend bool operator!=(Array const &lhs, Array const &rhs) {
    return not(lhs == rhs);
  }

 private:
  explicit Array(size_t l, Type const *t)
      : Type(t->flags()), len_(l), data_type_(t) {}

  size_t len_;
  Type const *data_type_;
};

Array const *Arr(size_t len, Type const *t);

}  // namespace type
#endif  // ICARUS_TYPE_ARRAY_H

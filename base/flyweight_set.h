#ifndef ICARUS_BASE_FLYWEIGHT_SET_H
#define ICARUS_BASE_FLYWEIGHT_SET_H

#include <concepts>
#include <deque>
#include <initializer_list>
#include <utility>

#include "absl/container/flat_hash_set.h"
#include "base/debug.h"
#include "base/meta.h"

namespace base {
namespace internal_flyweight_set {
// We use a wrapper struct so that transparent hashing can distinguish the
// difference between a `size_t` and an index which happens to be implemented as
// such referring to a value of type `size_t`.
struct Index {
  size_t value;
};
}  // namespace internal_flyweight_set

// `flyweight_set<V>` is an ordered container, where the keys are guaranteed to
// be distinct. Pointers to elements in the container are never invalidated
// other than by assignment or calls to `clear`. Iteration occurs in the order
// the elements are inserted.
template <typename V, Hasher<V> Hash = absl::Hash<V>,
          std::equivalence_relation<V, V> Eq = std::equal_to<V>>
struct flyweight_set {
  using value_type = V;
  using size_type  = size_t;
  using hasher     = Hash;
  using key_equal  = Eq;

  using iterator       = typename std::deque<value_type>::const_iterator;
  using const_iterator = typename std::deque<value_type>::const_iterator;
  using reverse_iterator =
      typename std::deque<value_type>::const_reverse_iterator;
  using const_reverse_iterator = reverse_iterator;

  using reference       = typename std::deque<value_type>::reference;
  using const_reference = typename std::deque<value_type>::const_reference;
  using pointer         = value_type const*;
  using const_pointer   = value_type const*;

  flyweight_set() : set_(0, H(&values_), E(&values_)) {}
  flyweight_set(flyweight_set&& s)
      : values_(std::move(s.values_)), set_(std::move(s.set_)) {
    set_.hash_function().set_deque_pointer(&values_);
    set_.key_eq().set_deque_pointer(&values_);
  }

  flyweight_set& operator=(flyweight_set&& s) {
    values_ = std::move(s.values_);
    set_    = std::move(s.set_);
    set_.hash_function().set_deque_pointer(&values_);
    set_.key_eq().set_deque_pointer(&values_);
    return *this;
  }

  template <std::input_iterator Iter>
  flyweight_set(Iter b, Iter e) : set_(0, H(&values_), E(&values_)) {
    for (auto i = b; i < e; ++i) { insert(*i); }
  }

  flyweight_set(flyweight_set const& f) : flyweight_set(f.begin(), f.end()) {}
  flyweight_set& operator=(flyweight_set const& f) {
    values_.clear();
    set_.clear();
    set_.hash_function().set_deque_pointer(&values_);
    set_.key_eq().set_deque_pointer(&values_);
    for (auto i = f.begin(); i < f.end(); ++i) { insert(*i); }
    return *this;
  }

  flyweight_set(std::initializer_list<value_type> l)
      : flyweight_set(l.begin(), l.end()) {}

  size_type size() const { return values_.size(); }
  bool empty() const { return values_.empty(); }

  void clear() {
    values_.clear();
    set_.clear();
  }

  // Iterators traverse elements in the order they were inserted.
  iterator begin() { return values_.begin(); }
  iterator end() { return values_.end(); }
  const_iterator begin() const { return values_.cbegin(); }
  const_iterator end() const { return values_.cend(); }
  const_iterator cbegin() const { return values_.cbegin(); }
  const_iterator cend() const { return values_.cend(); }
  reverse_iterator rbegin() { return values_.rbegin(); }
  reverse_iterator rend() { return values_.rend(); }
  const_reverse_iterator rbegin() const { return values_.rbegin(); }
  const_reverse_iterator rend() const { return values_.rend(); }
  const_reverse_iterator crbegin() const { return values_.crbegin(); }
  const_reverse_iterator crend() const { return values_.crend(); }

  // Returns a reference to the first inserted object. Behavior is undefiend if
  // the container is empty.
  const_reference front() const {
    ASSERT(values_.size() != 0);
    return values_.front();
  }
  reference front() {
    ASSERT(values_.size() != 0);
    return values_.front();
  }

  // Returns a reference to the most recently inserted object. Behavior is
  // undefined if the container is empty.
  const_reference back() const {
    ASSERT(values_.size() != 0);
    return values_.back();
  }
  reference back() {
    ASSERT(values_.size() != 0);
    return values_.back();
  }

  // Attempts to insert an element into the container of value `v`.  If an
  // equivalent element already exists, no item is inserted. A pair is returned
  // where the first element is an iterator to the element in the container with
  // key equivalent to the value referenced `v` before `insert` was called, and
  // a boolean indicating whether an insertion actually took place.
  std::pair<iterator, bool> insert(value_type const& v) {
    if (auto iter = set_.find(v); iter != set_.end()) {
      return std::pair<iterator, bool>(begin() + iter->value, false);
    } else {
      values_.push_back(v);
      iter = set_.insert({.value = values_.size() - 1}).first;
      return std::pair<iterator, bool>(begin() + iter->value, true);
    }
  }

  // Attempts to insert an element into the container of value `v`.  If an
  // equivalent element already exists, no item is inserted and the object
  // referenced by `v` is unmodified. If no equivalent element already exists,
  // the object referenced by `v` is left in its moved-from state. In either
  // case, a pair is returned where the first element is an iterator to the
  // element in the container with key equivalent to the value referenced `v`
  // before `insert` was called, and a boolean indicating whether an insertion
  // actually took place.
  std::pair<iterator, bool> insert(value_type&& v) {
    if (auto iter = set_.find(v); iter != set_.end()) {
      return std::pair<iterator, bool>(begin() + iter->value, false);
    } else {
      values_.push_back(std::move(v));
      iter = set_.insert({.value = values_.size() - 1}).first;
      return std::pair<iterator, bool>(begin() + iter->value, true);
    }
  }

  // Returns a pointer to an element in the container equivalent to the object
  // referenced by `t` if one exists, or a null pointer otherwise.
  template <std::convertible_to<value_type> T>
  const_iterator find(T const& t) const requires(Hasher<hasher, T>) {
    auto iter = set_.find(t);
    return iter != set_.end() ? begin() + iter->value : cend();
  }

  // Returns the index of the element referenced by the iterator.
  size_t index(const_iterator it) const { return std::distance(begin(), it); }

  // Returns the index of an element equivalent if it is in the container. If
  // not present, returns `end_index()`
  size_t index(value_type const& v) const {
    auto iter = set_.find(v);
    return iter == set_.end() ? end_index() : iter->value;
  }

  // Returns a reference to the element indexed by `n` if one exists. Behavior
  // is undefined if no such element exists.
  value_type const& from_index(size_t n) const {
    ASSERT(n < values_.size());
    return values_[n];
  }

  // Returns a value for which `index(v) == end_index()` is false for every `v`
  // in the container. The value returned by `end_index()` is not dependent of
  // the values in the container.
  size_t end_index() const { return std::numeric_limits<size_t>::max(); }

 private:
  struct H : private hasher {
    explicit H(std::deque<value_type> const* values) : values_(values) {}

    using is_transparent = void;

    using hasher::operator();

    size_t operator()(internal_flyweight_set::Index p) const
        requires(Hasher<hasher, value_type const>) {
      return operator()((*values_)[p.value]);
    }

    // Though `const` is a lie here, this is not exposed outside
    // `flyweight_set`, and the pointer itself does not affect the hash function
    // (only the values held in the pointed-to container. Thus, it is safe to
    // call `set_deque_pointer` if `p` points to a `std::deque` that compares
    // equal to `values_`. This allows us to efficiently implement
    // move construction/assignment operators without incurring a rehash.
    void set_deque_pointer(std::deque<value_type> const* p) const {
      values_ = p;
    }

   private:
    mutable std::deque<value_type> const* values_;
  };

  struct E : private key_equal {
    explicit E(std::deque<value_type> const* values) : values_(values) {}

    using is_transparent = void;

    bool operator()(auto const& lhs, auto const& rhs) const requires(
        std::equivalence_relation<key_equal, std::decay_t<decltype(lhs)>,
                                  std::decay_t<decltype(rhs)>>) {
      return key_equal::operator()(lhs, rhs);
    }

    bool operator()(internal_flyweight_set::Index lhs, auto const& rhs) const
        requires(std::equivalence_relation<key_equal, value_type const&,
                                           std::decay_t<decltype(rhs)>>) {
      return key_equal::operator()((*values_)[lhs.value], rhs);
    }

    bool operator()(auto const& lhs, internal_flyweight_set::Index rhs) const
        requires(std::equivalence_relation<
                 key_equal, std::decay_t<decltype(lhs)>, value_type const&>) {
      return key_equal::operator()(lhs, (*values_)[rhs.value]);
    }

    bool operator()(internal_flyweight_set::Index lhs,
                    internal_flyweight_set::Index rhs) const {
      return key_equal::operator()((*values_)[lhs.value],
                                   (*values_)[rhs.value]);
    }

    // Though `const` is a lie here, this is not exposed outside
    // `flyweight_set`, and the pointer itself does not affect the hash function
    // (only the values held in the pointed-to container. Thus, it is safe to
    // call `set_deque_pointer` if `p` points to a `std::deque` that compares
    // equal to `values_`. This allows us to efficiently implement
    // move construction/assignment operators without incurring a rehash.
    void set_deque_pointer(std::deque<value_type> const* p) const {
      values_ = p;
    }

   private:
    mutable std::deque<value_type> const* values_;
  };

  std::deque<value_type> values_;
  absl::flat_hash_set<internal_flyweight_set::Index, H, E> set_;
};

}  // namespace base

#endif  // ICARUS_BASE_FLYWEIGHT_SET_H

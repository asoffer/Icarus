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

  flyweight_set()                = default;
  flyweight_set(flyweight_set&&) = default;
  flyweight_set& operator=(flyweight_set&&) = default;

  template <std::input_iterator Iter>
  flyweight_set(Iter b, Iter e) {
    for (auto i = b; i < e; ++i) { insert(*i); }
  }

  flyweight_set(flyweight_set const& f) : flyweight_set(f.begin(), f.end()) {}
  flyweight_set& operator=(flyweight_set const& f) {
    values_.clear();
    set_.clear();
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
      return std::pair<iterator, bool>(*iter, false);
    } else {
      values_.push_back(v);
      iter = set_.insert(std::prev(values_.end())).first;
      return std::pair<iterator, bool>(*iter, true);
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
      return std::pair<iterator, bool>(*iter, false);
    } else {
      values_.push_back(std::move(v));
      iter = set_.insert(std::prev(values_.end())).first;
      return std::pair<iterator, bool>(*iter, true);
    }
  }

  // Returns a pointer to an element in the container equivalent to the object
  // referenced by `t` if one exists, or a null pointer otherwise.
  template <std::convertible_to<value_type> T>
  const_iterator find(T const& t) const requires(Hasher<hasher, T>) {
    auto iter = set_.find(t);
    return iter != set_.end() ? *iter : cend();
  }

  // Returns the index of the element referenced by the iterator.
  size_t index(const_iterator it) const { return std::distance(begin(), it); }

  // Returns the index of an element equivalent if it is in the container. If
  // not present, returns `end_index()`
  size_t index(value_type const& v) const {
    auto iter = set_.find(v);
    return iter == set_.end() ? end_index() : index(*iter);
  }

  // Returns a value for which `index(v) == end_index()` is false for every `v`
  // in the container. The value returned by `end_index()` is not dependent of
  // the values in the container.
  size_t end_index() const { return std::numeric_limits<size_t>::max(); }

 private:
  struct H : private hasher {
    using is_transparent = void;

    using hasher::operator();

    size_t operator()(const_iterator it) const
        requires(Hasher<hasher, std::decay_t<decltype(*it)>>) {
      return operator()(*it);
    }
  };

  struct E : private key_equal {
    using is_transparent = void;

    bool operator()(value_type const& lhs, value_type const& rhs) const {
      return key_equal::operator()(lhs, rhs);
    }

    bool operator()(const_iterator lhs, value_type const& rhs) const {
      return key_equal::operator()(*lhs, rhs);
    }

    bool operator()(value_type const& lhs, const_iterator rhs) const {
      return key_equal::operator()(lhs, *rhs);
    }

    bool operator()(const_iterator lhs, const_iterator rhs) const {
      return key_equal::operator()(*lhs, *rhs);
    }
  };

  std::deque<value_type> values_;
  absl::flat_hash_set<const_iterator, H, E> set_;
};

}  // namespace base

#endif  // ICARUS_BASE_FLYWEIGHT_SET_H

#ifndef ICARUS_BASE_RAW_FLYWEIGHT_H
#define ICARUS_BASE_RAW_FLYWEIGHT_H

#include <concepts>
#include <deque>
#include <initializer_list>
#include <utility>

#include "absl/container/flat_hash_set.h"
#include "base/debug.h"
#include "base/meta.h"

namespace base {

// `flyweight_map<K, M>` is an ordered associative container, where the keys are
// guaranteed to be distinct. Pointers to elements in the container are never
// invalidated other than by assignment or calls to `clear`. Iteration occurs in
// the order the elements are inserted.
template <typename V, Hasher<V> Hash, std::equivalence_relation<V, V> Eq>
struct raw_flyweight {
  using value_type  = V;
  using size_type   = size_t;

  using iterator       = typename std::deque<value_type>::iterator;
  using const_iterator = typename std::deque<value_type>::const_iterator;
  using reverse_iterator =
      typename std::deque<value_type>::const_reverse_iterator;
  using const_reverse_iterator = reverse_iterator;

  using reference       = typename std::deque<value_type>::reference;
  using const_reference = typename std::deque<value_type>::const_reference;
  using pointer         = value_type*;
  using const_pointer   = value_type const*;

  raw_flyweight()                     = default;
  raw_flyweight(raw_flyweight&&)      = default;
  raw_flyweight& operator=(raw_flyweight&&) = default;

  raw_flyweight(std::initializer_list<value_type> l)
      : raw_flyweight(l.begin(), l.end()) {}

  template <std::input_iterator Iter>
  raw_flyweight(Iter b, Iter e) {
    for (auto iter = b; iter != e; ++iter) {
      set_.try
set_.try_emplace(*iter);
    }

    for (auto& value : values_) { set_.insert(std::addressof(value)); }


    if (auto iter = this->set_.find(std::addressof(t)); iter != this->set_.end()) {
      return std::pair<pointer, bool>(*iter, false);
    } else {
      reference ref = this->values_.emplace_back(
          std::piecewise_construct, std::forward_as_tuple(t),
          std::forward_as_tuple(args...));
      auto* p       = std::addressof(ref);
      iter          = this->set_.insert(p).first;
      return std::pair<pointer, bool>(p, true);
    }
  }

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

  // Returns the index of `it` in the container.
  size_t index(const_iterator it) const { return std::distance(begin(), it); }

  // Returns an iterator pointing to the element at index `n` in the container.
  const_iterator at_index(size_t n) const { return std::next(begin(), n); }

 // private:
  struct H : private Hash {
    using is_transparent = void;

    size_t operator()(auto* p) const
        requires(Hasher<Hash, std::decay_t<decltype(*p)>>) {
      return Hash::operator()(*p);
    }

    size_t operator()(pointer p) const { return Hash::operator()(p->first); }
  };

  struct E : private Eq {
    using is_transparent = void;

    bool operator()(pointer lhs, pointer rhs) const {
      return Eq::operator()(lhs->first, rhs->first);
    }

    bool operator()(pointer lhs, auto* rhs) const requires(
        std::equivalence_relation<Eq, std::decay_t<decltype(lhs->first)>,
                                  std::decay_t<decltype(*rhs)>>) {
      return Eq::operator()(lhs->first, *rhs);
    }

    bool operator()(auto* lhs, pointer rhs) const requires(
        std::equivalence_relation<Eq, std::decay_t<decltype(*lhs)>,
                                  std::decay_t<decltype(rhs->first)>>) {
      return Eq::operator()(*lhs, rhs->first);
    }

    bool operator()(auto* lhs, auto* rhs) const
        requires(std::equivalence_relation<Eq, std::decay_t<decltype(*lhs)>,
                                           std::decay_t<decltype(*rhs)>>) {
      return Eq::operator()(*lhs, *rhs);
    }
  };

  std::deque<value_type> values_;
  absl::flat_hash_set<pointer, H, E> set_;
};

}  // namespace base

#endif  // ICARUS_BASE_RAW_FLYWEIGHT_H

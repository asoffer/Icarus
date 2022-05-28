#ifndef ICARUS_BASE_FLYWEIGHT_MAP_H
#define ICARUS_BASE_FLYWEIGHT_MAP_H

#include <concepts>
#include <deque>
#include <initializer_list>
#include <utility>

#include "absl/container/flat_hash_set.h"
#include "base/debug.h"
#include "base/meta.h"

// TODO: This is relying on incorrect assumptions about iterator invalidation.
// It should be rewritten the way we rewrote flyweight_set.
namespace base {

// `flyweight_map<K, M>` is an ordered associative container, where the keys are
// guaranteed to be distinct. Pointers to elements in the container are never
// invalidated other than by assignment or calls to `clear`. Iteration occurs in
// the order the elements are inserted.
template <typename K, typename M, Hasher<K> Hash = absl::Hash<K>,
          std::equivalence_relation<K, K> Eq = std::equal_to<K>>
struct flyweight_map {
  using key_type    = K;
  using mapped_type = M;
  using value_type  = std::pair<key_type const, mapped_type>;
  using size_type   = size_t;
  using hasher      = Hash;
  using key_equal   = Eq;

  using iterator       = typename std::deque<value_type>::iterator;
  using const_iterator = typename std::deque<value_type>::const_iterator;
  using reverse_iterator =
      typename std::deque<value_type>::const_reverse_iterator;
  using const_reverse_iterator = reverse_iterator;

  using reference       = typename std::deque<value_type>::reference;
  using const_reference = typename std::deque<value_type>::const_reference;
  using pointer         = value_type*;
  using const_pointer   = value_type const*;

  flyweight_map()                = default;
  flyweight_map(flyweight_map&&) = default;
  flyweight_map& operator=(flyweight_map&&) = default;

  template <std::input_iterator Iter>
  flyweight_map(Iter b, Iter e) {
    for (auto i = b; i < e; ++i) { try_emplace(i->first, i->second); }
  }

  flyweight_map(flyweight_map const& f) : flyweight_map(f.begin(), f.end()) {}
  flyweight_map& operator=(flyweight_map const& f) {
    values_.clear();
    set_.clear();
    for (auto i = f.begin(); i < f.end(); ++i) {
      try_emplace(i->first, i->second);
    }
    return *this;
  }

  template <std::convertible_to<key_type> T>
  mapped_type& operator[](T&& t) requires(
      std::default_initializable<mapped_type>) {
    return try_emplace(t).first->second;
  }

  flyweight_map(std::initializer_list<value_type> l)
      : flyweight_map(l.begin(), l.end()) {}

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

  // Attempts to insert an element into the container with a key constructible
  // from `T` and a value constructible from `Args...`. If an equivalent element
  // already exists, no item is inserted and the object referenced by `t` is
  // unmodified. If no equivalent element already exists, the object referenced
  // by `t` is left in its moved-from state. In either case, a pair is returned
  // where the first element is a pointer to the element in the container with
  // key equivalent to the value referenced `t` before `try_emplace` was called,
  // and a boolean indicating whether an insertion actually took place.
  template <std::convertible_to<key_type> T, typename... Args>
  std::pair<iterator, bool> try_emplace(T&& t, Args&&... args) requires(
      std::constructible_from<mapped_type, Args...>) {
    if (auto set_iter = set_.find(std::addressof(t)); set_iter != set_.end()) {
      return std::pair<iterator, bool>(*set_iter, false);
    } else {
      reference ref = values_.emplace_back(std::piecewise_construct,
                                           std::forward_as_tuple(t),
                                           std::forward_as_tuple(args...));
      auto iter     = std::prev(values_.end());
      set_.insert(iter);
      return std::pair<iterator, bool>(iter, true);
    }
  }

  // Returns a pointer to an element in the container equivalent to the object
  // referenced by `t` if one exists, or a null pointer otherwise.
  template <std::convertible_to<K> T>
  iterator find(T const& t) requires(Hasher<hasher, T>) {
    auto set_iter = set_.find(std::addressof(t));
    return set_iter != set_.end() ? *set_iter : values_.end();
  }

  // Returns a pointer to an element in the container equivalent to the object
  // referenced by `t` if one exists, or a null pointer otherwise.
  template <std::convertible_to<K> T>
  const_iterator find(T const& t) const requires(Hasher<hasher, T>) {
    auto set_iter = set_.find(std::addressof(t));
    return set_iter != set_.end() ? *set_iter : values_.end();
  }

 private:
  struct H : private hasher {
    using is_transparent = void;

    size_t operator()(auto* p) const
        requires(Hasher<hasher, std::decay_t<decltype(*p)>>) {
      return hasher::operator()(*p);
    }

    size_t operator()(iterator iter) const {
      return hasher::operator()(iter->first);
    }
  };

  struct E : private key_equal {
    using is_transparent = void;

    bool operator()(iterator lhs, iterator rhs) const {
      return key_equal::operator()(lhs->first, rhs->first);
    }

    bool operator()(iterator lhs, auto* rhs) const requires(
        std::equivalence_relation<key_equal, std::decay_t<decltype(lhs->first)>,
                                  std::decay_t<decltype(*rhs)>>) {
      return key_equal::operator()(lhs->first, *rhs);
    }

    bool operator()(auto* lhs, iterator rhs) const requires(
        std::equivalence_relation<key_equal, std::decay_t<decltype(*lhs)>,
                                  std::decay_t<decltype(rhs->first)>>) {
      return key_equal::operator()(*lhs, rhs->first);
    }

    bool operator()(auto* lhs, auto* rhs) const requires(
        std::equivalence_relation<key_equal, std::decay_t<decltype(*lhs)>,
                                  std::decay_t<decltype(*rhs)>>) {
      return key_equal::operator()(*lhs, *rhs);
    }
  };

  std::deque<value_type> values_;
  absl::flat_hash_set<iterator, H, E> set_;
};

}  // namespace base

#endif  // ICARUS_BASE_FLYWEIGHT_MAP_H

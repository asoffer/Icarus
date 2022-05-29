#ifndef ICARUS_BASE_FLYWEIGHT_MAP_H
#define ICARUS_BASE_FLYWEIGHT_MAP_H

#include <concepts>
#include <deque>
#include <initializer_list>
#include <utility>

#include "absl/container/flat_hash_set.h"
#include "base/debug.h"
#include "base/meta.h"

namespace base {
namespace internal_flyweight_map {
// We use a wrapper struct so that transparent hashing can distinguish the
// difference between a `size_t` and an index which happens to be implemented as
// such referring to a value of type `size_t`.
struct Index {
  size_t value;
};
}  // namespace internal_flyweight_set

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


  flyweight_map() : set_(0, H(&values_), E(&values_)) {}
  flyweight_map(flyweight_map&& m)
      : values_(std::move(m.values_)), set_(std::move(m.set_)) {
    set_.hash_function().set_deque_pointer(&values_);
    set_.key_eq().set_deque_pointer(&values_);
  }

  flyweight_map& operator=(flyweight_map&& m) {
    values_ = std::move(m.values_);
    set_    = std::move(m.set_);
    set_.hash_function().set_deque_pointer(&values_);
    set_.key_eq().set_deque_pointer(&values_);
    return *this;
  }

  template <std::input_iterator Iter>
  flyweight_map(Iter b, Iter e) : set_(0, H(&values_), E(&values_)) {
    for (auto i = b; i < e; ++i) { try_emplace(i->first, i->second); }
  }

  flyweight_map(flyweight_map const& f) : flyweight_map(f.begin(), f.end()) {}
  flyweight_map& operator=(flyweight_map const& f) {
    values_.clear();
    set_.clear();
    set_.hash_function().set_deque_pointer(&values_);
    set_.key_eq().set_deque_pointer(&values_);
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
    if (auto set_iter = set_.find(t); set_iter != set_.end()) {
      return std::pair<iterator, bool>(begin() + set_iter->value, false);
    } else {
      reference ref = values_.emplace_back(std::piecewise_construct,
                                           std::forward_as_tuple(t),
                                           std::forward_as_tuple(args...));
      auto iter     = std::prev(values_.end());
      set_.insert({.value = values_.size() -1});
      return std::pair<iterator, bool>(iter, true);
    }
  }

  // Returns a pointer to an element in the container equivalent to the object
  // referenced by `t` if one exists, or a null pointer otherwise.
  template <std::convertible_to<K> T>
  iterator find(T const& t) requires(Hasher<hasher, T>) {
    auto set_iter = set_.find(t);
    return set_iter != set_.end() ? begin() + set_iter->value : values_.end();
  }

  // Returns a pointer to an element in the container equivalent to the object
  // referenced by `t` if one exists, or a null pointer otherwise.
  template <std::convertible_to<K> T>
  const_iterator find(T const& t) const requires(Hasher<hasher, T>) {
    auto set_iter = set_.find(t);
    return set_iter != set_.end() ? begin() + set_iter->value : values_.end();
  }

  // Returns the index of the element referenced by the iterator.
  size_t index(const_iterator it) const { return std::distance(begin(), it); }

  // Returns a reference to the element indexed by `n` if one exists. Behavior
  // is undefined if no such element exists.
  value_type const& from_index(size_t n) const {
    ASSERT(n < values_.size());
    return values_[n];
  }

  // Returns the index of an element equivalent if it is in the container. If
  // not present, returns `end_index()`
  size_t index(key_type const& k) const {
    auto iter = set_.find(k);
    return iter == set_.end() ? end_index() : iter->value;
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

    size_t operator()(internal_flyweight_map::Index index) const {
      return hasher::operator()((*values_)[index.value].first);
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

    bool operator()(internal_flyweight_map::Index lhs,
                    internal_flyweight_map::Index rhs) const {
      return key_equal::operator()((*values_)[lhs.value].first,
                                   (*values_)[rhs.value].first);
    }

    bool operator()(internal_flyweight_map::Index lhs, auto const& rhs) const
        requires(std::equivalence_relation<key_equal, key_type const&,
                                           std::decay_t<decltype(rhs)>>) {
      return key_equal::operator()((*values_)[lhs.value].first, rhs);
    }

    bool operator()(auto const& lhs, internal_flyweight_map::Index rhs) const
        requires(std::equivalence_relation<
                 key_equal, std::decay_t<decltype(lhs)>, key_type const&>) {
      return key_equal::operator()(lhs, (*values_)[rhs.value].first);
    }

    bool operator()(auto const& lhs, auto const& rhs) const requires(
        std::equivalence_relation<key_equal, std::decay_t<decltype(lhs)>,
                                  std::decay_t<decltype(rhs)>>) {
      return key_equal::operator()(lhs, rhs);
    }

    // Though `const` is a lie here, this is not exposed outside
    // `flyweight_set`, and the pointer itself does not affect the hash
    // function (only the values held in the pointed-to container. Thus, it is
    // safe to call `set_deque_pointer` if `p` points to a `std::deque` that
    // compares equal to `values_`. This allows us to efficiently implement
    // move construction/assignment operators without incurring a rehash.
    void set_deque_pointer(std::deque<value_type> const* p) const {
      values_ = p;
    }

   private:
    mutable std::deque<value_type> const* values_;
  };

  std::deque<value_type> values_;
  absl::flat_hash_set<internal_flyweight_map::Index, H, E> set_;
};

}  // namespace base

#endif  // ICARUS_BASE_FLYWEIGHT_MAP_H

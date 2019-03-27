#ifndef ICARUS_BASE_UNTYPED_MAP_H
#define ICARUS_BASE_UNTYPED_MAP_H

#include <cstddef>
#include <utility>

#include "absl/container/flat_hash_map.h"
#include "base/untyped_buffer.h"

namespace base {

template <typename K>
struct untyped_map {
 public:
  template <typename U>
  U get(K const& key) {
    return values_.get<U>(keys_[key]);
  }

  size_t size() const { return keys_.size(); }
  bool empty() const { return keys_.empty(); }

  template <typename V>
  void emplace(K const& key, V&& val) {
    keys_.emplace(key, values_.append(std::forward<V>(val)));
  }

  struct const_iterator {
    const_iterator operator++() { return const_iterator{++iter_, map_}; }
    const_iterator operator++(int) { return const_iterator{iter_++, map_}; }
    const_iterator operator--() { return const_iterator{--iter_, map_}; }
    const_iterator operator--(int) { return const_iterator{iter_--, map_}; }
    std::pair<K const, void const*> operator*() {
      return std::pair(iter_->first, map_->values_.raw(iter_->second));
    }

   private:
    friend struct untyped_map;
    friend bool operator==(const_iterator lhs, const_iterator rhs) {
      return lhs.iter_ == rhs.iter_;
    }
    friend bool operator!=(const_iterator lhs, const_iterator rhs) {
      return !(lhs == rhs);
    }

    explicit const_iterator(
        typename absl::flat_hash_map<K, size_t>::const_iterator iter,
        untyped_map<K> const* map)
        : iter_(iter), map_(map) {}
    typename absl::flat_hash_map<K, size_t>::const_iterator iter_;
    untyped_map<K> const* map_;
  };

  struct iterator {
    iterator operator++() { return iterator{++iter_, map_}; }
    iterator operator++(int) { return iterator{iter_++, map_}; }
    iterator operator--() { return iterator{--iter_, map_}; }
    iterator operator--(int) { return iterator{iter_--, map_}; }
    std::pair<K const, void*> operator*() {
      return std::pair(iter_->first, map_->values_.raw(iter_->second));
    }

   private:
    friend struct untyped_map;
    friend bool operator==(iterator lhs, iterator rhs) {
      return lhs.iter_ == rhs.iter_;
    }
    friend bool operator!=(iterator lhs, iterator rhs) { return !(lhs == rhs); }

    explicit iterator(typename absl::flat_hash_map<K, size_t>::iterator iter,
                      untyped_map<K>* map)
        : iter_(iter), map_(map) {}
    typename absl::flat_hash_map<K, size_t>::iterator iter_;
    untyped_map<K>* map_;
  };

  iterator begin() { return iterator(keys_.begin(), this); }
  iterator end() { return iterator(keys_.end(), this); }

  const_iterator begin() const { return const_iterator(keys_.begin(), this); }
  const_iterator end() const { return const_iterator(keys_.end(), this); }

 private:
  absl::flat_hash_map<K, size_t> keys_;
  base::untyped_buffer values_;
};

}  // namespace base

#endif  // ICARUS_BASE_UNTYPED_MAP_H

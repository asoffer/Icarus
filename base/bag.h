#ifndef ICARUS_BASE_CONTAINER_BAG_H
#define ICARUS_BASE_CONTAINER_BAG_H

#include <vector>

namespace base {
template <typename T, typename Allocator = std::allocator<T>>
struct bag {
 public:
  struct const_iterator {
    const_iterator operator++() { return const_iterator{++value_}; }
    const_iterator operator++(int) { return const_iterator{value_++}; }
    const_iterator operator--() { return const_iterator(--value_); }
    const_iterator operator--(int) { return const_iterator(value_--); }
    T const& operator*() { return *value_; }
    T const* operator->() { return &*value_; }

   private:
    friend struct bag;
    friend bool operator==(const_iterator lhs, const_iterator rhs) {
      return lhs.value_ == rhs.value_;
    }
    friend bool operator!=(const_iterator lhs, const_iterator rhs) {
      return not(lhs == rhs);
    }

    explicit const_iterator(typename std::vector<T>::const_iterator iter)
        : value_(iter) {}
    typename std::vector<T>::const_iterator value_;
  };

  struct iterator {
    iterator operator++() { return iterator(++value_); }
    iterator operator++(int) { return iterator(value_++); }
    iterator operator--() { return iterator(--value_); }
    iterator operator--(int) { return iterator(value_--); }
    T& operator*() { return *value_; }
    T* operator->() { return &*value_; }

   private:
    friend struct bag;
    friend bool operator==(iterator lhs, iterator rhs) {
      return lhs.value_ == rhs.value_;
    }
    friend bool operator!=(iterator lhs, iterator rhs) {
      return not(lhs == rhs);
    }

    explicit iterator(typename std::vector<T>::iterator iter) : value_(iter) {}
    typename std::vector<T>::iterator value_;
  };

  iterator begin() { return iterator(data_.begin()); }
  iterator end() { return iterator(data_.end()); }

  const_iterator begin() const { return const_iterator(data_.begin()); }
  const_iterator end() const { return const_iterator(data_.end()); }

  size_t size() const { return data_.size(); }
  bool empty() const { return data_.empty(); }

  void insert(T const& t) noexcept { data_.emplace_back(t); }
  void insert(T&& t) noexcept { data_.emplace_back(std::move(t)); }

  template <typename... Args>
  void emplace(Args&&... args) {
    data_.emplace_back(std::forward<Args>(args)...);
  }

  iterator erase(iterator it) noexcept {
    *it = std::move(data_.back());
    data_.pop_back();
    return it;
  }

  T* data() { return data_.data(); }
  T const* data() const { return data_.data(); }

  void clear() { data_.clear(); }
  void reserve(size_t n) { data_.reserve(n); }

  template <typename Fn>
  void for_each(Fn&& fn) {
    for (T& t : data_) { fn(&t); }
  }

  template <typename Fn>
  void for_each(Fn&& fn) const {
    for (T const& t : data_) { fn(&t); }
  }

  template <typename Fn>
  void keep(Fn&& fn) {
    static_assert(std::is_same_v<bool, decltype(fn(std::declval<T>()))>);
    auto head_iter = data_.begin();
    auto tail_iter = data_.end();
    --tail_iter;
    while (head_iter != tail_iter) {
      if (fn(*head_iter)) {
        ++head_iter;
      } else {
        *head_iter = std::move(*tail_iter);
        --tail_iter;
      }
    }
    ++tail_iter;
    data_.erase(tail_iter, data_.end());
  }

 private:
  std::vector<T, Allocator> data_;
};

}  // namespace base

#endif  // ICARUS_BASE_CONTAINER_BAG_H

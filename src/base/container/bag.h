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
    T const& operator*() { return *value_; }
    T const* operator->() { return &*value_; }

   private:
    friend struct bag;
    friend bool operator==(const_iterator lhs, const_iterator rhs) {
      return lhs.value_ == rhs.value_;
    }
    friend bool operator!=(const_iterator lhs, const_iterator rhs) {
      return !(lhs == rhs);
    }

    const_iterator(typename std::vector<T>::const_iterator iter)
        : value_(iter) {}
    typename std::vector<T>::const_iterator value_;
  };

  struct iterator {
    iterator operator++() { return iterator(++value_); }
    iterator operator++(int) { return iterator(value_++); }
    T& operator*() { return *value_; }
    T* operator->() { return &*value_; }

   private:
    friend struct bag;
    friend bool operator==(iterator lhs, iterator rhs) {
      return lhs.value_ == rhs.value_;
    }
    friend bool operator!=(iterator lhs, iterator rhs) { return !(lhs == rhs); }

    iterator(typename std::vector<T>::iterator iter) : value_(iter) {}
    typename std::vector<T>::iterator value_;

  };

  iterator begin() { return iterator(data_.begin()); }
  iterator end() { return iterator(data_.end()); }

  const_iterator begin() const { return const_iterator(data_.begin()); }
  const_iterator end() const { return const_iterator(data_.end()); }

  size_t size() const { return data_.size(); }
  bool empty() const { return data_.empty(); }

  iterator insert(T const& t) noexcept {
    data_.push_back(t);
    return iterator(std::prev(data_.end()));
  }
  iterator insert(T&& t) noexcept {
    data_.push_back(std::move(t));
    return iterator(std::prev(data_.end()));
  }
  iterator erase(iterator it) noexcept {
    *it = std::move(data_.back());
    data_.pop_back();
    return it;
  }

  void clear() { data_.clear(); }

  template <typename Fn>
  void for_each(Fn&& fn) {
    for (T& t : data_) { fn(&t); }
  }

  template <typename Fn>
  void for_each(Fn&& fn) const {
    for (T const& t : data_) { fn(&t); }
  }

 private:
  std::vector<T, Allocator> data_;
};

}  // namespace base

#endif  // ICARUS_BASE_CONTAINER_BAG_H

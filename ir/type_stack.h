#ifndef ICARUS_IR_TYPE_STACK_H
#define ICARUS_IR_TYPE_STACK_H

#include <cstdint>
#include <initializer_list>
#include <span>
#include <vector>

#include "type/qualified_type.h"
#include "type/type.h"

namespace ic {

struct TypeStack {
  void push(std::initializer_list<type::QualifiedType> types);
  void push(std::span<type::QualifiedType const> types);

  std::span<type::QualifiedType const> top() const;

  [[nodiscard]] size_t type_count() const { return types_.size(); }
  [[nodiscard]] size_t group_count() const { return group_count_; }
  [[nodiscard]] bool empty() const { return types_.empty(); }

  struct const_iterator {
    const_iterator& operator++();
    const_iterator operator++(int) {
      auto copy = *this;
      ++*this;
      return copy;
    }
    std::span<type::QualifiedType const> operator*() const;

    friend bool operator==(const_iterator, const_iterator) = default;
    friend bool operator!=(const_iterator, const_iterator) = default;

   private:
    friend TypeStack;

    explicit const_iterator(
        std::vector<type::QualifiedType>::const_reverse_iterator iter)
        : iter_(iter) {}

    std::vector<type::QualifiedType>::const_reverse_iterator iter_;
  };

  friend void NthPrint(auto& p, auto& fmt, TypeStack const & stack) {
    p.write("[");
    std::string_view separator = "";
    for (auto iter = stack.rbegin(); iter != stack.rend(); ++iter) {
      p.write(std::exchange(separator, ", "));
      fmt(p, *iter);
    }
    p.write("]");
  }

  const_iterator rbegin() const { return const_iterator(types_.rbegin()); }
  const_iterator rend() const { return const_iterator(types_.rend()); }

  void pop();

 private:
  std::vector<type::QualifiedType> types_;
  size_t group_count_;
};

}  // namespace ic

#endif  // ICARUS_IR_TYPE_STACK_H

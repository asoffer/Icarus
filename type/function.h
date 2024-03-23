#ifndef ICARUS_TYPE_FUNCTION_H
#define ICARUS_TYPE_FUNCTION_H

#include <cstddef>
#include <span>

#include "common/constant/component.h"
#include "common/identifier.h"
#include "type/parameters.h"
#include "type/type.h"

namespace ic::type {

struct ReturnsType {
  size_t size() const { return size_; }
  bool empty() const { return size() == 0; }
  Type operator[](size_t index) const;

  struct iterator;
  iterator begin() const;
  iterator end() const;

 private:
  friend struct FunctionType;

  ReturnsType(size_t index, size_t size) : index_(index), size_(size) {}
  size_t index_;
  size_t size_;
};

struct ReturnsType::iterator {
  iterator operator++(int) {
    auto copy = *this;
    ++*this;
    return copy;
  }
  iterator& operator++() {
    ++ptr_;
    return *this;
  }
  Type operator*() const { return Type(Type::from_index, ptr_->value()); }
  friend bool operator==(iterator, iterator) = default;
  friend bool operator!=(iterator, iterator) = default;

 private:
  friend ReturnsType;
  iterator(ConstantComponent const* ptr) : ptr_(ptr) {}

  ConstantComponent const* ptr_;
};

enum Evaluation {
  RequireCompileTime,
  PreferCompileTime,
  PreferRuntime,
  RequireRuntime,
};

struct FunctionType : Type {
  explicit FunctionType() = default;

  Evaluation evaluation() const;
  ParametersType parameters() const;
  ReturnsType returns() const;

  friend void NthPrint(auto& p, auto& fmt, FunctionType f);

 private:
  friend Type;
  friend FunctionType Function(ParametersType, std::span<Type const>,
                               Evaluation);

  explicit constexpr FunctionType(uint32_t n) : Type(Type::Kind::Function, n) {}
};

FunctionType Function(ParametersType pt, std::initializer_list<Type> rets,
                      Evaluation e = Evaluation::PreferRuntime);

FunctionType Function(ParametersType pt, std::span<Type const> rets,
                      Evaluation e = Evaluation::PreferRuntime);

}  // namespace ic::type

#endif  // ICARUS_TYPE_FUNCTION_H

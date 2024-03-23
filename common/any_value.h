#ifndef ICARUS_COMMON_ANY_VALUE_H
#define ICARUS_COMMON_ANY_VALUE_H

#include <memory>
#include <utility>

#include "jasmin/core/value.h"
#include "type/type.h"

namespace ic {

struct AnyValue {
  explicit AnyValue(type::Type type, jasmin::Value value)
      : type_(type),
        width_(1),
        value_(static_cast<jasmin::Value*>(
            ::operator new(sizeof(jasmin::Value)))) {
    *value_ = value;
  }
  explicit AnyValue(type::Type type, std::span<jasmin::Value const> value)
      : type_(type),
        width_(value.size()),
        value_(static_cast<jasmin::Value*>(
            ::operator new(sizeof(jasmin::Value) * width_))) {
    auto* p = value_;
    for (auto v : value) { new (p++) jasmin::Value(v); }
  }

  AnyValue(AnyValue const& a) : AnyValue(a.type(), a.value()) {}

  AnyValue(AnyValue&& a)
      : type_(a.type_),
        width_(a.width_),
        value_(std::exchange(a.value_, nullptr)) {}

  static AnyValue JustType(type::Type t) {
    AnyValue a;
    a.type_ = t;
    return a;
  }

  AnyValue& operator=(AnyValue const& a) {
    return *this = AnyValue(a.type(), a.value());
  }

  AnyValue& operator=(AnyValue&& a) {
    type_  = a.type_;
    width_ = a.width_;
    value_ = std::exchange(a.value_, nullptr);
    return *this;
  }

  ~AnyValue() {
    static_assert(std::is_trivially_destructible_v<jasmin::Value>);
    ::operator delete(value_);
  }

  template <typename H>
  friend H AbslHashValue(H h, AnyValue const& tv) {
    h = H::combine(std::move(h), tv.type());
    if (tv.has_value()) {
      auto* p = tv.value_;
      for (size_t i = 0; i < tv.width_; ++i, ++p) {
        h = H::combine(std::move(h), p->raw_value());
      }
    }
    return h;
  }
  friend bool operator==(AnyValue const& lhs, AnyValue const& rhs) {
    if (lhs.type_ != rhs.type_ or lhs.width_ != rhs.width_) { return false; }
    auto* l = lhs.value_;
    auto* r = rhs.value_;
    // TODO: We should probably query the type for the corresponding equality.
    for (size_t i = 0; i < lhs.width_; ++i, ++l, ++r) {
      if (l->raw_value() != r->raw_value()) { return false; }
    }
    return true;
  }

  bool has_value() const { return value_; }
  type::Type type() const { return type_; }

  std::span<jasmin::Value const> value() const {
    return std::span<jasmin::Value const>(value_, value_ + width_);
  }

 private:
  AnyValue() = default;

  type::Type type_;
  size_t width_         = 0;
  jasmin::Value* value_ = nullptr;
};

}  // namespace ic

#endif  // ICARUS_COMMON_ANY_VALUE_H

#ifndef ICARUS_IR_TYPE_ERASED_VALUE_H
#define ICARUS_IR_TYPE_ERASED_VALUE_H

#include <vector>

#include "type/basic.h"

namespace ic {

struct TypeErasedValue {
  explicit TypeErasedValue(type::Type type, std::vector<jasmin::Value> value)
      : type_(type), value_(std::move(value)) {}

  template <typename H>
  friend H AbslHashValue(H h, TypeErasedValue const& tv) {
    h = H::combine(std::move(h), tv.type());
    for (auto const& v : tv.value_) {
      h = H::combine(std::move(h), v.raw_value());
    }
    return h;
  }
  friend bool operator==(TypeErasedValue const& lhs,
                         TypeErasedValue const& rhs) {
    if (lhs.type_ != rhs.type_ or lhs.value_.size() != rhs.value_.size()) {
      return false;
    }
    auto l = lhs.value_.begin();
    auto r = rhs.value_.begin();
    // TODO: We should probably query the type for the corresponding equality.
    for (; l != lhs.value_.end(); ++l, ++r) {
      if (l->raw_value() != r->raw_value()) { return false; }
    }
    return true;
  }

  type::Type type() const { return type_; }

  std::span<jasmin::Value const> value() const { return value_; }

 private:
  type::Type type_;
  std::vector<jasmin::Value> value_;
};

}  // namespace ic

#endif  // ICARUS_IR_TYPE_ERASED_VALUE_H

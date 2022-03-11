#ifndef ICARUS_TEST_EXPECTED_VALUE_H
#define ICARUS_TEST_EXPECTED_VALUE_H

#include <functional>
#include <optional>
#include <ostream>

#include "ir/value/result_buffer.h"
#include "type/type.h"

namespace test {
struct AsType {
  explicit AsType(std::optional<ir::CompleteResultBuffer> buffer, type::Type t)
      : buffer_(std::move(buffer)), t_(t) {}

  friend std::ostream &operator<<(std::ostream &os, AsType const &at) {
    if (not at.buffer_) { return os << "[No value]"; }
    return os << at.t_.Representation((*at.buffer_)[0]);
  }

  ir::CompleteResultBuffer const *buffer() const {
    return buffer_ ? &*buffer_ : nullptr;
  }

 private:
  std::optional<ir::CompleteResultBuffer> buffer_;
  type::Type t_;
};

struct ExpectedValue {
  template <typename T>
  ExpectedValue(T &&value)
      : representation_(base::UniversalPrintToString(value)),
        compare_([expected = std::forward<T>(value)](
                     ir::CompleteResultBuffer const &actual) {
          return expected == actual.get<std::decay_t<T>>(0);
        }) {}

  friend bool operator==(ExpectedValue const &lhs,
                         ir::CompleteResultBuffer const &rhs) {
    return lhs.compare_(rhs);
  }
  friend bool operator==(ir::CompleteResultBuffer const &lhs,
                         ExpectedValue const &rhs) {
    return rhs.compare_(lhs);
  }
  friend bool operator==(ExpectedValue const &lhs, AsType const &rhs) {
    auto const *b = rhs.buffer();
    return b and lhs.compare_(*b);
  }
  friend bool operator==(AsType const &lhs, ExpectedValue const &rhs) {
    auto const *b = lhs.buffer();
    return b and rhs.compare_(*b);
  }

  friend std::ostream &operator<<(std::ostream &os, ExpectedValue const &ev) {
    return os << ev.representation_;
  }

 private:
  std::string representation_;
  std::function<bool(ir::CompleteResultBuffer const &actual)> compare_;
};

}  // namespace test

#endif  // ICARUS_TEST_EXPECTED_VALUE_H

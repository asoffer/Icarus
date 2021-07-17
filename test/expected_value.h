#ifndef ICARUS_TEST_EXPECTED_VALUE_H
#define ICARUS_TEST_EXPECTED_VALUE_H

#include <functional>

#include "ir/value/result_buffer.h"

namespace test {
struct ExpectedValue {
  template <typename T>
  ExpectedValue(T &&value)
      : compare_([expected = std::forward<T>(value)](
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

 private:
  std::function<bool(ir::CompleteResultBuffer const &actual)> compare_;
};

}  // namespace test

#endif  // ICARUS_TEST_EXPECTED_VALUE_H

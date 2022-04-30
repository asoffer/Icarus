#ifndef ICARUS_IR_VALUE_MATCHER_H
#define ICARUS_IR_VALUE_MATCHER_H

#include <iostream>
#include <optional>
#include <string>
#include <vector>

#include "gtest/gtest.h"
#include "ir/value/result_buffer.h"

namespace ir {
namespace internal_matcher {

struct StreamMatchResultListener : testing::MatchResultListener {
  explicit StreamMatchResultListener(::std::ostream* os)
      : MatchResultListener(os) {}

  StreamMatchResultListener(const StreamMatchResultListener&) = delete;
  StreamMatchResultListener& operator=(const StreamMatchResultListener&) =
      delete;
};

struct ResultBufferHoldingMatcher {
  using is_gtest_matcher = void;

  template <typename... Ms>
  explicit ResultBufferHoldingMatcher(
      std::vector<testing::Matcher<CompleteResultRef>> ms)
      : ms_(std::move(ms)) {}

  bool MatchAndExplain(CompleteResultBuffer const& value,
                       std::ostream* os) const {
    if (value.num_entries() != ms_.size()) {
      if (os) {
        *os << "Expected a CompleteResultBuffer with " << ms_.size()
            << " entries but encountered one with " << value.num_entries()
            << " entries.";
      }
      return false;
    }

    bool ok = true;
    std::vector<std::optional<std::string>> outputs;
    outputs.reserve(ms_.size());
    for (size_t i = 0; i < ms_.size(); ++i) {
      std::stringstream ss;
      internal_matcher::StreamMatchResultListener l(&ss);
      if (not ms_[i].MatchAndExplain(value[i], os ? &l : nullptr)) {
        ok = false;
        if (os) {
          outputs.emplace_back(std::move(ss).str());
        } else {
          outputs.emplace_back(std::nullopt);
        }
      }
    }

    if (os and not ok) {
      for (size_t i = 0; i < outputs.size(); ++i) {
        if (not outputs[i]) { continue; }
        *os << "\n * Entry " << i << " did not match because " << *outputs[i];
      }
    }
    return ok;
  }

  void DescribeTo(std::ostream* os) const {
    if (not os) { return; }
    *os << "where all of the following hold:\n";
    for (size_t i = 0; i < ms_.size(); ++i) {
      *os << "  * entry #" << i << " ";
      ms_[i].DescribeTo(os);
      *os << "\n";
    }
  }

  void DescribeNegationTo(std::ostream* os) const {
    if (not os) { return; }
    *os << "where one of the following holds:\n";
    for (size_t i = 0; i < ms_.size(); ++i) {
      *os << "  * entry #" << i << " ";
      ms_[i].DescribeNegationTo(os);
      *os << "\n";
    }
  }

 private:
  std::vector<testing::Matcher<CompleteResultRef>> ms_;
};

}  // namespace internal_matcher

template <typename... Ms>
testing::Matcher<CompleteResultBuffer> ResultBufferHolding(Ms&&... ms) {
  std::vector<testing::Matcher<CompleteResultRef>> matchers;
  matchers.reserve(sizeof...(Ms));
  (matchers.emplace_back(std::forward<Ms>(ms)), ...);
  return internal_matcher::ResultBufferHoldingMatcher(std::move(matchers));
}

template <typename T>
struct ValueOfType {
  using is_gtest_matcher = void;

  explicit ValueOfType(testing::Matcher<T> m) : m_(std::move(m)) {}

  bool MatchAndExplain(CompleteResultRef const& value,
                       testing::MatchResultListener* l) const {
    // TODO: Improve this explanation and figure out why MatchAndExplain doesn't
    // have one.
    bool result = m_.MatchAndExplain(value.get<T>(), l);
    if (not result and l)
      *l << "when interpreted as a value of type " << typeid(T).name() << ", "
         << "the value did not match.";
    return result;
  }

  void DescribeTo(std::ostream* os) const {
    if (not os) { return; }
    (*os) << "is a value of type " << typeid(T).name() << " that ";
    m_.DescribeTo(os);
  }

  void DescribeNegationTo(std::ostream* os) const {
    std::cerr << "Negating `test::ValueOfType` is unsupported.";
    std::abort();
  }

 private:
  testing::Matcher<T> m_;
};

}  // namespace ir

#endif  // ICARUS_IR_VALUE_MATCHER_H

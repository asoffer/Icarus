#ifndef ICARUS_TEST_FAILING_DIAGNOSTIC_CONSUMER_H
#define ICARUS_TEST_FAILING_DIAGNOSTIC_CONSUMER_H

#include <iostream>
#include <string>

#include "base/meta.h"
#include "diagnostic/consumer/consumer.h"
#include "diagnostic/message.h"
#include "gtest/gtest.h"

namespace test {

// `FailingConsumer` is a diagnostic consumer which emits GoogleTest errors for
// each diagnostic that is consumed.
struct FailingConsumer : diagnostic::DiagnosticConsumer {
  explicit FailingConsumer() : diagnostic::DiagnosticConsumer(nullptr) {}
  ~FailingConsumer() override {}

  void ConsumeImpl(diagnostic::DiagnosticMessage&& d) override {
    // TODO move this out to an ostream renderer.
    std::stringstream ss;
    d.for_each_component([&](auto const& component) {
      using T = std::decay_t<decltype(component)>;
      if constexpr (std::is_same_v<T, diagnostic::Text>) {
        ss << component.c_str();
      } else if constexpr (std::is_same_v<T, diagnostic::List>) {
        for (std::string const& item : component.items()) {
          ss << "  * " << item.c_str() << "\n";
        }
      } else if constexpr (std::is_same_v<T, diagnostic::SourceQuote>) {
        // TODO
      } else {
        static_assert(base::always_false<T>());
      }
      ss << "\n\n";
    });
    GTEST_FAIL() << "Compilation error\n:" << ss.str();
  }
};

}  // namespace test

#endif  // ICARUS_TEST_FAILING_DIAGNOSTIC_CONSUMER_H

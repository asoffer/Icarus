#ifndef ICARUS_TEST_FAILING_DIAGNOSTIC_CONSUMER_H
#define ICARUS_TEST_FAILING_DIAGNOSTIC_CONSUMER_H

#include <iostream>
#include <string>

#include "diagnostic/consumer/consumer.h"
#include "diagnostic/message.h"
#include "gtest/gtest.h"
#include "nth/meta/type.h"

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
      static constexpr auto type = nth::type<decltype(component)>.decayed();
      if constexpr (type == nth::type<diagnostic::Text>) {
        ss << component.c_str();
      } else if constexpr (type == nth::type<diagnostic::List>) {
        for (std::string const& item : component.items()) {
          ss << "  * " << item.c_str() << "\n";
        }
      } else if constexpr (type == nth::type<diagnostic::SourceQuote>) {
        // TODO
      } else {
        static_assert(nth::type<T>.dependent(false));
      }
      ss << "\n\n";
    });
    GTEST_FAIL() << "Compilation error\n:" << ss.str();
  }
};

}  // namespace test

#endif  // ICARUS_TEST_FAILING_DIAGNOSTIC_CONSUMER_H

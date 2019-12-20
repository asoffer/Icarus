#ifndef ICARUS_DIAGNOSTIC_CONSUMER_CONSUMER_H
#define ICARUS_DIAGNOSTIC_CONSUMER_CONSUMER_H

#include "diagnostic/message.h"

namespace diagnostic {

struct DiagnosticConsumer {
  virtual ~DiagnosticConsumer() {}

  template <typename Diag>
    void Consume(Diag const& diag) {
    ConsumeImpl(diag.ToMessage());
    ++num_consumed_;
  }

  constexpr size_t num_consumed() const { return num_consumed_; }

 protected:
  virtual void ConsumeImpl(DiagnosticMessage&& diag) = 0;

 private:
  size_t num_consumed_ = 0;
};

}  // namespace diagnostic

#endif  // ICARUS_DIAGNOSTIC_CONSUMER_CONSUMER_H

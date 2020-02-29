#ifndef ICARUS_DIAGNOSTIC_CONSUMER_CONSUMER_H
#define ICARUS_DIAGNOSTIC_CONSUMER_CONSUMER_H

#include "diagnostic/message.h"

namespace diagnostic {

struct DiagnosticConsumer {
  explicit DiagnosticConsumer(frontend::Source const* src) : src_(src) {}
  virtual ~DiagnosticConsumer() {}

  template <typename Diag>
  void Consume(Diag const& diag) {
    ConsumeImpl(diag.ToMessage(src_));
    ++num_consumed_;
  }

  frontend::Source const* source() const { return src_; }

  // TODO this should be overridable. What it means to count the number consumed
  // is dependent on what it consumes. For example, if warnings are considered
  // errors, we might change the count.
  constexpr size_t num_consumed() const { return num_consumed_; }

 protected:
  virtual void ConsumeImpl(DiagnosticMessage&& diag) = 0;

 private:
  frontend::Source const* src_;
  size_t num_consumed_ = 0;
};

}  // namespace diagnostic

#endif  // ICARUS_DIAGNOSTIC_CONSUMER_CONSUMER_H

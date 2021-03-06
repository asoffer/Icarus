#ifndef ICARUS_DIAGNOSTIC_CONSUMER_CONSUMER_H
#define ICARUS_DIAGNOSTIC_CONSUMER_CONSUMER_H

#include "diagnostic/message.h"

namespace diagnostic {

struct ConsumedMessage {
  std::string_view category;
  std::string_view name;
  DiagnosticMessage message;
};

struct DiagnosticConsumer : base::Cast<DiagnosticConsumer> {
  explicit DiagnosticConsumer(frontend::Source const* src) : src_(src) {}
  virtual ~DiagnosticConsumer() {}

  void Consume(ConsumedMessage m) {
    ConsumeImpl(m.category, m.name, std::move(m.message));
    ++num_consumed_;
  }

  template <typename Diag>
  void Consume(Diag const& diag) {
    ConsumeImpl(Diag::kCategory, Diag::kName, diag.ToMessage(src_));
    ++num_consumed_;
  }

  frontend::Source const* source() const { return src_; }

  // TODO this should be overridable. What it means to count the number consumed
  // is dependent on what it consumes. For example, if warnings are considered
  // errors, we might change the count.
  constexpr size_t num_consumed() const { return num_consumed_; }

  virtual void ConsumeImpl(std::string_view category, std::string_view name,
                           DiagnosticMessage&& diag) = 0;

 private:
  frontend::Source const* src_;
  size_t num_consumed_ = 0;
};

}  // namespace diagnostic

#endif  // ICARUS_DIAGNOSTIC_CONSUMER_CONSUMER_H

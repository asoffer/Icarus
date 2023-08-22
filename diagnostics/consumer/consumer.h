#ifndef ICARUS_DIAGNOSTICS_CONSUMER_CONSUMER_H
#define ICARUS_DIAGNOSTICS_CONSUMER_CONSUMER_H

#include <string_view>

#include "diagnostics/message.h"

namespace ic::diag {

struct DiagnosticConsumer {
  explicit DiagnosticConsumer(std::string_view source) : source_(source) {}

  void set_source(std::string_view source) { source_ = source; }

  void Consume(Message const &message) {
    for (auto const &component : message.components()) {
      Start(component);
      Process(component);
      Complete(component);
    }
  }

  virtual ~DiagnosticConsumer()                            = default;
  virtual void Start(MessageComponent const &component)    = 0;
  virtual void Process(MessageComponent const &component)  = 0;
  virtual void Complete(MessageComponent const &component) = 0;

 private:
  std::string_view source_;
};

}  // namespace ic::diag

#endif  // ICARUS_DIAGNOSTICS_CONSUMER_CONSUMER_H

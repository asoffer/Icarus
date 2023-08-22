#ifndef ICARUS_DIAGNOSTICS_CONSUMER_TTY_H
#define ICARUS_DIAGNOSTICS_CONSUMER_TTY_H

#include <cstdint>
#include <optional>
#include <string_view>

#include "diagnostics/consumer/consumer.h"
#include "diagnostics/message.h"

namespace ic::diag {

struct TtyConsumer : DiagnosticConsumer {
  ~TtyConsumer() = default;

  static std::optional<TtyConsumer> Make(std::string_view source);

  void Start(MessageComponent const &component) override;
  void Process(MessageComponent const &component) override;
  void Complete(MessageComponent const &component) override;

 private:
  explicit TtyConsumer(std::string_view source, uint64_t columns);

  int Columns();

  uint64_t window_change_epoch_ = 0;
  uint64_t columns_             = 0;
  uint64_t indentation_         = -2;
};

}  // namespace ic::diag

#endif  // ICARUS_DIAGNOSTICS_CONSUMER_TTY_H

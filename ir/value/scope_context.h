#ifndef ICARUS_IR_VALUE_SCOPE_CONTEXT_H
#define ICARUS_IR_VALUE_SCOPE_CONTEXT_H

#include <string>
#include <vector>

#include "absl/types/span.h"

namespace ir {

struct ScopeContext {
  explicit ScopeContext(std::vector<std::string> block_names)
      : block_names_(std::move(block_names)) {}

  absl::Span<std::string const> blocks() const { return block_names_; }

 private:
  std::vector<std::string> block_names_;
};

}  // namespace ir

#endif  // ICARUS_IR_VALUE_SCOPE_CONTEXT_H

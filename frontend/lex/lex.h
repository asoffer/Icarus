#ifndef ICARUS_FRONTEND_LEX_LEX_H
#define ICARUS_FRONTEND_LEX_LEX_H

#include <optional>
#include <string_view>
#include <vector>

#include "base/debug.h"
#include "core/lexeme.h"
#include "diagnostic/consumer/consumer.h"

namespace frontend {

struct LexResult {
  std::vector<core::Lexeme> lexemes_;
};

// Analyzes `source` constructing a `LexResult` object or `nullopt` if `source`
// does not represent a lexically valid program. Parts of the returned
// `LexResult` may reference ranges of characters in `source`. Thus, underlying
// data referenced by `source` must remain valid for the lifetime of the return
// value of this function.
std::optional<LexResult> Lex(
    std::string_view source,
    diagnostic::DiagnosticConsumer &diagnostic_consumer);

}  // namespace frontend

#endif  // ICARUS_FRONTEND_LEX_LEX_H

#ifndef ICARUS_FRONTEND_PARSE_H
#define ICARUS_FRONTEND_PARSE_H

#include <memory>

#include "absl/types/span.h"
#include "ast/module.h"
#include "diagnostic/consumer/consumer.h"
#include "frontend/lexeme.h"

namespace frontend {

std::optional<ast::Module> ParseModule(
    absl::Span<Lexeme const> lexemes, diagnostic::DiagnosticConsumer& consumer);

}  // namespace frontend

#endif  // ICARUS_FRONTEND_PARSE_H

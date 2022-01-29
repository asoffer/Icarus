#ifndef ICARUS_FRONTEND_PARSE_H
#define ICARUS_FRONTEND_PARSE_H

#include <memory>

#include "absl/types/span.h"
#include "ast/module.h"
#include "frontend/lexeme.h"

namespace frontend {

std::unique_ptr<ast::Module> ParseModule(absl::Span<Lexeme const> lexemes);

}  // namespace frontend

#endif  // ICARUS_FRONTEND_PARSE_H

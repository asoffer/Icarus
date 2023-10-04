#ifndef ICARUS_LEXER_LEXER_H
#define ICARUS_LEXER_LEXER_H

#include <string_view>

#include "diagnostics/consumer/consumer.h"
#include "lexer/token_buffer.h"

namespace ic::lex {

TokenBuffer Lex(std::string_view source,
                diag::DiagnosticConsumer& diagnostic_consumer);

}  // namespace ic::lex

#endif  // ICARUS_LEXER_LEXER_H

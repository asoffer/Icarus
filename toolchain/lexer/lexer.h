#ifndef ICARUS_TOOLCHAIN_LEXER_LEXER_H
#define ICARUS_TOOLCHAIN_LEXER_LEXER_H

#include <string_view>

#include "toolchain/lexer/token_buffer.h"

namespace ic {

TokenBuffer Lex(std::string_view source,
                DiagnosticConsumer& diagnostic_consumer);

}  // namespace ic

#endif  // ICARUS_TOOLCHAIN_LEXER_LEXER_H

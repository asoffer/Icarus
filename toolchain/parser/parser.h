#ifndef ICARUS_TOOLCHAIN_PARSER_PARSER_H
#define ICARUS_TOOLCHAIN_PARSER_PARSER_H

#include "toolchain/lexer/token_buffer.h"
#include "toolchain/parser/parse_tree.h"

namespace ic {

ParseTree Parse(TokenBuffer const& token_buffer,
                DiagnosticConsumer& diagnostic_consumer);

}  // namespace ic

#endif  // ICARUS_TOOLCHAIN_PARSER_PARSER_H

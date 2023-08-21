#ifndef ICARUS_PARSER_PARSER_H
#define ICARUS_PARSER_PARSER_H

#include "lexer/token_buffer.h"
#include "parser/parse_tree.h"

namespace ic {

ParseTree Parse(TokenBuffer const& token_buffer,
                DiagnosticConsumer& diagnostic_consumer);

}  // namespace ic

#endif  // ICARUS_PARSER_PARSER_H

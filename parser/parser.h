#ifndef ICARUS_PARSER_PARSER_H
#define ICARUS_PARSER_PARSER_H

#include "diagnostics/consumer/consumer.h"
#include "lexer/token_buffer.h"
#include "parser/parse_tree.h"

namespace ic {

ParseTree Parse(TokenBuffer const& token_buffer,
                diag::DiagnosticConsumer& diagnostic_consumer);

}  // namespace ic

#endif  // ICARUS_PARSER_PARSER_H

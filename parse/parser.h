#ifndef ICARUS_PARSER_PARSER_H
#define ICARUS_PARSER_PARSER_H

#include "diagnostics/consumer/consumer.h"
#include "ir/lexical_scope.h"
#include "lexer/token_buffer.h"
#include "parse/tree.h"

namespace ic {

struct ParseResult {
  ParseTree parse_tree;
  LexicalScopeTree scope_tree;
};

ParseResult Parse(TokenBuffer const& token_buffer,
                  diag::DiagnosticConsumer& diagnostic_consumer);

}  // namespace ic

#endif  // ICARUS_PARSER_PARSER_H

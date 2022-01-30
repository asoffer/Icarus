#ifndef ICARUS_FRONTEND_LEX_LEX_H
#define ICARUS_FRONTEND_LEX_LEX_H

#include "base/global.h"
#include "diagnostic/consumer/consumer.h"
#include "frontend/lex/lexeme.h"
#include "frontend/source/buffer.h"
#include "frontend/source/source.h"
#include "ir/value/hashtag.h"

namespace frontend {

struct LexState {
  LexState(std::string_view content, diagnostic::DiagnosticConsumer &diag)
      : cursor_(content), diag_(diag) {}

  char peek() { return cursor_[0]; }

  std::string_view cursor_;
  diagnostic::DiagnosticConsumer &diag_;
};

struct StringLiteralError {
  enum class Kind {
    kInvalidEscapedChar,
    kRunaway,
  } kind;
  // Offset of the offending character in the range (excluding the leading
  // quotation mark).
  int offset;
};

std::vector<Lexeme> Lex(std::string_view content,
                        diagnostic::DiagnosticConsumer &diag);
Lexeme NextToken(LexState *state);

}  // namespace frontend

#endif  // ICARUS_FRONTEND_LEX_LEX_H

#ifndef ICARUS_FRONTEND_LEX_LEX_H
#define ICARUS_FRONTEND_LEX_LEX_H

#include "base/global.h"
#include "diagnostic/consumer/consumer.h"
#include "frontend/lex/lexeme.h"
#include "frontend/source/buffer.h"
#include "frontend/source/cursor.h"
#include "frontend/source/source.h"
#include "ir/value/hashtag.h"

namespace frontend {

struct LexState {
  LexState(SourceBuffer *buffer, diagnostic::DiagnosticConsumer &diag,
           size_t chunk = 0)
      : buffer_(*buffer),
        cursor_(SourceLoc(chunk, 0), buffer_.last_chunk()),
        diag_(diag) {}

  char peek() {
    ASSERT(cursor_.view().size() != 0u);
    return cursor_.view()[0];
  }

  SourceBuffer& buffer_;
  SourceCursor cursor_;
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

std::vector<Lexeme> Lex(SourceBuffer &buffer,
                        diagnostic::DiagnosticConsumer &diag, size_t chunk = 0);
Lexeme NextToken(LexState *state);

}  // namespace frontend

#endif  // ICARUS_FRONTEND_LEX_LEX_H

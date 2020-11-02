#ifndef ICARUS_FRONTEND_LEX_LEX_H
#define ICARUS_FRONTEND_LEX_LEX_H

#include "base/expected.h"
#include "base/global.h"
#include "diagnostic/consumer/consumer.h"
#include "frontend/lex/lexeme.h"
#include "frontend/source/cursor.h"
#include "frontend/source/range.h"
#include "frontend/source/source.h"
#include "ir/value/hashtag.h"

namespace frontend {

struct LexState {
  LexState(Source *src, diagnostic::DiagnosticConsumer &diag,
           LineNum initial_line_num = LineNum(1))
      : src_(src),
        cursor_(SourceLoc(initial_line_num, Offset(0)),
                src_->ReadUntil('\n').view),
        diag_(diag) {}

  char peek() {
    ASSERT(cursor_.view().size() != 0u);
    return cursor_.view()[0];
  }

  Source *src_;
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

std::vector<Lexeme> Lex(Source &src, diagnostic::DiagnosticConsumer &diag,
                        LineNum initial_line_num = LineNum(1));
Lexeme NextToken(LexState *state);

}  // namespace frontend

#endif  // ICARUS_FRONTEND_LEX_LEX_H

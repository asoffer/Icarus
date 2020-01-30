#ifndef ICARUS_FRONTEND_LEX_LEX_H
#define ICARUS_FRONTEND_LEX_LEX_H

#include "ast/hashtag.h"
#include "base/expected.h"
#include "diagnostic/consumer/consumer.h"
#include "frontend/lex/lexeme.h"
#include "frontend/source/cursor.h"
#include "frontend/source/range.h"
#include "frontend/source/source.h"

namespace frontend {
extern absl::flat_hash_map<std::string_view, ast::Hashtag::Builtin> const
    BuiltinHashtagMap;

struct LexState {
  LexState(Source *src, diagnostic::DiagnosticConsumer &diag)
      : src_(src),
        cursor_(SourceLoc(LineNum(1), Offset(0)), src_->ReadUntil('\n').view),
        diag_(diag) {}

  char peek() {
    ASSERT(cursor_.view().size() != 0u);
    return cursor_.view()[0];
  }

  Source *src_;
  SourceCursor cursor_;
  diagnostic::DiagnosticConsumer& diag_;
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

Lexeme NextToken(LexState *state);

}  // namespace frontend

#endif  // ICARUS_FRONTEND_LEX_LEX_H

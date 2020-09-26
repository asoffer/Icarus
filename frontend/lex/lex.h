#ifndef ICARUS_FRONTEND_LEX_LEX_H
#define ICARUS_FRONTEND_LEX_LEX_H

#include "ast/hashtag.h"
#include "base/expected.h"
#include "base/global.h"
#include "diagnostic/consumer/consumer.h"
#include "frontend/lex/lexeme.h"
#include "frontend/source/cursor.h"
#include "frontend/source/range.h"
#include "frontend/source/source.h"

namespace frontend {

inline base::Global BuiltinHashtagMap =
    absl::flat_hash_map<std::string_view, ast::Hashtag::Builtin>{
        {"{export}", ast::Hashtag::Builtin::Export},
        {"{uncopyable}", ast::Hashtag::Builtin::Uncopyable},
        {"{immovable}", ast::Hashtag::Builtin::Immovable},
        {"{inline}", ast::Hashtag::Builtin::Inline},
        {"{no_default}", ast::Hashtag::Builtin::NoDefault}};

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

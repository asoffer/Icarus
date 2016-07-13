#ifndef ICARUS_ERROR_LOG_H
#define ICARUS_ERROR_LOG_H

namespace Error {
namespace Log {
extern size_t num_errs_;
extern std::map<std::string, std::map<size_t, std::vector<std::string>>> log_;

extern bool ImmediateMode;
void Dump();
inline size_t NumErrors() { return num_errs_; }
void Log(const Cursor &loc, const std::string &msg);

void NullCharInSrc(const Cursor &loc);
void NonGraphicCharInSrc(const Cursor &loc);
void TooManyDots(const Cursor &loc, size_t num_dots);
void NonWhitespaceAfterNewlineEscape(const Cursor &loc, size_t dist);
void InvalidHashtag(const Cursor &loc);
void NotInMultilineComment(const Cursor &loc);
void RunawayMultilineComment();
void EscapedDoubleQuoteInCharLit(const Cursor &loc);
void EscapedSingleQuoteInStringLit(const Cursor &loc);
void RunawayStringLit(const Cursor &loc);
void RunawayCharLit(const Cursor &loc);
void InvalidEscapeCharInStringLit(const Cursor &loc);
void InvalidEscapeCharInCharLit(const Cursor &loc);
void InvalidCharQuestionMark(const Cursor &loc);
void InvalidCharTilde(const Cursor &loc);
void TabInCharLit(const Cursor &loc);
void MissingComma(const Cursor &loc);
void UndeclaredIdentifier(const Cursor &loc, const char *token);
void AmbiguousIdentifier(const Cursor &loc, const char *token);

} // namespace Log
} // namespace Error

#endif // ICARUS_ERROR_LOG_H

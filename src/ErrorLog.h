#ifndef ICARUS_ERROR_LOG_H
#define ICARUS_ERROR_LOG_H

namespace AST {
  struct Unop;
}// namespace AST

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
void InvalidCapture(const Cursor &loc, const AST::Declaration *decl);
void UnopTypeFail(const std::string &msg, const AST::Unop *unop);
void NonIntegralArrayIndex(const Cursor &loc, const Type *index_type);
void EmptyArrayLit(const Cursor &loc);
void InvalidAddress(const Cursor &loc, Assign mode);
void InvalidAssignment(const Cursor &loc, Assign mode);
void CaseLHSBool(const Cursor &case_loc, const Cursor &loc, const Type *t);
void ResizingFixedArray(const Cursor &loc);
void MissingMember(const Cursor &loc, const std::string &member_name,
                   const Type *t);
void IndexingNonArray(const Cursor &loc, const Type *t);
void SlicingNonArray(const Cursor &loc, const Type *t);
void InvalidCast(const Cursor &loc, const Type *from, const Type *to);
} // namespace Log
} // namespace Error

#endif // ICARUS_ERROR_LOG_H

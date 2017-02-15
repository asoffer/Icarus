#ifndef ICARUS_ERROR_LOG_H
#define ICARUS_ERROR_LOG_H

namespace AST {
struct Unop;
struct Case;
} // namespace AST

namespace ErrorLog {
extern size_t num_errs_;

void Dump();
inline size_t NumErrors() { return num_errs_; }
void LogGeneric(const Cursor &loc, const std::string &msg);

void TooManyDots(const Cursor &loc, size_t num_dots);
void NonWhitespaceAfterNewlineEscape(const Cursor &loc, size_t dist);
void RunawayMultilineComment();
void UndeclaredIdentifier(const Cursor &loc, const std::string &token);
void AmbiguousIdentifier(const Cursor &loc, const std::string &token);
void InvalidCapture(const Cursor &loc, const AST::Declaration *decl);
void UnopTypeFail(const std::string &msg, const AST::Unop *unop);
void InvalidAddress(const Cursor &loc, Assign mode);
void InvalidAssignment(const Cursor &loc, Assign mode);
void CaseLHSBool(const Cursor &case_loc, const Cursor &loc, const Type *t);
void MissingMember(const Cursor &loc, const std::string &member_name,
                   const Type *t);
void NotAType(const Cursor &loc, const std::string &id_tok);
void DeclaredVoidType(const Cursor &loc, const std::string &id_tok);
void DeclaredParametricType(const Cursor &loc, const std::string &id_tok);
void UnknownParserError(const std::string &file_name,
                        const std::vector<Cursor> &lines);
void InvalidReturnType(const Cursor &loc, Type *given, Type *correct);
void DoubleDeclAssignment(const Cursor &decl_loc, const Cursor &val_loc);
void DeclOutOfOrder(AST::Declaration *decl, AST::Identifier *id);

void NullCharInSrc(const Cursor &loc);
void NonGraphicCharInSrc(const Cursor &loc);
void GlobalNonDecl(const Cursor &loc);
void EmptyFile(const Cursor &loc);

void NotBinary(const Cursor &loc, const std::string &token);
void Reserved(const Cursor &loc, const std::string &token);

void InvalidCast(const Cursor &loc, const Type *from, const Type *to);
void AssignmentTypeMismatch(const Cursor &loc, const Type *lhs,
                            const Type *rhs);
void InvalidRangeTypes(const Cursor &loc, const Type *lhs, const Type *rhs);
void InitWithNull(const Cursor &loc, const Type *lhs, const Type *rhs);

void AlreadyFoundMatch(const Cursor &loc, const std::string &op_symbol,
                       const Type *lhs, const Type *rhs);
void NoKnownOverload(const Cursor &loc, const std::string &op_symbol,
                     const Type *lhs, const Type *rhs);

void AssignmentArrayLength(const Cursor &loc, size_t len);
void NonBinaryAssignment(const Cursor &loc, size_t len);
void ChainTypeMismatch(const Cursor &loc, std::set<Type *> types);

void NotAType(AST::Expression *expr, Type *t);
void IndeterminantType(AST::Expression *expr);
void CyclicDependency(AST::Node *node);

void InvalidRangeType(const Cursor &loc, Type *t);
void InvalidStringIndex(const Cursor &loc, Type *index_type);
void NonIntegralArrayIndex(const Cursor &loc, const Type *index_type);
void IndexingNonArray(const Cursor &loc, const Type *t);
void SlicingNonArray(const Cursor &loc, const Type *t);
void CaseTypeMismatch(AST::Case *case_ptr, Type *correct = nullptr);
void InvalidPrintDefinition(const Cursor &loc, const Type *t);
void InvalidAssignDefinition(const Cursor &loc, const Type *t);
void InvalidScope(const Cursor &loc, const Type *t);
void UserDefinedError(const Cursor &loc, const std::string& msg);

#define ERROR_MACRO(fn_name, msg_head, msg_foot, underline_length)             \
  void fn_name(const Cursor &loc);
#include "config/error.conf"
#undef ERROR_MACRO
} // namespace ErrorLog

#endif // ICARUS_ERROR_LOG_H

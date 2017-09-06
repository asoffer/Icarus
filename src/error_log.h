#ifndef ICARUS_ERROR_LOG_H
#define ICARUS_ERROR_LOG_H

#include <string>
#include <set>
#include <vector>

struct Type;

#include "input/cursor.h"
#include "base/debug.h"

namespace AST {
struct Case;
struct Declaration;
struct Expression;
struct Identifier;
struct Node;
struct Unop;
} // namespace AST

#include "ast/ast.h"
#include "type/type.h"

struct Error {
  enum class Code : char {
    Other, // This is awful. It should never be used! TODO Remove it!
    CyclicDependency,
    UndeclaredIdentifier,
    AmbiguousIdentifier,
    ImplicitCapture,
    FreeNonPtr,
    PrintVoid,
    ReturnVoid,
    DerefNonPtr,
    UnsignedNegation,
    MissingOperator,
    InvalidRangeType,
    LogicalNegationOfNonBool,
    MissingMember,
    NoCallMatches,
    AmbiguousCall,
    InvalidCast,
    ArrayAssignmentDifferentLengths,
    AssignmentArrayLength,
    AssignmentTypeMismatch,
    InvalidStringIndexType,
    SlicingNonArray,
    IndexingNonArray,
    NonIntegralArrayIndex,
    InvalidRangeTypes,
    NonBooleanLogicalAssignemnt,
    AlreadyFoundMatch, // Probably can resue overload resolution instead of
                       // this?
    NoKnownOverload,   // same goes for tihs
    NonComposableFns,
    NonTypeFnInput,
    NonTypeFnOutput,
    ChainTypeMismatch,
    TypeIteration,
    IndeterminantType,
    NotAType,
    VoidDeclaration,
  };

  Error(Error::Code c) : code_(c) {}
  Error::Code code_;
  // TODO hold metadata too
};

namespace LogError {
void UndeclaredIdentifier(AST::Identifier *id);
void AmbiguousIdentifier(AST::Identifier *id);
void ImplicitCapture(AST::Identifier *id);
void PreconditionNeedsBool(AST::Expression *expr);
void EnsureNeedsBool(AST::Expression *expr);
void FailedPrecondition(const IR::Property& property);
} // namespace LogError

namespace ErrorLog {
extern size_t num_errs_;

void Dump();
inline size_t NumErrors() { return num_errs_; }
void LogGeneric(const SourceLocation &loc, const std::string &msg);

void TooManyDots(const SourceLocation &loc, size_t num_dots);
void NonWhitespaceAfterNewlineEscape(const SourceLocation &loc, size_t dist);
void RunawayMultilineComment();
void UnopTypeFail(const std::string &msg, const AST::Unop *unop);
void InvalidAddress(const SourceLocation &loc, Assign mode);
void InvalidAssignment(const SourceLocation &loc, Assign mode);
void CaseLHSBool(const SourceLocation &case_loc, const SourceLocation &loc, const Type *t);
void MissingMember(const SourceLocation &loc, const std::string &member_name,
                   const Type *t);
void NotAType(const SourceLocation &loc, const std::string &id_tok);
void DeclaredVoidType(const SourceLocation &loc, const std::string &id_tok);
void DeclaredParametricType(const SourceLocation &loc, const std::string &id_tok);
void UnknownParserError(const Source::Name &source_name,
                        const std::vector<SourceLocation> &lines);
void InvalidReturnType(const SourceLocation &loc, Type *given, Type *correct);
void DoubleDeclAssignment(const SourceLocation &decl_loc, const SourceLocation &val_loc);
void DeclOutOfOrder(AST::Declaration *decl, AST::Identifier *id);

void NullCharInSrc(const SourceLocation &loc);
void NonGraphicCharInSrc(const SourceLocation &loc);
void GlobalNonDecl(const SourceLocation &loc);

void NotBinary(const SourceLocation &loc, const std::string &token);
void Reserved(const SourceLocation &loc, const std::string &token);

void InvalidCast(const SourceLocation &loc, const Type *from, const Type *to);
void AssignmentTypeMismatch(const SourceLocation &loc, const Type *lhs,
                            const Type *rhs);
void InvalidRangeTypes(const SourceLocation &loc, const Type *lhs, const Type *rhs);
void InitWithNull(const SourceLocation &loc, const Type *lhs, const Type *rhs);

void AlreadyFoundMatch(const SourceLocation &loc, const std::string &op_symbol,
                       const Type *lhs, const Type *rhs);
void NoKnownOverload(const SourceLocation &loc, const std::string &op_symbol,
                     const Type *lhs, const Type *rhs);

void AssignmentArrayLength(const SourceLocation &loc, size_t len);
void NonBinaryAssignment(const SourceLocation &loc, size_t len);
void ChainTypeMismatch(const SourceLocation &loc, std::set<Type *> types);

void NotAType(AST::Expression *expr, Type *t);
void IndeterminantType(AST::Expression *expr);
void CyclicDependency(AST::Node *node);

void InvalidRangeType(const SourceLocation &loc, Type *t);
void InvalidStringIndex(const SourceLocation &loc, Type *index_type);
void NonIntegralArrayIndex(const SourceLocation &loc, const Type *index_type);
void IndexingNonArray(const SourceLocation &loc, const Type *t);
void SlicingNonArray(const SourceLocation &loc, const Type *t);
void CaseTypeMismatch(AST::Case *case_ptr, Type *correct = nullptr);
void InvalidPrintDefinition(const SourceLocation &loc, const Type *t);
void InvalidAssignDefinition(const SourceLocation &loc, const Type *t);
void InvalidScope(const SourceLocation &loc, const Type *t);
void UserDefinedError(const SourceLocation &loc, const std::string& msg);

#define ERROR_MACRO(fn_name, msg_head, msg_foot, underline_length)             \
  void fn_name(const SourceLocation &loc);
#include "config/error.conf"
#undef ERROR_MACRO
} // namespace ErrorLog

#endif // ICARUS_ERROR_LOG_H

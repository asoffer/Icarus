#ifndef ICARUS_ERROR_LOG_H
#define ICARUS_ERROR_LOG_H

#include <set>
#include <string>
#include <vector>
#include <unordered_map>

#include "base/debug.h"
#include "input/cursor.h"

struct Type;

namespace AST {
struct Case;
struct Declaration;
struct Expression;
struct Identifier;
struct Node;
struct Unop;
} // namespace AST

namespace error {
struct Log {
  void UndeclaredIdentifier(AST::Identifier *id);
  void AmbiguousIdentifier(AST::Identifier *id);
  void PreconditionNeedsBool(AST::Expression *expr);
  void PostconditionNeedsBool(AST::Expression *expr);
  void DeclOutOfOrder(AST::Declaration *decl, AST::Identifier *id);
  void InvalidEscapedCharacterInStringLiteral(const TextSpan &span);
  void RunawayStringLiteral(const TextSpan &span);
  void EscapedDoubleQuoteInCharacterLiteral(const TextSpan &span);
  void InvalidEscapedCharacterInCharacterLiteral(const TextSpan &span);
  void RunawayCharacterLiteral(const TextSpan &span);
  void SpaceInCharacterLiteral(const TextSpan &span);
  void TabInCharacterLiteral(const TextSpan &span);
  void TooManyDots(const TextSpan &span);
  void InvalidCharacterQuestionMark(const TextSpan &span);
  void InvalidCharacterTilde(const TextSpan &span);

  size_t size() const {
    return undeclared_ids_.size() + out_of_order_decls_.size() + errors_.size();
  }
  void Dump() const;

  // TODO per source file splitting? Can't do this until you figure out the
  // module/multi-source-file story.
  using Token = std::string;
  std::unordered_map<Token, std::vector<AST::Identifier *>> undeclared_ids_;
  std::unordered_map<AST::Declaration *, std::vector<AST::Identifier *>>
      out_of_order_decls_;

  std::vector<std::string> errors_;
};
} // namespace error

// TODO everything below here is legacy and needs to be cleaned up

#include "ast/ast.h"
#include "type/type.h"
#include "ir/property.h"

namespace ErrorLog {
extern size_t num_errs_;

void Dump();
inline size_t NumErrors() { return num_errs_; }
void LogGeneric(const TextSpan &span, const std::string &msg);
// TODO build a graph of declarations that shadow each other and show connected components together.
void ShadowingDeclaration(const AST::Declaration &decl1,
                          const AST::Declaration &decl2);
void NonWhitespaceAfterNewlineEscape(const TextSpan &span, size_t dist);
void RunawayMultilineComment();
void UnopTypeFail(const std::string &msg, const AST::Unop *unop);
void InvalidAddress(const TextSpan &span, Assign mode);
void InvalidAssignment(const TextSpan &span, Assign mode);
void CaseLHSBool(const TextSpan &case_span, const TextSpan &span,
                 const Type *t);
void MissingMember(const TextSpan &span, const std::string &member_name,
                   const Type *t);
void NotAType(const TextSpan &span, const std::string &id_tok);
void DeclaredVoidType(const TextSpan &span, const std::string &id_tok);
void UnknownParserError(const Source::Name &source_name,
                        const std::vector<TextSpan> &lines);
void InvalidReturnType(const TextSpan &span, Type *given, Type *correct);
void DoubleDeclAssignment(const TextSpan &decl_span, const TextSpan &val_loc);

void NullCharInSrc(const TextSpan &loc);
void NonGraphicCharInSrc(const TextSpan &loc);
void GlobalNonDecl(const TextSpan &loc);

void NotBinary(const TextSpan &span, const std::string &token);
void Reserved(const TextSpan &span, const std::string &token);

void InvalidCast(const TextSpan &span, const Type *from, const Type *to);
void AssignmentTypeMismatch(const TextSpan &span, const Type *lhs,
                            const Type *rhs);
void InvalidRangeTypes(const TextSpan &span, const Type *lhs, const Type *rhs);
void InitWithNull(const TextSpan &span, const Type *lhs, const Type *rhs);

void AlreadyFoundMatch(const TextSpan &span, const std::string &op_symbol,
                       const Type *lhs, const Type *rhs);
void NoKnownOverload(const TextSpan &span, const std::string &op_symbol,
                     const Type *lhs, const Type *rhs);

void AssignmentArrayLength(const TextSpan &span, size_t len);
void NonBinaryAssignment(const TextSpan &span, size_t len);
void ChainTypeMismatch(const TextSpan &span, std::set<Type *> types);

void NotAType(AST::Expression *expr, Type *t);
void IndeterminantType(AST::Expression *expr);
void CyclicDependency(AST::Node *node);

void InvalidRangeType(const TextSpan &span, Type *t);
void InvalidStringIndex(const TextSpan &span, Type *index_type);
void NonIntegralArrayIndex(const TextSpan &span, const Type *index_type);
void IndexingNonArray(const TextSpan &span, const Type *t);
void SlicingNonArray(const TextSpan &span, const Type *t);
void CaseTypeMismatch(AST::Case *case_ptr, Type *correct = nullptr);
void InvalidAssignDefinition(const TextSpan &span, const Type *t);
void InvalidScope(const TextSpan &span, const Type *t);
void UserDefinedError(const TextSpan &span, const std::string &msg);
void UninferrableType(const TextSpan &span);
void CommaListStatement(const TextSpan &span);

#define ERROR_MACRO(fn_name, msg_head, msg_foot, underline_length)             \
  void fn_name(const TextSpan &loc);
#include "config/error.conf"
#undef ERROR_MACRO
} // namespace ErrorLog

#endif // ICARUS_ERROR_LOG_H

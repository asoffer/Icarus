#ifndef ICARUS_ERROR_LOG_H
#define ICARUS_ERROR_LOG_H

#include <set>
#include <string>
#include <vector>
#include <unordered_map>

#include "../base/debug.h"
#include "../frontend/text_span.h"

namespace type {
struct Type;
} // namespace type

namespace AST {
struct Declaration;
struct Expression;
struct Identifier;
struct Node;
struct Unop;
} // namespace AST

namespace error {
struct Log {
#define MAKE_LOG_ERROR(fn_name, msg)                                           \
  void fn_name(const TextSpan& span);
#include "errors.xmacro.h"
#undef MAKE_LOG_ERROR

  void UndeclaredIdentifier(AST::Identifier *id);
  void AmbiguousIdentifier(AST::Identifier *id);
  void PreconditionNeedsBool(AST::Expression *expr);
  void PostconditionNeedsBool(AST::Expression *expr);
  void DeclOutOfOrder(AST::Declaration *decl, AST::Identifier *id);
  void AssignmentTypeMismatch(AST::Expression *lhs,
                              AST::Expression *rhs);
  void RunawayMultilineComment();
  void DoubleDeclAssignment(const TextSpan &decl_span,
                            const TextSpan &val_span);
  void Reserved(const TextSpan &span, const std::string &token);
  void NotBinary(const TextSpan &span, const std::string &token);
  void UnknownParseError(const std::vector<TextSpan> &span);
  void PositionalArgumentFollowingNamed(const std::vector<TextSpan> &pos_spans,
                                        const TextSpan &named_span);
  void NotAType(AST::Expression *expr);
  void ShadowingDeclaration(const AST::Declaration &decl1,
                            const AST::Declaration &decl2);

  // TODO include a source location/span/trace or whatever you decide to include.
  void UserDefinedError(const std::string &err);
  void DereferencingNonPointer(const type::Type *type, const TextSpan &span);
  void FreeingNonPointer(const type::Type *type, const TextSpan &span);
  void ReturnTypeMismatch(const type::Type *expected_type,
                          const AST::Expression *ret_expr);
  void IndexedReturnTypeMismatch(const type::Type *expected_type,
                                 const AST::Expression *ret_expr, size_t index);
  void ReturningWrongNumber(const AST::Expression *ret_expr, size_t num_rets);
  void NoReturnTypes(const AST::Expression *ret_expr);
  void DeclarationUsedInUnop(const std::string &unop,
                             const TextSpan &decl_span);
  void MissingMember(const TextSpan &span, const std::string &member_name,
                     const type::Type *t);
  void InvalidStringIndex(const TextSpan &span, const type::Type *index_type);
  void NonIntegralArrayIndex(const TextSpan &span,
                             const type::Type *index_type);
  void IndexingNonArray(const TextSpan &span, const type::Type *t);

  std::vector<AST::Identifier *> *CyclicDependency();

  size_t size() const {
    return undeclared_ids_.size() + out_of_order_decls_.size() +
           errors_.size() + cyc_dep_vecs_.size();
  }
  void Dump() const;

  // TODO per source file splitting? Can't do this until you figure out the
  // module/multi-source-file story.
  using Token = std::string;
  std::unordered_map<Token, std::vector<AST::Identifier *>>
      undeclared_ids_;
  std::unordered_map<AST::Declaration *, std::vector<AST::Identifier *>>
      out_of_order_decls_;

  std::vector<std::unique_ptr<std::vector<AST::Identifier *>>> cyc_dep_vecs_;

  std::vector<std::string> errors_;
};
} // namespace error

#endif // ICARUS_ERROR_LOG_H

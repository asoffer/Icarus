#ifndef ICARUS_ERROR_LOG_H
#define ICARUS_ERROR_LOG_H

#include <set>
#include <string>
#include "base/container/unordered_map.h"
#include "base/container/vector.h"

#include "../base/debug.h"
#include "../frontend/text_span.h"

namespace type {
struct Type;
}  // namespace type

namespace ast {
struct Declaration;
struct Expression;
struct Identifier;
struct Node;
struct Unop;
}  // namespace ast

namespace error {
struct Log {
#define MAKE_LOG_ERROR(fn_name, msg) void fn_name(const TextSpan &span);
#include "error/errors.xmacro.h"
#undef MAKE_LOG_ERROR

  void UndeclaredIdentifier(ast::Identifier *id);
  void AmbiguousIdentifier(ast::Identifier *id);
  void PreconditionNeedsBool(ast::Expression *expr);
  void PostconditionNeedsBool(ast::Expression *expr);
  void DeclOutOfOrder(ast::Declaration *decl, ast::Identifier *id);
  void AssignmentTypeMismatch(ast::Expression *lhs, ast::Expression *rhs);
  void RunawayMultilineComment();
  void DoubleDeclAssignment(const TextSpan &decl_span,
                            const TextSpan &val_span);
  void Reserved(const TextSpan &span, const std::string &token);
  void NotBinary(const TextSpan &span, const std::string &token);
  void UnknownParseError(const base::vector<TextSpan> &span);
  void PositionalArgumentFollowingNamed(const base::vector<TextSpan> &pos_spans,
                                        const TextSpan &named_span);
  void NotAType(ast::Expression *expr);
  void ShadowingDeclaration(const ast::Declaration &decl1,
                            const ast::Declaration &decl2);

  // TODO include a source location/span/trace or whatever you decide to
  // include.
  void UserDefinedError(const std::string &err);
  void DereferencingNonPointer(const type::Type *type, const TextSpan &span);
  void WhichNonVariant(const type::Type *type, const TextSpan &span);
  void ReturnTypeMismatch(const type::Type *expected_type,
                          const ast::Expression *ret_expr);
  void IndexedReturnTypeMismatch(const type::Type *expected_type,
                                 const ast::Expression *ret_expr, size_t index);
  void ReturningWrongNumber(const ast::Expression *ret_expr, size_t num_rets);
  void NoMatchingOperator(const std::string &op, const type::Type *lhs,
                          const type::Type *rhs, const TextSpan &span);
  void NoReturnTypes(const ast::Expression *ret_expr);
  void DeclarationUsedInUnop(const std::string &unop,
                             const TextSpan &decl_span);
  void MissingMember(const TextSpan &span, const std::string &member_name,
                     const type::Type *t);
  void InvalidCharBufIndex(const TextSpan &span, const type::Type *index_type);
  void NonIntegralArrayIndex(const TextSpan &span,
                             const type::Type *index_type);
  void IndexingNonArray(const TextSpan &span, const type::Type *t);

  base::vector<ast::Identifier *> *CyclicDependency();

  size_t size() const {
    return undeclared_ids_.size() + out_of_order_decls_.size() +
           errors_.size() + cyc_dep_vecs_.size();
  }
  void Dump() const;

  // TODO per source file splitting? Can't do this until you figure out the
  // module/multi-source-file story.
  using Token = std::string;
  base::unordered_map<Token, base::vector<ast::Identifier *>> undeclared_ids_;
  base::unordered_map<ast::Declaration *, base::vector<ast::Identifier *>>
      out_of_order_decls_;

  base::vector<std::unique_ptr<base::vector<ast::Identifier *>>> cyc_dep_vecs_;

  base::vector<std::string> errors_;
};
}  // namespace error

#endif  // ICARUS_ERROR_LOG_H

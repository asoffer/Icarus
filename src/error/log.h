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
#define MAKE_LOG_ERROR(fn_name, msg) void fn_name(TextSpan const &span);
#include "error/errors.xmacro.h"
#undef MAKE_LOG_ERROR

  void UndeclaredIdentifier(ast::Identifier *id);
  void AmbiguousIdentifier(ast::Identifier *id);
  void PreconditionNeedsBool(ast::Expression *expr);
  void PostconditionNeedsBool(ast::Expression *expr);
  void DeclOutOfOrder(ast::Declaration *decl, ast::Identifier *id);
  void AssignmentTypeMismatch(ast::Expression *lhs, ast::Expression *rhs);
  void RunawayMultilineComment();
  void DoubleDeclAssignment(TextSpan const &decl_span,
                            TextSpan const &val_span);
  void Reserved(TextSpan const &span, std::string const &token);
  void NotBinary(TextSpan const &span, std::string const &token);
  void UnknownParseError(base::vector<TextSpan> const &span);
  void PositionalArgumentFollowingNamed(base::vector<TextSpan> const &pos_spans,
                                        TextSpan const &named_span);
  void NotAType(ast::Expression *expr);
  void ShadowingDeclaration(ast::Declaration const &decl1,
                            ast::Declaration const &decl2);

  // TODO include a source location/span/trace or whatever you decide to
  // include.
  void UserDefinedError(std::string const &err);
  void DereferencingNonPointer(type::Type const *type, TextSpan const &span);
  void WhichNonVariant(type::Type const *type, TextSpan const &span);
  void ReturnTypeMismatch(type::Type const *expected_type,
                          ast::Expression const *ret_expr);
  void IndexedReturnTypeMismatch(type::Type const *expected_type,
                                 ast::Expression const *ret_expr, size_t index);
  void ReturningWrongNumber(ast::Expression const *ret_expr, size_t num_rets);
  void NoMatchingOperator(std::string const &op, type::Type const *lhs,
                          type::Type const *rhs, TextSpan const &span);
  void NoReturnTypes(ast::Expression const *ret_expr);
  void DeclarationUsedInUnop(std::string const &unop,
                             TextSpan const &decl_span);
  void MissingMember(TextSpan const &span, std::string const &member_name,
                     type::Type const *t);
  void InvalidCharBufIndex(TextSpan const &span, type::Type const *index_type);
  // TODO is this the same as `ArrayIndexType`?
  void NonIntegralArrayIndex(TextSpan const &span,
                             type::Type const *index_type);
  void InvalidIndexing(TextSpan const &span, type::Type const *t);

  void TypeMustBeInitialized(TextSpan const &span, type::Type const *t);

  void ComparingIncomparables(type::Type const *lhs, type::Type const *rhs,
                              TextSpan const &span);
  void CyclicDependency(base::vector<ast::Identifier const *> cyc_deps);

  void MismatchedAssignmentSize(TextSpan const &span, size_t lhs, size_t rhs);

  void InvalidNumber(TextSpan const& span, std::string_view err);

  void NoCallMatch(TextSpan const &span,
                   base::unordered_map<ast::Expression const *,
                                       std::string> const &failure_reasons);

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

  base::vector<base::vector<ast::Identifier const *>> cyc_dep_vecs_;

  base::vector<std::string> errors_;
};
}  // namespace error

#endif  // ICARUS_ERROR_LOG_H

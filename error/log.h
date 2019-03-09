#ifndef ICARUS_ERROR_LOG_H
#define ICARUS_ERROR_LOG_H

#include <filesystem>
#include <string>
#include <unordered_map>
#include <vector>

#include "core/fn_args.h"
#include "base/debug.h"
#include "error/inference_failure_reason.h"
#include "frontend/text_span.h"

struct Context;

namespace type {
struct Type;
struct Tuple;
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
  void PreconditionNeedsBool(TextSpan const &span, type::Type const *t);
  void PostconditionNeedsBool(TextSpan const &span, type::Type const *t);
  void DeclOutOfOrder(ast::Declaration *decl, ast::Identifier *id);
  void RunawayMultilineComment();
  void DoubleDeclAssignment(TextSpan const &decl_span,
                            TextSpan const &val_span);
  void Reserved(TextSpan const &span, std::string const &token);
  void NotBinary(TextSpan const &span, std::string const &token);
  void UnknownParseError(std::vector<TextSpan> const &span);
  void PositionalArgumentFollowingNamed(std::vector<TextSpan> const &pos_spans,
                                        TextSpan const &named_span);
  void NotAType(TextSpan const &span, type::Type const *t);
  void ShadowingDeclaration(TextSpan const &span1, TextSpan const &span2);

  // TODO include a source location/span/trace or whatever you decide to
  // include.
  void UserDefinedError(std::string const &err);
  void DereferencingNonPointer(type::Type const *type, TextSpan const &span);
  void WhichNonVariant(type::Type const *type, TextSpan const &span);
  void ReturnTypeMismatch(type::Type const *expected_type,
                          type::Type const *actual_type, TextSpan const &span);
  void IndexedReturnTypeMismatch(type::Type const *expected_type,
                                 type::Type const *actual_type,
                                 TextSpan const &span, size_t index);
  void ReturningWrongNumber(TextSpan const &span, type::Type const *t,
                            size_t num_rets);
  void NoReturnTypes(ast::Expression const *ret_expr);
  void DeclarationUsedInUnop(std::string const &unop,
                             TextSpan const &decl_span);
  void MissingMember(TextSpan const &span, std::string const &member_name,
                     type::Type const *t);
  void NonExportedMember(TextSpan const &span, std::string const &member_name,
                         type::Type const *t);
  void InvalidByteViewIndex(TextSpan const &span, type::Type const *index_type);
  // TODO is this the same as `ArrayIndexType`?
  void IndexingTupleOutOfBounds(TextSpan const &span, type::Tuple const *tup,
                                size_t index);

  void InvalidIndexing(TextSpan const &span, type::Type const *t);
  void InvalidIndexType(TextSpan const &span, type::Type const *t,
                        type::Type const *index_type);

  void TypeMustBeInitialized(TextSpan const &span, type::Type const *t);

  void ComparingIncomparables(type::Type const *lhs, type::Type const *rhs,
                              TextSpan const &span);
  void CyclicDependency(std::vector<ast::Identifier const *> cyc_deps);

  void MismatchedAssignmentSize(TextSpan const &span, size_t lhs, size_t rhs);

  void InvalidNumber(TextSpan const& span, std::string_view err);

  void NoCallMatch(TextSpan const &span,
                   std::vector<std::string> const &generic_failure_reasons,
                   std::unordered_map<ast::Expression const *,
                                       std::string> const &failure_reasons);
  void UninferrableType(InferenceFailureReason reason, TextSpan const &span);

  void NotCopyable(TextSpan const &span, type::Type const *from);
  void NotMovable(TextSpan const &span, type::Type const *from);

  void BuiltinError(TextSpan const&span, std::string_view text);

  void MissingDispatchContingency(
      TextSpan const &span,
      std::vector<core::FnArgs<type::Type const *>> const &missing_dispatch);

  void MissingModule(std::filesystem::path const &src,
                     std::filesystem::path const &requestor);

  void StatementsFollowingJump(TextSpan const &span);

  size_t size() const {
    return undeclared_ids_.size() + out_of_order_decls_.size() +
           errors_.size() + cyc_dep_vecs_.size();
  }
  void Dump() const;

  // TODO per source file splitting? Can't do this until you figure out the
  // module/multi-source-file story.
  std::unordered_map<std::string, std::vector<ast::Identifier *>>
      undeclared_ids_;
  std::unordered_map<ast::Declaration *, std::vector<ast::Identifier *>>
      out_of_order_decls_;

  std::vector<std::vector<ast::Identifier const *>> cyc_dep_vecs_;

  std::vector<std::string> errors_;
};
}  // namespace error

#endif  // ICARUS_ERROR_LOG_H

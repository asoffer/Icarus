#ifndef ICARUS_ERROR_LOG_H
#define ICARUS_ERROR_LOG_H

#include <filesystem>
#include <string>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "base/debug.h"
#include "core/fn_args.h"
#include "error/inference_failure_reason.h"
#include "frontend/text_span.h"

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

  // TODO most of these probably should just be replaced by a constexpr we can
  // pass into libfmt.
  void UndeclaredIdentifier(ast::Identifier const *id);
  void PreconditionNeedsBool(TextSpan const &span, std::string_view type);
  void PostconditionNeedsBool(TextSpan const &span, std::string_view type);
  void DeclOutOfOrder(ast::Declaration const *decl, ast::Identifier const *id);
  void RunawayMultilineComment();
  void DoubleDeclAssignment(TextSpan const &decl_span,
                            TextSpan const &val_span);
  void Reserved(TextSpan const &span, std::string const &token);
  void NotBinary(TextSpan const &span, std::string const &token);
  void UnknownParseError(std::vector<TextSpan> const &span);
  void PositionalArgumentFollowingNamed(std::vector<TextSpan> const &pos_spans,
                                        TextSpan const &named_span);
  void NotAType(TextSpan const &span, std::string_view type);
  void ShadowingDeclaration(TextSpan const &span1, TextSpan const &span2);

  // TODO include a source location/span/trace or whatever you decide to
  // include.
  void UserDefinedError(std::string const &err);
  void DereferencingNonPointer(std::string_view type, TextSpan const &span);
  void WhichNonVariant(std::string_view type, TextSpan const &span);
  void ReturnTypeMismatch(std::string_view expected_type,
                          std::string_view actual_type, TextSpan const &span);
  void IndexedReturnTypeMismatch(std::string_view expected_type,
                                 std::string_view actual_type,
                                 TextSpan const &span, size_t index);
  void ReturningWrongNumber(TextSpan const &span, size_t actual,
                            size_t expected);
  void NoReturnTypes(ast::Expression const *ret_expr);
  void DeclarationUsedInUnop(std::string const &unop,
                             TextSpan const &decl_span);
  void MissingMember(TextSpan const &span, std::string const &member_name,
                     std::string_view type);
  void NonExportedMember(TextSpan const &span, std::string const &member_name,
                         std::string_view type);
  // TODO is this the same as `ArrayIndexType`?
  void IndexingTupleOutOfBounds(TextSpan const &span, std::string_view tup,
                                size_t tup_size, size_t index);

  void InvalidIndexing(TextSpan const &span, std::string_view type);
  void InvalidIndexType(TextSpan const &span, std::string_view type,
                        std::string_view index_type);

  void TypeMustBeInitialized(TextSpan const &span, std::string_view type);

  void ComparingIncomparables(std::string_view lhs, std::string_view rhs,
                              TextSpan const &span);
  void CyclicDependency(std::vector<ast::Identifier const *> cyc_deps);

  void MismatchedAssignmentSize(TextSpan const &span, size_t lhs, size_t rhs);

  void InvalidNumber(TextSpan const& span, std::string_view err);

  void NoCallMatch(TextSpan const &span,
                   std::vector<std::string> const &generic_failure_reasons,
                   absl::flat_hash_map<ast::Expression const *,
                                       std::string> const &failure_reasons);
  void UninferrableType(InferenceFailureReason reason, TextSpan const &span);

  void NotCopyable(TextSpan const &span, std::string_view from);
  void NotMovable(TextSpan const &span, std::string_view from);

  void BuiltinError(TextSpan const&span, std::string_view text);

  void MissingDispatchContingency(
      TextSpan const &span,
      std::vector<core::FnArgs<std::string>> const &missing_dispatch);

  void MissingModule(std::filesystem::path const &src,
                     std::filesystem::path const &requestor);

  void StatementsFollowingJump(TextSpan const &span);

  void MismatchedBinopArithmeticType(std::string_view lhs, std::string_view rhs,
                                     TextSpan const &span);
  void InvalidCast(std::string_view lhs, std::string_view rhs,
                   TextSpan const &span);
  void PrintMustReturnVoid(std::string_view type, TextSpan const &span);
  void SwitchConditionNeedsBool(std::string_view type, TextSpan const &span);

  size_t size() const {
    return undeclared_ids_.size() + out_of_order_decls_.size() +
           errors_.size() + cyc_dep_vecs_.size();
  }
  void Dump() const;

  // TODO per source file splitting? Can't do this until you figure out the
  // module/multi-source-file story.
  absl::flat_hash_map<std::string, std::vector<ast::Identifier const *>>
      undeclared_ids_;
  absl::flat_hash_map<ast::Declaration const *,
                      std::vector<ast::Identifier const *>>
      out_of_order_decls_;

  std::vector<std::vector<ast::Identifier const *>> cyc_dep_vecs_;

  std::vector<std::string> errors_;
};
}  // namespace error

#endif  // ICARUS_ERROR_LOG_H

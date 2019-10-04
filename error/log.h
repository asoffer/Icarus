#ifndef ICARUS_ERROR_LOG_H
#define ICARUS_ERROR_LOG_H

#include <filesystem>
#include <string>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "ast/ast_fwd.h"
#include "base/debug.h"
#include "core/fn_args.h"
#include "diagnostic/console_renderer.h"
#include "error/inference_failure_reason.h"
#include "frontend/source/range.h"

namespace frontend {
struct Source;
}  // namespace frontend

namespace error {
struct Log {
  // TODO remove this overload
  explicit Log() : src_(nullptr) {}
  ~Log() { renderer_.Flush(); }

  explicit Log(frontend::Source *src) : src_(ASSERT_NOT_NULL(src)) {}

#define MAKE_LOG_ERROR(fn_name, msg)                                           \
  void fn_name(frontend::SourceRange const &range);
#include "error/errors.xmacro.h"
#undef MAKE_LOG_ERROR

  // TODO most of these probably should just be replaced by a constexpr we can
  // pass into libfmt.
  void UndeclaredIdentifier(ast::Identifier const *id);
  void PreconditionNeedsBool(frontend::SourceRange const &range,
                             std::string_view type);
  void PostconditionNeedsBool(frontend::SourceRange const &range,
                              std::string_view type);
  void DeclOutOfOrder(ast::Declaration const *decl, ast::Identifier const *id);
  void RunawayMultilineComment();
  void DoubleDeclAssignment(frontend::SourceRange const &decl_range,
                            frontend::SourceRange const &val_range);
  void Reserved(frontend::SourceRange const &range, std::string const &token);
  void NotBinary(frontend::SourceRange const &range, std::string const &token);
  void UnknownParseError(std::vector<frontend::SourceRange> const &range);
  void PositionalArgumentFollowingNamed(
      std::vector<frontend::SourceRange> const &pos_ranges,
      frontend::SourceRange const &named_range);
  void NotAType(frontend::SourceRange const &range, std::string_view type);
  void ShadowingDeclaration(frontend::SourceRange const &span1,
                            frontend::SourceRange const &span2);

  // TODO include a source location/range/trace or whatever you decide to
  // include.
  void UserDefinedError(std::string const &err);
  void DereferencingNonPointer(std::string_view type,
                               frontend::SourceRange const &range);
  void WhichNonVariant(std::string_view type,
                       frontend::SourceRange const &range);
  void ReturnTypeMismatch(std::string_view expected_type,
                          std::string_view actual_type,
                          frontend::SourceRange const &range);
  void IndexedReturnTypeMismatch(std::string_view expected_type,
                                 std::string_view actual_type,
                                 frontend::SourceRange const &range,
                                 size_t index);
  void ReturningWrongNumber(frontend::SourceRange const &range, size_t actual,
                            size_t expected);
  void NoReturnTypes(ast::ReturnStmt const *ret_expr);
  void DeclarationUsedInUnop(std::string const &unop,
                             frontend::SourceRange const &decl_range);
  void MissingMember(frontend::SourceRange const &range,
                     std::string_view member_name, std::string_view type);
  void NonExportedMember(frontend::SourceRange const &range,
                         std::string_view member_name, std::string_view type);
  // TODO is this the same as `ArrayIndexType`?
  void IndexingTupleOutOfBounds(frontend::SourceRange const &range,
                                std::string_view tup, size_t tup_size,
                                size_t index);

  void InvalidIndexing(frontend::SourceRange const &range,
                       std::string_view type);
  void InvalidIndexType(frontend::SourceRange const &range,
                        std::string_view type, std::string_view index_type);

  void TypeMustBeInitialized(frontend::SourceRange const &range,
                             std::string_view type);

  void ComparingIncomparables(std::string_view lhs, std::string_view rhs,
                              frontend::SourceRange const &range);
  void CyclicDependency(std::vector<ast::Identifier const *> cyc_deps);

  void MismatchedAssignmentSize(frontend::SourceRange const &range, size_t lhs,
                                size_t rhs);

  void InvalidNumber(frontend::SourceRange const &range, std::string_view err);

  void NoCallMatch(frontend::SourceRange const &range,
                   std::vector<std::string> const &generic_failure_reasons,
                   absl::flat_hash_map<ast::Expression const *,
                                       std::string> const &failure_reasons);
  void UninferrableType(InferenceFailureReason reason,
                        frontend::SourceRange const &range);

  void NotCopyable(frontend::SourceRange const &range, std::string_view from);
  void NotMovable(frontend::SourceRange const &range, std::string_view from);

  void BuiltinError(frontend::SourceRange const &range, std::string_view text);

  void MissingDispatchContingency(
      frontend::SourceRange const &range,
      std::vector<core::FnArgs<std::string>> const &missing_dispatch);

  void MissingModule(std::filesystem::path const &src,
                     std::filesystem::path const &requestor);

  void StatementsFollowingJump(frontend::SourceRange const &range);

  void MismatchedBinopArithmeticType(std::string_view lhs, std::string_view rhs,
                                     frontend::SourceRange const &range);
  void InvalidCast(std::string_view lhs, std::string_view rhs,
                   frontend::SourceRange const &range);
  void PrintMustReturnVoid(std::string_view type,
                           frontend::SourceRange const &range);
  void SwitchConditionNeedsBool(std::string_view type,
                                frontend::SourceRange const &range);

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
  frontend::Source *src_;
  diagnostic::ConsoleRenderer renderer_{stderr};
};
}  // namespace error

#endif  // ICARUS_ERROR_LOG_H

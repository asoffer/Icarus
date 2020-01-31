#ifndef ICARUS_ERROR_LOG_H
#define ICARUS_ERROR_LOG_H

#include <string>
#include <string_view>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "ast/ast_fwd.h"
#include "base/debug.h"
#include "core/fn_args.h"
#include "diagnostic/console_renderer.h"
#include "diagnostic/consumer/consumer.h"
#include "error/inference_failure_reason.h"
#include "frontend/source/range.h"

namespace frontend {
struct Source;
}  // namespace frontend

namespace error {
struct Log {
  // TODO remove this overload
  explicit Log(diagnostic::DiagnosticConsumer &diag) : src_(nullptr), diag_(diag) {}
  ~Log() { renderer_.Flush(); }

  explicit Log(frontend::Source *src, diagnostic::DiagnosticConsumer &diag)
      : src_(ASSERT_NOT_NULL(src)), diag_(diag) {}

  diagnostic::DiagnosticConsumer &diag() { return diag_; }

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
  void NotBinary(frontend::SourceRange const &range, std::string const &token);
  void NotAType(frontend::SourceRange const &range, std::string_view type);
  void ShadowingDeclaration(frontend::SourceRange const &span1,
                            frontend::SourceRange const &span2);

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

  void InvalidNumber(frontend::SourceRange const &range, std::string_view err);

  void NoCallMatch(frontend::SourceRange const &range,
                   std::vector<std::string> generic_failure_reasons,
                   absl::flat_hash_map<ast::Expression const *,
                                       std::string> const &failure_reasons);
  void UninferrableType(InferenceFailureReason reason,
                        frontend::SourceRange const &range);

  void NotCopyable(frontend::SourceRange const &range, std::string_view from);

  void BuiltinError(frontend::SourceRange const &range, std::string text);

  void MissingDispatchContingency(
      frontend::SourceRange const &range,
      std::vector<core::FnArgs<std::string>> const &missing_dispatch);

  void MissingModule(std::string_view src, std::string_view requestor);

  void StatementsFollowingJump(frontend::SourceRange const &range);

  void PrintMustReturnVoid(std::string_view type,
                           frontend::SourceRange const &range);
  void SwitchConditionNeedsBool(std::string_view type,
                                frontend::SourceRange const &range);

  size_t size() const {
    return undeclared_ids_.size() + out_of_order_decls_.size() +
           cyc_dep_vecs_
               .size();  // TODO get size out of renderer? Probably have a first
                         // step before it goes to the renderer.
  }
  void Dump();

  // TODO per source file splitting? Can't do this until you figure out the
  // module/multi-source-file story.
  absl::flat_hash_map<std::string, std::vector<ast::Identifier const *>>
      undeclared_ids_;
  absl::flat_hash_map<ast::Declaration const *,
                      std::vector<ast::Identifier const *>>
      out_of_order_decls_;

  std::vector<std::vector<ast::Identifier const *>> cyc_dep_vecs_;

  frontend::Source *src_;
  diagnostic::DiagnosticConsumer& diag_;
  diagnostic::ConsoleRenderer renderer_{stderr};
};
}  // namespace error

#endif  // ICARUS_ERROR_LOG_H

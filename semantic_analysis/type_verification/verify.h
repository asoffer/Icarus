#ifndef ICARUS_SEMANTIC_ANALYSIS_TYPE_VERIFICATION_VERIFY_H
#define ICARUS_SEMANTIC_ANALYSIS_TYPE_VERIFICATION_VERIFY_H

#include "absl/types/span.h"
#include "ast/ast.h"
#include "ast/module.h"
#include "compiler/context.h"
#include "compiler/type_for_diagnostic.h"
#include "diagnostic/consumer/consumer.h"
#include "semantic_analysis/task.h"
#include "type/qual_type.h"

namespace semantic_analysis {
// TODO: Move to this namespace
using ::compiler::TypeForDiagnostic;

enum class TypeVerificationPhase {
  VerifyParameters,
  VerifyType,
  VerifyBody,
  Completed,
};

namespace internal_verify {

using Types = std::tuple<core::Parameters<type::QualType> const *,
                         absl::Span<type::QualType const>, void, void>;
template <TypeVerificationPhase P>
using ReturnType = std::tuple_element_t<static_cast<int>(P), Types>;

}  // namespace internal_verify

using VerificationTask =
    Task<ast::Node const *, TypeVerificationPhase, internal_verify::ReturnType>;
using VerificationScheduler = Scheduler<VerificationTask>;

inline auto VerifyTypeOf(ast::Node const *node) {
  return VerificationTask::Phase<TypeVerificationPhase::VerifyType>(node);
}

inline auto VerifyParametersOf(ast::Node const *node) {
  return VerificationTask::Phase<TypeVerificationPhase::VerifyParameters>(node);
}

struct TypeVerifier : VerificationScheduler {
  using signature = VerificationTask();

  explicit TypeVerifier(compiler::Context &c, diagnostic::DiagnosticConsumer &d)
      : VerificationScheduler([](VerificationScheduler &s,
                                 ast::Node const *node) -> VerificationTask {
          return node->visit(static_cast<TypeVerifier &>(s));
        }),
        context_(c),
        diagnostic_consumer_(d) {}

  compiler::Context &context() const { return context_; }

  template <typename D>
  void ConsumeDiagnostic(D &&d) {
    diagnostic_consumer_.Consume(std::forward<D>(d));
  }

  VerificationTask operator()(auto const *node) {
    return VerifyType(*this, node);
  }

  void complete_verification(ast::Expression const *node,
                             type::QualType qual_type) {
    this->set_completed<TypeVerificationPhase::VerifyType>(
        node, context().set_qual_type(node, qual_type));
  }
  void complete_verification(ast::Expression const *node,
                             std::vector<type::QualType> qual_types) {
    this->set_completed<TypeVerificationPhase::VerifyType>(
        node, context().set_qual_types(node, std::move(qual_types)));
  }

  template <typename NodeType>
  static VerificationTask VerifyType(TypeVerifier &tv, NodeType const *) {
    NOT_YET(base::meta<NodeType>);
  }

  static VerificationTask VerifyType(TypeVerifier &,
                                     ast::BindingDeclaration const *);
  static VerificationTask VerifyType(TypeVerifier &, ast::Declaration const *);
  static VerificationTask VerifyType(TypeVerifier &,
                                     ast::Declaration::Id const *);
  static VerificationTask VerifyType(TypeVerifier &, ast::Identifier const *);
  static VerificationTask VerifyType(TypeVerifier &,
                                     ast::ShortFunctionLiteral const *);
  static VerificationTask VerifyType(TypeVerifier &,
                                     ast::UnaryOperator const *);
  static VerificationTask VerifyType(TypeVerifier &, ast::Terminal const *);

 private:
  compiler::Context &context_;
  diagnostic::DiagnosticConsumer &diagnostic_consumer_;
};

}  // namespace semantic_analysis

#endif  // ICARUS_SEMANTIC_ANALYSIS_TYPE_VERIFICATION_VERIFY_H

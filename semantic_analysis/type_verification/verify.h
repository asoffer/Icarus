#ifndef ICARUS_SEMANTIC_ANALYSIS_TYPE_VERIFICATION_VERIFY_H
#define ICARUS_SEMANTIC_ANALYSIS_TYPE_VERIFICATION_VERIFY_H

#include "absl/types/span.h"
#include "ast/ast.h"
#include "ast/module.h"
#include "compiler/context.h"
#include "semantic_analysis/task.h"
#include "type/qual_type.h"

namespace semantic_analysis {

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

  explicit TypeVerifier(compiler::Context &c)
      : VerificationScheduler(
            [this](VerificationScheduler &s,
                   ast::Node const *node) -> VerificationTask {
              return VerifyType(static_cast<TypeVerifier &>(s), node);
            }),
        context_(c) {}

  compiler::Context &context() const { return context_; }

  VerificationTask operator()(auto const *node) {
    return VerifyType(*this, node);
  }

  VerificationTask VerifyType(VerificationScheduler &scheduler,
                              ast::Node const *node) {
    return node->visit(*this);
  }

  static VerificationTask VerifyType(TypeVerifier &tv,
                                     ast::ShortFunctionLiteral const *node);
  static VerificationTask VerifyType(TypeVerifier &tv,
                                     ast::Terminal const *node);

 private:
  compiler::Context& context_;
};

}  // namespace semantic_analysis

#endif  // ICARUS_SEMANTIC_ANALYSIS_TYPE_VERIFICATION_VERIFY_H

#ifndef ICARUS_SEMANTIC_ANALYSIS_TYPE_VERIFICATION_H
#define ICARUS_SEMANTIC_ANALYSIS_TYPE_VERIFICATION_H

#include "absl/types/span.h"
#include "ast/ast.h"
#include "ast/module.h"
#include "compiler/context.h"
#include "semantic_analysis/task.h"
#include "type/qual_type.h"

namespace semantic_analysis {

enum class TypeVerificationPhase {
  VerifyType,
  VerifyParameters,
  VerifyBody,
};

using VerificationTask = Task<ast::Node const *, TypeVerificationPhase>;
using VerificationScheduler =
    Scheduler<ast::Node const *, TypeVerificationPhase>;

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
                                     ast::Terminal const *node);

 private:
  compiler::Context& context_;
};

}  // namespace semantic_analysis

#endif  // ICARUS_SEMANTIC_ANALYSIS_TYPE_VERIFICATION_H

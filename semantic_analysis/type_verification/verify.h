#ifndef ICARUS_SEMANTIC_ANALYSIS_TYPE_VERIFICATION_VERIFY_H
#define ICARUS_SEMANTIC_ANALYSIS_TYPE_VERIFICATION_VERIFY_H

#include "absl/types/span.h"
#include "ast/ast.h"
#include "ast/module.h"
#include "diagnostic/consumer/consumer.h"
#include "jasmin/execute.h"
#include "semantic_analysis/byte_code/byte_code.h"
#include "semantic_analysis/byte_code/instruction_set.h"
#include "semantic_analysis/context.h"
#include "semantic_analysis/task.h"
#include "semantic_analysis/type_system.h"

namespace semantic_analysis {

enum class TypeVerificationPhase {
  VerifyParameters,
  VerifyType,
  VerifyBody,
  Completed,
};

namespace internal_verify {

using Types =
    std::tuple<absl::Span<absl::flat_hash_map<
                   core::ParameterType, Context::CallableIdentifier> const>,
               absl::Span<QualifiedType const>, void, void>;
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

  explicit TypeVerifier(TypeSystem &type_system, Context &c,
                        diagnostic::DiagnosticConsumer &d)
      : VerificationScheduler([](VerificationScheduler &s,
                                 ast::Node const *node) -> VerificationTask {
          return node->visit(static_cast<TypeVerifier &>(s));
        }),
        type_system_(type_system),
        context_(c),
        diagnostic_consumer_(d) {}

  Context &context() const { return context_; }
  TypeSystem &type_system() const { return type_system_; }

  template <typename D>
  void ConsumeDiagnostic(D &&d) {
    diagnostic_consumer_.Consume(std::forward<D>(d));
  }

  VerificationTask operator()(auto const *node) {
    return VerifyType(*this, node);
  }

  auto TypeOf(ast::Expression const *node, QualifiedType qualified_type) {
    return VerificationTask::YieldResult<TypeVerificationPhase::VerifyType>(
        node, context().set_qualified_type(node, qualified_type));
  }
  auto TypeOf(ast::Expression const *node,
              std::vector<QualifiedType> qualified_types) {
    return VerificationTask::YieldResult<TypeVerificationPhase::VerifyType>(
        node, context().set_qualified_types(node, std::move(qualified_types)));
  }
  auto TypeOf(ast::Expression const *node,
              absl::Span<QualifiedType const> qualified_types) {
    return VerificationTask::YieldResult<TypeVerificationPhase::VerifyType>(
        node,
        context().set_qualified_types(
            node, std::vector(qualified_types.begin(), qualified_types.end())));
  }

  auto ParametersOf(
      ast::Expression const *node,
      std::vector<
          absl::flat_hash_map<core::ParameterType, Context::CallableIdentifier>>
          parameter_ids) {
    return VerificationTask::YieldResult<
        TypeVerificationPhase::VerifyParameters>(
        node, context().set_parameters(node, std::move(parameter_ids)));
  }

  auto ParametersOf(
      ast::Expression const *node,
      absl::flat_hash_map<core::ParameterType, Context::CallableIdentifier>
          parameter_ids) {
    std::vector<decltype(parameter_ids)> v;
    v.push_back(std::move(parameter_ids));
    return ParametersOf(node, std::move(v));
  }

  template <typename NodeType>
  static VerificationTask VerifyType(TypeVerifier &tv, NodeType const *) {
    NOT_YET(base::meta<NodeType>);
  }

  static VerificationTask VerifyType(TypeVerifier &, ast::ArrayLiteral const *);
  static VerificationTask VerifyType(TypeVerifier &, ast::ArrayType const *);
  static VerificationTask VerifyType(TypeVerifier &,
                                     ast::BindingDeclaration const *);
  static VerificationTask VerifyType(TypeVerifier &, ast::Call const *);
  static VerificationTask VerifyType(TypeVerifier &, ast::Declaration const *);
  static VerificationTask VerifyType(TypeVerifier &,
                                     ast::Declaration::Id const *);
  static VerificationTask VerifyType(TypeVerifier &, ast::Identifier const *);
  static VerificationTask VerifyType(TypeVerifier &, ast::IfStmt const *);
  static VerificationTask VerifyType(TypeVerifier &,
                                     ast::ShortFunctionLiteral const *);
  static VerificationTask VerifyType(TypeVerifier &,
                                     ast::ProgramArguments const *);
  static VerificationTask VerifyType(TypeVerifier &,
                                     ast::UnaryOperator const *);
  static VerificationTask VerifyType(TypeVerifier &, ast::Terminal const *);

 private:
  TypeSystem &type_system_;
  Context &context_;
  diagnostic::DiagnosticConsumer &diagnostic_consumer_;
};

template <typename T>
std::optional<T> EvaluateAs(Context &context, TypeSystem &type_system,
                            ast::Expression const *expr) {
  auto qt        = context.qualified_type(expr);
  bool has_error = (qt.qualifiers() >= Qualifiers::Error());
  ASSERT(has_error == false);

  IrFunction f = EmitByteCode(*expr, context, type_system);
  T result;
  jasmin::Execute(f, {}, result);
  return result;
}

}  // namespace semantic_analysis

#endif  // ICARUS_SEMANTIC_ANALYSIS_TYPE_VERIFICATION_VERIFY_H

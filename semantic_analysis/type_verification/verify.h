#ifndef ICARUS_SEMANTIC_ANALYSIS_TYPE_VERIFICATION_VERIFY_H
#define ICARUS_SEMANTIC_ANALYSIS_TYPE_VERIFICATION_VERIFY_H

#include <span>

#include "ast/ast.h"
#include "ast/module.h"
#include "diagnostic/consumer/consumer.h"
#include "jasmin/execute.h"
#include "module/module.h"
#include "module/resources.h"
#include "semantic_analysis/byte_code/byte_code.h"
#include "semantic_analysis/context.h"
#include "semantic_analysis/function_data.h"
#include "semantic_analysis/task.h"
#include "semantic_analysis/type_system.h"
#include "semantic_analysis/type_verification/diagnostics.h"

namespace semantic_analysis {

struct UnreachableStatement {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "unreachable-statement";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "The statement is unreachable due to a previous return or yield."),
        diagnostic::SourceQuote().Highlighted(view, diagnostic::Style{}));
  }
  std::string_view view;
};

enum class TypeVerificationPhase {
  VerifyParameters,
  VerifyType,
  VerifyBody,
  Completed,
};

namespace internal_verify {

inline constexpr auto Signatures = nth::type_sequence<
    std::span<absl::flat_hash_map<core::ParameterType,
                                  Context::CallableIdentifier> const>(),
    std::span<QualifiedType const>(), void(), void()>;

}  // namespace internal_verify

using VerificationTask =
    Task<ast::Node const *, TypeVerificationPhase, internal_verify::Signatures>;
using VerificationScheduler = Scheduler<VerificationTask>;

inline auto VerifyTypeOf(ast::Node const *node) {
  return Phase<VerificationTask, TypeVerificationPhase::VerifyType>(node);
}

inline auto VerifyParametersOf(ast::Node const *node) {
  return Phase<VerificationTask, TypeVerificationPhase::VerifyParameters>(node);
}

struct TypeVerifier : VerificationScheduler {
  using signature = VerificationTask();

  explicit TypeVerifier(module::Resources &resources, Context &c)
      : VerificationScheduler([](VerificationScheduler &s,
                                 ast::Node const *node) -> VerificationTask {
          return node->visit(static_cast<TypeVerifier &>(s));
        }),
        resources_(resources),
        module_(resources.primary_module()),
        context_(c) {}

  Context &context() const { return context_; }
  TypeSystem &type_system() const { return module_.type_system(); }
  serialization::ForeignSymbolMap &foreign_symbol_map() const {
    return module_.foreign_symbol_map();
  }
  auto &module() { return module_; }
  auto const &module() const { return module_; }
  auto &resources() { return resources_; }

  template <typename T>
  T EvaluateAs(ast::Expression const *expression) const {
    return ::semantic_analysis::EvaluateAs<T>(context(), resources_,
                                              expression);
  }

  std::span<std::byte const> EvaluateConstant(ast::Expression const *expr,
                                              QualifiedType qt) {
    return ::semantic_analysis::EvaluateConstant(context(), resources_, expr,
                                                 qt);
  }

  template <typename D>
  void ConsumeDiagnostic(D &&d) {
    resources().diagnostic_consumer().Consume(std::forward<D>(d));
  }

  VerificationTask operator()(auto const *node) { return VerifyType(node); }

  auto TypeOf(ast::Node const *node, QualifiedType qualified_type) {
    static QualifiedType const no_return(NoReturn);
    auto *e = node->if_as<ast::Expression>();
    return YieldResult<VerificationTask, TypeVerificationPhase::VerifyType>(
        node, e ? context().set_qualified_type(e, qualified_type)
                : std::span(&no_return, 1));
  }
  auto TypeOf(ast::Expression const *node,
              std::vector<QualifiedType> qualified_types) {
    return YieldResult<VerificationTask, TypeVerificationPhase::VerifyType>(
        node, context().set_qualified_types(node, std::move(qualified_types)));
  }
  auto TypeOf(ast::Expression const *node,
              std::span<QualifiedType const> qualified_types) {
    return YieldResult<VerificationTask, TypeVerificationPhase::VerifyType>(
        node,
        context().set_qualified_types(
            node, std::vector(qualified_types.begin(), qualified_types.end())));
  }

  auto ParametersOf(
      ast::Expression const *node,
      std::vector<
          absl::flat_hash_map<core::ParameterType, Context::CallableIdentifier>>
          parameter_ids) {
    return YieldResult<VerificationTask, 
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

  auto Completed(ast::Node const *node) {
    return YieldResult<VerificationTask, TypeVerificationPhase::Completed>(
        node);
  }

  template <typename NodeType>
  VerificationTask VerifyType(NodeType const *) {
    NTH_UNIMPLEMENTED("{}") <<= {nth::type<NodeType>};
  }

  VerificationTask VerifyType(ast::Access const *);
  VerificationTask VerifyType(ast::Assignment const *);
  VerificationTask VerifyType(ast::ArrayLiteral const *);
  VerificationTask VerifyType(ast::ArrayType const *);
  VerificationTask VerifyType(ast::BinaryOperator const *);
  VerificationTask VerifyType(ast::BindingDeclaration const *);
  VerificationTask VerifyType(ast::Call const *);
  VerificationTask VerifyType(ast::Cast const *);
  VerificationTask VerifyType(ast::ComparisonOperator const *);
  VerificationTask VerifyType(ast::Declaration const *);
  VerificationTask VerifyType(ast::Declaration::Id const *);
  VerificationTask VerifyType(ast::EnumLiteral const *);
  VerificationTask VerifyType(ast::FunctionLiteral const *);
  VerificationTask VerifyType(ast::FunctionType const *);
  VerificationTask VerifyType(ast::Identifier const *);
  VerificationTask VerifyType(ast::IfStmt const *);
  VerificationTask VerifyType(ast::Import const *);
  VerificationTask VerifyType(ast::Index const *);
  VerificationTask VerifyType(ast::Module const *);
  VerificationTask VerifyType(ast::ShortFunctionLiteral const *);
  VerificationTask VerifyType(ast::SliceType const *);
  VerificationTask VerifyType(ast::ReturnStmt const *);
  VerificationTask VerifyType(ast::UnaryOperator const *);
  VerificationTask VerifyType(ast::Terminal const *);
  VerificationTask VerifyType(ast::WhileStmt const *);

  // TODO: ArgumentType, BinaryAssignmentOperator, BlockNode,
  //       DesignatedInitializer, InterfaceLiteral, Label, ScopeLiteral,
  //       ScopeNode, StructLiteral, YieldStmt

  std::string TypeForDiagnostic(ast::Expression const &expression) const {
    return ::semantic_analysis::TypeForDiagnostic(expression, context(),
                                                  type_system());
  }

  std::string ExpressionForDiagnostic(ast::Expression const &expression) const {
    return ::semantic_analysis::ExpressionForDiagnostic(expression, context(),
                                                        type_system());
  }

 private:
  module::Resources& resources_;
  module::Module &module_;
  Context &context_;
};

}  // namespace semantic_analysis

#endif  // ICARUS_SEMANTIC_ANALYSIS_TYPE_VERIFICATION_VERIFY_H

#ifndef ICARUS_COMPILER_COMPILER_H
#define ICARUS_COMPILER_COMPILER_H

#include "ast/ast.h"
#include "ast/module.h"
#include "diagnostic/consumer/consumer.h"
#include "module/module.h"
#include "module/resources.h"
#include "nth/coroutine/coroutine.h"
#include "nth/utility/buffer.h"
#include "semantic_analysis/context.h"
#include "semantic_analysis/function_data.h"
#include "semantic_analysis/type_system.h"
#include "semantic_analysis/type_verification/diagnostics.h"

namespace compiler {

using namespace semantic_analysis;

struct Compiler  {
  using Task = nth::coroutine<
      void,
      nth::buffer_sufficient_for<std::vector<QualifiedType>, FunctionData>>;

  using signature = Task();

  explicit Compiler(module::Resources &resources, Context &c)
      : resources_(resources),
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

  template <typename D>
  void ConsumeDiagnostic(D &&d) {
    resources().diagnostic_consumer().Consume(std::forward<D>(d));
  }

  Task operator()(ast::Node const *node) { return node->visit(*this); }
  Task operator()(auto const *node) { return TaskFor(node); }

  template <typename NodeType>
  Task TaskFor(NodeType const *) {
    NTH_UNIMPLEMENTED("{}") <<= {nth::type<NodeType>};
  }

  Task TaskFor(ast::Node const *node) { return operator()(node); }
  Task TaskFor(ast::Expression const *node) {
    return operator()(static_cast<ast::Node const *>(node));
  }

  Task TaskFor(ast::SliceType const *);
  Task TaskFor(ast::Terminal const *);
  // Access, Assignment, ArrayLiteral, ArrayType, BinaryOperator,
  // BindingDeclaration, Call, Cast, ComparisonOperator, Declaration,
  // Declaration::Id, EnumLiteral, FunctionLiteral, FunctionType, Identifier,
  // IfStmt, Import, Index, Module, ShortFunctionLiteral, ReturnStmt,
  // UnaryOperator, WhileStmt, ArgumentType, BinaryAssignmentOperator,
  // BlockNode, DesignatedInitializer, InterfaceLiteral, Label, ScopeLiteral,
  // ScopeNode, StructLiteral, YieldStmt

  std::string TypeForDiagnostic(ast::Expression const &expression) const {
    return ::semantic_analysis::TypeForDiagnostic(expression, context(),
                                                  type_system());
  }

  std::string ExpressionForDiagnostic(ast::Expression const &expression) const {
    return ::semantic_analysis::ExpressionForDiagnostic(expression, context(),
                                                        type_system());
  }

 private:
  module::Resources &resources_;
  module::Module &module_;
  Context &context_;
};

}  // namespace compiler

#endif  // ICARUS_COMPILER_COMPILER_H

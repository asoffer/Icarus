#ifndef ICARUS_SEMANTIC_ANALYSIS_BYTE_CODE_EMITTER_H
#define ICARUS_SEMANTIC_ANALYSIS_BYTE_CODE_EMITTER_H

#include "ast/expression.h"
#include "ast/module.h"
#include "jasmin/execute.h"
#include "jasmin/function.h"
#include "jasmin/instructions/core.h"
#include "module/module.h"
#include "module/resources.h"
#include "nth/container/flyweight_map.h"
#include "semantic_analysis/context.h"
#include "semantic_analysis/function_data.h"
#include "semantic_analysis/type_system.h"
#include "vm/execute.h"
#include "vm/function.h"

namespace semantic_analysis {

struct EmitterBase {
  using FunctionData = FunctionData;

  explicit constexpr EmitterBase(Context &c, module::Resources &resources)
      : context_(c), resources_(resources) {}

  Context const &context() const { return context_; }
  Context &context() { return context_; }
  module::Resources &resources() { return resources_; }

  auto &module() const { return resources_.primary_module(); }
  TypeSystem &type_system() const { return module().type_system(); }
  auto &foreign_symbol_map() const { return module().foreign_symbol_map(); }

  template <std::derived_from<EmitterBase> E>
  E as() const {
    return E(context_, resources_);
  }

  // TODO this is reasonable for types that are generally passed in registers,
  // but not great in general.
  template <typename NodeType>
  void EmitInitialize(NodeType const *node, FunctionData data);

  void EmitDefaultInitialize(core::Type type, FunctionData data);

  std::span<std::byte const> EvaluateConstant(ast::Expression const *expr,
                                              QualifiedType qt);

 private:
  Context &context_;
  module::Resources &resources_;
};

template <typename E>
struct Emitter : EmitterBase {
  using signature = void(FunctionData);

  explicit constexpr Emitter(Context &c, module::Resources &resources)
      : EmitterBase(c, resources) {}

  void Emit(ast::Node const *node, FunctionData data) {
    node->visit<E>(static_cast<E &>(*this), data);
  }

  template <typename T>
  T EvaluateAs(ast::Expression const *expression);
};

struct ByteCodeValueEmitter : Emitter<ByteCodeValueEmitter> {
  explicit ByteCodeValueEmitter(Context &c, module::Resources &resources)
      : Emitter<ByteCodeValueEmitter>(c, resources) {}

  void CastTo(ast::Expression const *node, QualifiedType to_qt,
              FunctionData data);

  template <typename NodeType>
  void operator()(NodeType const *node, FunctionData) {
    if (ast::ExpressionType<NodeType>) {
      NOT_YET(nth::type<NodeType>, node->DebugString());
    } else {
      UNREACHABLE(nth::type<NodeType>);
    }
  }

  void operator()(ast::Access const *node, FunctionData data);
  void operator()(ast::Assignment const *node, FunctionData data);
  void operator()(ast::BinaryOperator const *node, FunctionData data);
  void operator()(ast::Call const *node, FunctionData data);
  void operator()(ast::Cast const *node, FunctionData data);
  void operator()(ast::ComparisonOperator const *node, FunctionData data);
  void operator()(ast::EnumLiteral const *node, FunctionData data);
  void operator()(ast::FunctionLiteral const *node, FunctionData data);
  void operator()(ast::FunctionType const *node, FunctionData data);
  void operator()(ast::Declaration::Id const *node, FunctionData data);
  void operator()(ast::Identifier const *node, FunctionData data);
  void operator()(ast::IfStmt const *node, FunctionData data);
  void operator()(ast::Import const *node, FunctionData data);
  void operator()(ast::Index const *node, FunctionData data);
  void operator()(ast::ShortFunctionLiteral const *node, FunctionData data);
  void operator()(ast::SliceType const *node, FunctionData data);
  void operator()(ast::Terminal const *node, FunctionData data);
  void operator()(ast::UnaryOperator const *node, FunctionData data);
  void operator()(ast::WhileStmt const *node, FunctionData data);
  // TODO: ArgumentType, BinaryAssignmentOperator, BlockNode,
  //       DesignatedInitializer, InterfaceLiteral, Label,
  //       ParameterizedStructLiteral, PatternMatch, ScopeLiteral, ScopeNode,
  //       StructLiteral, YieldStmt,
};

struct ByteCodeStatementEmitter : Emitter<ByteCodeStatementEmitter> {
  explicit ByteCodeStatementEmitter(Context &c, module::Resources &resources)
      : Emitter<ByteCodeStatementEmitter>(c, resources) {}

  template <typename NodeType>
  void operator()(NodeType const *node, FunctionData) {
    NOT_YET(nth::type<NodeType>, node->DebugString());
  }

  void operator()(ast::Access const *node, FunctionData data);
  void operator()(ast::Assignment const *node, FunctionData data);
  void operator()(ast::BinaryOperator const *node, FunctionData data);
  void operator()(ast::Call const *node, FunctionData data);
  void operator()(ast::Cast const *node, FunctionData data);
  void operator()(ast::ComparisonOperator const *node, FunctionData data);
  void operator()(ast::Declaration::Id const *node, FunctionData data);
  void operator()(ast::Declaration const *node, FunctionData data);
  void operator()(ast::EnumLiteral const *node, FunctionData data);
  void operator()(ast::FunctionLiteral const *node, FunctionData data);
  void operator()(ast::FunctionType const *node, FunctionData data);
  void operator()(ast::Identifier const *node, FunctionData data);
  void operator()(ast::IfStmt const *node, FunctionData data);
  void operator()(ast::Import const *node, FunctionData data);
  void operator()(ast::Index const *node, FunctionData data);
  void operator()(ast::Module const *node, FunctionData data);
  void operator()(ast::ReturnStmt const *node, FunctionData data);
  void operator()(ast::ShortFunctionLiteral const *node, FunctionData data);
  void operator()(ast::SliceType const *node, FunctionData data);
  void operator()(ast::Terminal const *node, FunctionData data);
  void operator()(ast::UnaryOperator const *node, FunctionData data);
  void operator()(ast::WhileStmt const *node, FunctionData data);
  // TODO: ArgumentType, BinaryAssignmentOperator, BlockNode,
  //       DesignatedInitializer, InterfaceLiteral, Label,
  //       ParameterizedStructLiteral, PatternMatch, ScopeLiteral, ScopeNode,
  //       StructLiteral, YieldStmt
};

struct ByteCodeReferenceEmitter : Emitter<ByteCodeReferenceEmitter> {
  explicit ByteCodeReferenceEmitter(Context &c, module::Resources &resources)
      : Emitter<ByteCodeReferenceEmitter>(c, resources) {}

  template <typename NodeType>
  void operator()(NodeType const *node, FunctionData) {
    NOT_YET(nth::type<NodeType>, node->DebugString());
  }

  void operator()(ast::Identifier const *node, FunctionData data);
  void operator()(ast::Index const *node, FunctionData data);

  // Unreachable operators.
  void operator()(ast::ArgumentType const *, FunctionData) { UNREACHABLE(); }
  void operator()(ast::BlockNode const *, FunctionData) { UNREACHABLE(); }
  void operator()(ast::Declaration const *, FunctionData) { UNREACHABLE(); }
  void operator()(ast::Declaration::Id const *, FunctionData) { UNREACHABLE(); }
  void operator()(ast::DesignatedInitializer const *, FunctionData) {
    UNREACHABLE();
  }
  void operator()(ast::EnumLiteral const *, FunctionData) { UNREACHABLE(); }
  void operator()(ast::FunctionLiteral const *, FunctionData) { UNREACHABLE(); }
  void operator()(ast::Import const *, FunctionData) { UNREACHABLE(); }
  void operator()(ast::InterfaceLiteral const *, FunctionData) {
    UNREACHABLE();
  }
  void operator()(ast::Label const *, FunctionData) { UNREACHABLE(); }
  void operator()(ast::ParameterizedStructLiteral const *, FunctionData) {
    UNREACHABLE();
  }
  void operator()(ast::PatternMatch const *, FunctionData) { UNREACHABLE(); }
  void operator()(ast::ScopeLiteral const *, FunctionData) { UNREACHABLE(); }
  void operator()(ast::SliceType const *, FunctionData) { UNREACHABLE(); }
  void operator()(ast::ShortFunctionLiteral const *, FunctionData) {
    UNREACHABLE();
  }
  void operator()(ast::StructLiteral const *, FunctionData) { UNREACHABLE(); }
  void operator()(ast::YieldStmt const *, FunctionData) { UNREACHABLE(); }
  // TODO: Access, BinaryAssignmentOperator, BinaryOperator, Call, Cast,
  //       ComparisonOperator, ScopeNode, IfStmt, WhileStmt, UnaryOperator
};

template <typename E>
template <typename T>
T Emitter<E>::EvaluateAs(ast::Expression const *expression) {
  auto qt        = context().qualified_type(expression);
  bool has_error = (qt.qualifiers() >= Qualifiers::Error());
  ASSERT(not has_error);

  vm::Function f(0, 1);

  // This `variable_offsets` map is intentionally empty. There will never be
  // declarations from which data needs to be loaded. Because `EvaluateAs` is
  // only to be called on constant expressions, any identifier will refer to a
  // declaration that is constant, and so lookup will happen by loading the
  // value directly rather than adding instructions which load at runtime.
  nth::flyweight_map<ast::Declaration::Id const *, size_t> variable_offsets;

  this->as<ByteCodeValueEmitter>().Emit(expression,
                                        FunctionData(f, variable_offsets));
  f.AppendReturn();

  vm::ArgumentSlice argument_slice(nullptr, 0);
  T result;
  data_types::IntegerTable table;
  vm::Execute(f, vm::ExecutionState{table, type_system(), argument_slice}, {},
              result);
  return result;
}

template <typename NodeType>
void EmitterBase::EmitInitialize(NodeType const *node, FunctionData data) {
  as<ByteCodeValueEmitter>().Emit(node, data);
  std::span qts = context().qualified_types(node);
  if (qts.size() == 1) {
    data.function().AppendStore(SizeOf(qts[0].type(), type_system()).value());
  } else {
    for (auto iter = qts.rbegin(); iter != qts.rend(); ++iter) {
      data.function().AppendDuplicateAt(qts.size());
      data.function().AppendSwap();
      data.function().AppendStore(SizeOf(iter->type(), type_system()).value());
    }
    data.function().AppendDrop(qts.size());
  }
}

}  // namespace semantic_analysis

#endif  // ICARUS_SEMANTIC_ANALYSIS_BYTE_CODE_EMITTER_H

#ifndef ICARUS_SEMANTIC_ANALYSIS_BYTE_CODE_EMITTER_H
#define ICARUS_SEMANTIC_ANALYSIS_BYTE_CODE_EMITTER_H

#include "ast/expression.h"
#include "ast/module.h"
#include "jasmin/execute.h"
#include "jasmin/function.h"
#include "jasmin/instructions/core.h"
#include "module/module.h"
#include "semantic_analysis/context.h"
#include "semantic_analysis/instruction_set.h"
#include "semantic_analysis/type_system.h"

namespace semantic_analysis {

struct FunctionData {
  FunctionData(IrFunction &function,
               base::flyweight_map<ast::Declaration::Id const *, size_t>
                   &variable_offsets)
      : function_(function), variable_offsets_(variable_offsets) {}

  IrFunction &function() { return function_; }

  size_t OffsetFor(ast::Declaration::Id const *id) const {
    auto iter = variable_offsets_.find(id);
    ASSERT(iter != variable_offsets_.end());
    return iter->second;
  }

  base::flyweight_map<ast::Declaration::Id const *, size_t> &offsets() const {
    return variable_offsets_;
  }

 private:
  IrFunction &function_;
  base::flyweight_map<ast::Declaration::Id const *, size_t> &variable_offsets_;
};

struct EmitterBase {
  using FunctionData = FunctionData;

  explicit constexpr EmitterBase(Context &c, module::Module &module)
      : context_(c), module_(module) {}

  Context const &context() const { return context_; }
  Context &context() { return context_; }

  TypeSystem &type_system() const { return module_.type_system(); }
  auto &foreign_function_map() const { return module_.foreign_function_map(); }
  auto &module() const { return module_; }

  template <std::derived_from<EmitterBase> E>
  E as() const {
    return E(context_, module_);
  }

  void EmitDefaultInitialize(core::Type type, FunctionData data);

 private:
  Context &context_;
  module::Module &module_;
};

template <typename E>
struct Emitter : EmitterBase {
  using signature = void(FunctionData);

  explicit constexpr Emitter(Context &c, module::Module &module)
      : EmitterBase(c, module) {}

  void Emit(ast::Node const *node, FunctionData data) {
    node->visit<E>(static_cast<E &>(*this), data);
  }

  template <typename T>
  T EvaluateAs(ast::Expression const *expression);

  std::span<std::byte const> EvaluateConstant(ast::Expression const *expr,
                                              QualifiedType qt);

  // TODO this is reasonable for types that are generally passed in registers,
  // but not great in general.
  template <typename NodeType>
  void EmitInitialize(NodeType const *node, FunctionData data);
};

struct ByteCodeValueEmitter : Emitter<ByteCodeValueEmitter> {
  explicit ByteCodeValueEmitter(Context &c, module::Module &module)
      : Emitter<ByteCodeValueEmitter>(c, module) {}

  template <typename NodeType>
  void operator()(NodeType const *node, FunctionData) {
    if (ast::ExpressionType<NodeType>) {
      NOT_YET(base::meta<NodeType>, node->DebugString());
    } else {
      UNREACHABLE(base::meta<NodeType>);
    }
  }

  void operator()(ast::Access const *node, FunctionData data);
  void operator()(ast::Builtin const *node, FunctionData data);
  void operator()(ast::Call const *node, FunctionData data);
  void operator()(ast::FunctionLiteral const *node, FunctionData data);
  void operator()(ast::FunctionType const *node, FunctionData data);
  void operator()(ast::Declaration::Id const *node, FunctionData data);
  void operator()(ast::Identifier const *node, FunctionData data);
  void operator()(ast::UnaryOperator const *node, FunctionData data);
  void operator()(ast::Terminal const *node, FunctionData data);
  // TODO: ArgumentType, Assignment, BinaryAssignmentOperator,
  //       BinaryOperator, BlockNode,  Cast, ComparisonOperator,
  //       Declaration::Id, DesignatedInitializer, EnumLiteral, Import, Index,
  //       InterfaceLiteral, Label, ParameterizedStructLiteral,
  //       PatternMatch, ProgramArguments, ScopeLiteral, ScopeNode, SliceType,
  //       ShortFunctionLiteral, StructLiteral, YieldStmt, IfStmt, WhileStmt,
};

struct ByteCodeStatementEmitter : Emitter<ByteCodeStatementEmitter> {
  explicit ByteCodeStatementEmitter(Context &c, module::Module &module)
      : Emitter<ByteCodeStatementEmitter>(c, module) {}

  template <typename NodeType>
  void operator()(NodeType const *node, FunctionData) {
    NOT_YET(base::meta<NodeType>, node->DebugString());
  }

  void operator()(ast::Access const *node, FunctionData data);
  void operator()(ast::Builtin const *node, FunctionData data);
  void operator()(ast::Call const *node, FunctionData data);
  void operator()(ast::Declaration::Id const *node, FunctionData data);
  void operator()(ast::Declaration const *node, FunctionData data);
  void operator()(ast::FunctionLiteral const *node, FunctionData data);
  void operator()(ast::FunctionType const *node, FunctionData data);
  void operator()(ast::Identifier const *node, FunctionData data);
  void operator()(ast::Module const *node, FunctionData data);
  void operator()(ast::ReturnStmt const *node, FunctionData data);
  void operator()(ast::UnaryOperator const *node, FunctionData data);
  void operator()(ast::Terminal const *node, FunctionData data);
  // TODO: ArgumentType, Assignment, BinaryAssignmentOperator,
  //       BinaryOperator, BlockNode,  Cast, ComparisonOperator,
  //       Declaration::Id, DesignatedInitializer, EnumLiteral, Import, Index,
  //       InterfaceLiteral, Label, ParameterizedStructLiteral,
  //       PatternMatch, ProgramArguments, ScopeLiteral, ScopeNode, SliceType,
  //       ShortFunctionLiteral, StructLiteral, YieldStmt, IfStmt, WhileStmt,
  //       Call, Declaration::Id, Declaration
};

template <typename E>
template <typename T>
T Emitter<E>::EvaluateAs(ast::Expression const *expression) {
  auto qt        = context().qualified_type(expression);
  bool has_error = (qt.qualifiers() >= Qualifiers::Error());
  ASSERT(has_error == false);

  IrFunction f(0, 1);

  // This `variable_offsets` map is intentionally empty. There will never be
  // declarations from which data needs to be loaded. Because `EvaluateAs` is
  // only to be called on constant expressions, any identifier will refer to a
  // declaration that is constant, and so lookup will happen by loading the
  // value directly rather than adding instructions which load at runtime.
  base::flyweight_map<ast::Declaration::Id const *, size_t> variable_offsets;

  this->as<ByteCodeValueEmitter>().Emit(expression,
                                        FunctionData(f, variable_offsets));
  f.append<jasmin::Return>();

  T result;
  jasmin::Execute(f, {}, result);
  return result;
}

template <typename E>
std::span<std::byte const> Emitter<E>::EvaluateConstant(
    ast::Expression const *expr, QualifiedType qt) {
  ASSERT(qt == context().qualified_type(expr));
  auto [result_ptr, inserted] = context().insert_constant(expr);
  if (inserted) {
    if (PassInRegister(qt, type_system())) {
      IrFunction f(0, 1);

      // This `variable_offsets` map is intentionally empty. There will never
      // be declarations from which data needs to be loaded. Because
      // `EvaluateConstant` is only to be called on constant expressions, any
      // identifier will refer to a declaration that is constant, and so
      // lookup will happen by loading the value directly rather than adding
      // instructions which load at runtime.
      base::flyweight_map<ast::Declaration::Id const *, size_t>
          variable_offsets;

      as<ByteCodeValueEmitter>().Emit(expr, FunctionData(f, variable_offsets));
      f.append<jasmin::Return>();

      jasmin::ValueStack value_stack;
      jasmin::Execute(f, value_stack);
      size_t size = SizeOf(qt.type(), type_system()).value();
      result_ptr->resize(size);
      jasmin::Value::Store(value_stack.pop_value(), result_ptr->data(), size);
      return *result_ptr;
    } else {
      NOT_YET();
    }
  } else {
    return *result_ptr;
  }
}

template <typename E>
template <typename NodeType>
void Emitter<E>::EmitInitialize(NodeType const *node, FunctionData data) {
  as<ByteCodeValueEmitter>().Emit(node, data);
  std::span qts = context().qualified_types(node);
  if (qts.size() == 1) {
    data.function().append<jasmin::Store>(
        SizeOf(qts[0].type(), type_system()).value());
  } else {
    for (auto iter = qts.rbegin(); iter != qts.rend(); ++iter) {
      data.function().append<jasmin::DuplicateAt>(qts.size());
      data.function().append<jasmin::Swap>();
      data.function().append<jasmin::Store>(
          SizeOf(iter->type(), type_system()).value());
    }
    data.function().append<jasmin::Drop>(qts.size());
  }
}

}  // namespace semantic_analysis

#endif  // ICARUS_SEMANTIC_ANALYSIS_BYTE_CODE_EMITTER_H

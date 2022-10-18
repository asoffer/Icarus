#ifndef ICARUS_SEMANTIC_ANALYSIS_BYTE_CODE_EMITTER_H
#define ICARUS_SEMANTIC_ANALYSIS_BYTE_CODE_EMITTER_H

#include "ast/expression.h"
#include "jasmin/execute.h"
#include "jasmin/function.h"
#include "jasmin/instructions/core.h"
#include "semantic_analysis/compiler_state.h"
#include "semantic_analysis/context.h"
#include "semantic_analysis/instruction_set.h"
#include "semantic_analysis/type_system.h"

namespace semantic_analysis {

struct ByteCodeEmitterBase {
  struct FunctionData {
    FunctionData(
        IrFunction &function,
        base::flyweight_map<ast::Declaration::Id const *, size_t> &variable_offsets)
        : function_(function), variable_offsets_(variable_offsets) {}

    IrFunction &function() { return function_; }

    size_t OffsetFor(ast::Declaration::Id const *id) const {
      auto iter = variable_offsets_.find(id);
      ASSERT(iter != variable_offsets_.end());
      return iter->second;
    }

   private:
    IrFunction &function_;
    base::flyweight_map<ast::Declaration::Id const *, size_t> &variable_offsets_;
  };

  using signature = void(FunctionData);

  explicit ByteCodeEmitterBase(Context const *c, CompilerState &compiler_state)
      : compiler_state_(compiler_state), context_(ASSERT_NOT_NULL(c)) {}

  Context const &context() const { return *context_; }

  TypeSystem &type_system() const { return compiler_state_.type_system(); }
  auto &foreign_function_map() const {
    return compiler_state_.foreign_function_map();
  }
  auto &compiler_state() const { return compiler_state_; }

 private:
  CompilerState &compiler_state_; 
  Context const *context_;
};

struct ByteCodeValueEmitter : ByteCodeEmitterBase {
  explicit ByteCodeValueEmitter(Context const *c, CompilerState &compiler_state)
      : ByteCodeEmitterBase(c, compiler_state) {}

  void operator()(auto const *node, FunctionData data) {
    return Emit(node, data);
  }

  void EmitByteCode(ast::Node const *node, FunctionData data) {
    node->visit<ByteCodeValueEmitter>(*this, data);
  }

  template <typename T>
  std::optional<T> EvaluateAs(ast::Expression const *expression) {
    auto qt        = context().qualified_type(expression);
    bool has_error = (qt.qualifiers() >= Qualifiers::Error());
    ASSERT(has_error == false);

    IrFunction f(0, 1);
    base::flyweight_map<ast::Declaration::Id const *, size_t> variable_offsets;
    EmitByteCode(expression, FunctionData(f, variable_offsets));
    f.append<jasmin::Return>();

    T result;
    jasmin::Execute(f, {}, result);
    return result;
  }

  template <typename NodeType>
  void Emit(NodeType const *, FunctionData) {
    NOT_YET(base::meta<NodeType>);
  }

  void Emit(ast::Call const *node, FunctionData data);
  void Emit(ast::Declaration const *node, FunctionData data);
  void Emit(ast::FunctionType const *node, FunctionData data);
  void Emit(ast::Identifier const *node, FunctionData data);
  void Emit(ast::UnaryOperator const *node, FunctionData data);
  void Emit(ast::Terminal const *node, FunctionData data);

  void EmitDefaultInitialize(core::Type type, FunctionData data);
};

}  // namespace semantic_analysis

#endif  // ICARUS_SEMANTIC_ANALYSIS_BYTE_CODE_EMITTER_H

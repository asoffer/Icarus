#ifndef ICARUS_SEMANTIC_ANALYSIS_BYTE_CODE_EMITTER_H
#define ICARUS_SEMANTIC_ANALYSIS_BYTE_CODE_EMITTER_H

#include "ast/expression.h"
#include "jasmin/function.h"
#include "jasmin/instructions/core.h"
#include "semantic_analysis/byte_code/instruction_set.h"
#include "semantic_analysis/context.h"
#include "semantic_analysis/type_system.h"

namespace semantic_analysis {

struct ByteCodeEmitterBase {
  using signature = void(IrFunction &);

  explicit ByteCodeEmitterBase(Context const *c, TypeSystem &type_system)
      : type_system_(type_system), context_(ASSERT_NOT_NULL(c)) {}

  Context const &context() const { return *context_; }

  auto &type_system() const { return type_system_; }

 private:
  TypeSystem &type_system_;
  Context const *context_;
};

struct ByteCodeValueEmitter : ByteCodeEmitterBase {
  explicit ByteCodeValueEmitter(Context const *c, TypeSystem &type_system)
      : ByteCodeEmitterBase(c, type_system) {}

  void operator()(auto const *node, IrFunction &f) { return Emit(node, f); }

  void EmitByteCode(ast::Node const *node, IrFunction &f) {
    node->visit<ByteCodeValueEmitter>(*this, f);
  }

  template <typename NodeType>
  void Emit(NodeType const *node, IrFunction &f) {
    NOT_YET(base::meta<NodeType>);
  }

  void Emit(ast::FunctionType const *node, IrFunction &f);
  void Emit(ast::UnaryOperator const *node, IrFunction &f);
  void Emit(ast::Terminal const *node, IrFunction &f);
};

}  // namespace semantic_analysis

#endif  // ICARUS_SEMANTIC_ANALYSIS_BYTE_CODE_EMITTER_H

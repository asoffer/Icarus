#ifndef ICARUS_SEMANTIC_ANALYSIS_BYTE_CODE_EMITTER_H
#define ICARUS_SEMANTIC_ANALYSIS_BYTE_CODE_EMITTER_H

#include "ast/expression.h"
#include "compiler/context.h"
#include "jasmin/function.h"
#include "jasmin/instructions/core.h"
#include "semantic_analysis/byte_code/instruction_set.h"

namespace semantic_analysis {

struct ByteCodeEmitterBase {
  using signature = void(IrFunction &);

  explicit ByteCodeEmitterBase(compiler::Context const *c)
      : context_(ASSERT_NOT_NULL(c)) {}

  compiler::Context const &context() const { return *context_; }

 private:
  compiler::Context const *context_;
};

struct ByteCodeValueEmitter : ByteCodeEmitterBase {
  explicit ByteCodeValueEmitter(compiler::Context const *c)
      : ByteCodeEmitterBase(c) {}

  void operator()(auto const *node, IrFunction &f) { return Emit(node, f); }

  void EmitByteCode(ast::Node const *node, IrFunction &f) {
    node->visit<ByteCodeValueEmitter>(*this, f);
  }

  template <typename NodeType>
  void Emit(NodeType const *, IrFunction &f) {
    NOT_YET(base::meta<NodeType>);
  }

  void Emit(ast::Terminal const *t, IrFunction &f);
};

}  // namespace semantic_analysis

#endif  // ICARUS_SEMANTIC_ANALYSIS_BYTE_CODE_EMITTER_H

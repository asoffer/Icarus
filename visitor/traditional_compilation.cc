#include "visitor/traditional_compilation.h"

#include "ir/builder.h"
#include "ir/results.h"
#include "visitor/emit_ir.h"
#include "visitor/verify_type.h"

namespace visitor {

TraditionalCompilation::TraditionalCompilation(Module *mod)
    : mod_(mod), ctx_(mod), bldr_(ir::GetBuilder()) {}

#define ICARUS_AST_NODE_X(name)                                                \
  ir::Results TraditionalCompilation::EmitValue(ast::name const *node) {       \
    EmitIr visitor;                                                            \
    return visitor.Val(node, &context());                                      \
  }
#include "ast/node.xmacro.h"
#undef ICARUS_AST_NODE_X

#define ICARUS_AST_NODE_X(name)                                                \
  VerifyResult TraditionalCompilation::VerifyType(ast::name const *node) {     \
    visitor::VerifyType visitor;                                               \
    return visitor(node, &context());                                          \
  }
#include "ast/node.xmacro.h"
#undef ICARUS_AST_NODE_X

}  // namespace visitor

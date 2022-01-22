#include "absl/types/span.h"
#include "ast/ast.h"
#include "ast/scope.h"
#include "compiler/compiler.h"
#include "compiler/emit/compiler_common.h"
#include "ir/value/addr.h"
#include "ir/value/char.h"
#include "ir/value/reg.h"
#include "ir/value/reg_or.h"
#include "type/type.h"
#include "type/typed_value.h"

namespace compiler {

void Compiler::EmitToBuffer(ast::YieldStmt const *node,
                            ir::PartialResultBuffer &) {
  NOT_YET();
}

}  // namespace compiler

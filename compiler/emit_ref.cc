#include "compiler/compiler.h"

#include "ast/ast.h"

#include "ir/interpretter/evaluate.h"
#include "ir/value/addr.h"
#include "type/type.h"
#include "type/typed_value.h"

namespace compiler {

ir::RegOr<ir::Addr> Compiler::EmitRef(ast::Identifier const *node) {
  auto decl_span = data().decls(node);
  ASSERT(decl_span.size() == 1u);
  return data().addr(decl_span[0]);
}

}  // namespace compiler

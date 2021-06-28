#include "ast/ast.h"
#include "compiler/compiler.h"
#include "ir/value/addr.h"
#include "ir/value/reg_or.h"
#include "type/enum.h"
#include "type/flags.h"
#include "type/typed_value.h"

namespace compiler {

void Compiler::EmitToBuffer(ast::InterfaceLiteral const* node,
                            base::untyped_buffer&) {}

}  // namespace compiler

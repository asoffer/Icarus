#ifndef ICARUS_COMPILER_EMIT_FUNCTION_CALL_INFRASTRUCTURE_H
#define ICARUS_COMPILER_EMIT_FUNCTION_CALL_INFRASTRUCTURE_H

#include "ast/ast.h"
#include "ast/scope/exec.h"
#include "ast/scope/fn.h"
#include "base/ptr_span.h"
#include "compiler.h"

namespace compiler {

void MakeAllDestructions(Compiler &compiler, ast::ExecScope const *exec_scope);
void EmitIrForStatements(Compiler &compiler,
                         base::PtrSpan<ast::Node const> span);

void CompleteBody(Compiler *compiler, ast::ShortFunctionLiteral const *node,
                  type::Function const *t);
void CompleteBody(Compiler *compiler, ast::FunctionLiteral const *node,
                  type::Function const *t);
void CompleteBody(Compiler *compiler,
                  ast::ParameterizedStructLiteral const *node);
void CompleteBody(Compiler *compiler, ast::Jump const *node);

void ProcessExecutableBody(Compiler *c, base::PtrSpan<ast::Node const> nodes,
                           ir::CompiledFn *main_fn);

}  // namespace compiler

#endif  // ICARUS_COMPILER_EMIT_FUNCTION_CALL_INFRASTRUCTURE_H

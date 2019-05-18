#define ICARUS_AST_VISITOR_EMIT_IR

#ifdef ICARUS_AST_VISITOR_EMIT_IR
#define ICARUS_AST_VISITOR_ASSIGN_SCOPE
#include "visitor/emit_ir.xmacro.h"
#endif

#ifdef ICARUS_AST_VISITOR_ASSIGN_SCOPE
#include "visitor/assign_scope.xmacro.h"
#endif

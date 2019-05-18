#ifdef ICARUS_VISITOR_EMIT_IR
#define ICARUS_AST_VISITOR_ASSIGN_SCOPE
#include "visitor/emit_ir.xmacro.h"
#endif

#ifdef ICARUS_AST_VISITOR_ASSIGN_SCOPE
#include "visitor/assign_scope.xmacro.h"
#endif

#define ICARUS_VISITOR_FORMAT
#ifdef ICARUS_VISITOR_FORMAT
ICARUS_AST_VISITOR(void format(visitor::Format const *visitor) const,
                   { (*visitor)(this); });
#endif

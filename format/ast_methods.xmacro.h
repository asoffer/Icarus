#include "ast/methods/dump.xmacro.h"
// TODO remove these. they're not strictly necessary.
#include "module/dependent_decls.xmacro.h"
#include "module/assign_scope.xmacro.h"

ICARUS_AST_VISITOR(void ExtractTokens(format::TokenExtractor *visitor) const,
                   { (*visitor)(this); });

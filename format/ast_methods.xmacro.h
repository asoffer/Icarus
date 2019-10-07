#include "ast/methods/dump.xmacro.h"
#include "module/assign_scope.xmacro.h"
#include "module/dependent_decls.xmacro.h"

ICARUS_AST_VISITOR(void ExtractTokens(format::TokenExtractor *visitor) const,
                   { (*visitor)(this); });

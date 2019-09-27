#include "ast/methods/dump.xmacro.h"

ICARUS_AST_VISITOR(void ExtractTokens(format::TokenExtractor *visitor) const,
                   { (*visitor)(this); });

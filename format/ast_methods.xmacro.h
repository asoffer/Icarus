#include "ast/methods/dump.xmacro.h"

ICARUS_AST_VISITOR(void ExtractTokens(format::TokenExtractor *visitor) const,
                   { (*visitor)(this); });

// TODO remove these. they're not strictly necessary.
ICARUS_AST_VISITOR(void assign_scope(visitor::AssignScope *visitor,
                                     core::Scope *scope),
                   { (*visitor)(this, scope); });
ICARUS_AST_VISITOR(void DependentDecls(visitor::DependentDecls *visitor,
                                       ast::Declaration const *d) const,
                   { (*visitor)(this, d); });

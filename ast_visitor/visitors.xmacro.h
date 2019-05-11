ICARUS_AST_VISITOR(void, assign_scope,
                   (ast_visitor::AssignScope * visitor, core::Scope *scope),
                   { (*visitor)(this, scope); });
ICARUS_AST_VISITOR(void, ExtractJumps,
                   (ast_visitor::ExtractJumps * visitor) const,
                   { (*visitor)(this); });
ICARUS_AST_VISITOR(ast_visitor::VerifyResult, VerifyType,
                   (ast_visitor::VerifyType const *visitor, Context *ctx)
                       const,
                   { return (*visitor)(this, ctx); });

ICARUS_AST_VISITOR(void assign_scope(visitor::AssignScope *visitor,
                                     core::Scope *scope),
                   { (*visitor)(this, scope); });
ICARUS_AST_VISITOR(void DependentDecls(visitor::DependentDecls *visitor,
                                       ast::Declaration const *d) const,
                   { (*visitor)(this, d); });

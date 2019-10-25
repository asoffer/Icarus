ICARUS_AST_VISITOR(void assign_scope(module::AssignScope *visitor,
                                     ast::Scope *scope),
                   { (*visitor)(this, scope); });

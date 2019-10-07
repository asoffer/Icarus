ICARUS_AST_VISITOR(void assign_scope(module::AssignScope *visitor,
                                     core::Scope *scope),
                   { (*visitor)(this, scope); });

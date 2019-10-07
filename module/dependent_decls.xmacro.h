ICARUS_AST_VISITOR(void DependentDecls(module::DependentDecls *visitor,
                                       ast::Declaration const *d) const,
                   { (*visitor)(this, d); });

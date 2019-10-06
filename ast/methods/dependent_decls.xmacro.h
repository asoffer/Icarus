ICARUS_AST_VISITOR(void DependentDeclarations(ast::DependentDecls *visitor,
                                              ast::Declaration const *d) const,
                   { (*visitor)(this, d); });

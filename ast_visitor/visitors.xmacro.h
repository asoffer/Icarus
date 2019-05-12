ICARUS_AST_VISITOR(void, assign_scope,
                   (ast_visitor::AssignScope * visitor, core::Scope *scope),
                   { (*visitor)(this, scope); });
ICARUS_AST_VISITOR(ir::Results, EmitIr,
                   (ast_visitor::EmitIr const *visitor, Context *ctx) const,
                   { return visitor->Val(this, ctx); });
ICARUS_AST_VISITOR(std::vector<ir::RegisterOr<ir::Addr>>, EmitLVal,
                   (ast_visitor::EmitIr const *visitor, Context *ctx) const,
                   { return visitor->Ref(this, ctx); });
ICARUS_AST_VISITOR(void, EmitCopyInit,
                   (ast_visitor::EmitIr const *visitor,
                    type::Typed<ir::Reg> reg, Context *ctx) const,
                   { visitor->CopyInit(this, reg, ctx); });
ICARUS_AST_VISITOR(void, EmitMoveInit,
                   (ast_visitor::EmitIr const *visitor,
                    type::Typed<ir::Reg> reg, Context *ctx) const,
                   { visitor->MoveInit(this, reg, ctx); });
ICARUS_AST_VISITOR(void, ExtractJumps,
                   (ast_visitor::ExtractJumps * visitor) const,
                   { (*visitor)(this); });
ICARUS_AST_VISITOR(ast_visitor::VerifyResult, VerifyType,
                   (ast_visitor::VerifyType const *visitor, Context *ctx)
                       const,
                   { return (*visitor)(this, ctx); });
ICARUS_AST_VISITOR(void, DependentDecls,
                   (ast_visitor::DependentDecls * visitor,
                    ast::Declaration const *d) const,
                   { (*visitor)(this, d); });

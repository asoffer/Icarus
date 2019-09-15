ICARUS_AST_VISITOR(void DumpAst(visitor::DumpAst *visitor) const,
                   { (*visitor)(this); });

#ifdef ICARUS_MATCHER
ICARUS_AST_VISITOR(void match_expr(visitor::Match *visitor,
                                   visitor::MatchState *state) const,
                   { visitor->MatchExpr(this, state); });
#endif // ICARUS_MATCHER 

#ifdef ICARUS_VISITOR_EMIT_IR
#define ICARUS_AST_VISITOR_ASSIGN_SCOPE

ICARUS_AST_VISITOR(
    ir::Results EmitValue(visitor::TraditionalCompilation *visitor) const,
    { return visitor->EmitValue(this); });

ICARUS_AST_VISITOR(ir::Results EmitIr(visitor::EmitIr *visitor, Context *ctx)
                       const,
                   { return visitor->Val(this, ctx); });
ICARUS_AST_VISITOR(std::vector<ir::RegOr<ir::Addr>> EmitLVal(
                       visitor::EmitIr *visitor, Context *ctx) const,
                   { return visitor->Ref(this, ctx); });
ICARUS_AST_VISITOR(void EmitCopyInit(visitor::EmitIr *visitor,
                                     type::Typed<ir::Reg> reg, Context *ctx)
                       const,
                   { visitor->CopyInit(this, reg, ctx); });
ICARUS_AST_VISITOR(void EmitMoveInit(visitor::EmitIr *visitor,
                                     type::Typed<ir::Reg> reg, Context *ctx)
                       const,
                   { visitor->MoveInit(this, reg, ctx); });
ICARUS_AST_VISITOR(visitor::VerifyResult VerifyType(
                       visitor::VerifyType *visitor, Context *ctx) const,
                   { return (*visitor)(this, ctx); });
ICARUS_AST_VISITOR(void ExtractJumps(visitor::ExtractJumps *visitor) const,
                   { (*visitor)(this); });
#endif  // ICARUS_VISITOR_EMIT_IR

#ifdef ICARUS_AST_VISITOR_ASSIGN_SCOPE
ICARUS_AST_VISITOR(void assign_scope(visitor::AssignScope *visitor,
                                     core::Scope *scope),
                   { (*visitor)(this, scope); });
ICARUS_AST_VISITOR(void DependentDecls(visitor::DependentDecls *visitor,
                                       ast::Declaration const *d) const,
                   { (*visitor)(this, d); });
#endif  // ICARUS_AST_VISITOR_ASSIGN_SCOPE

#ifdef ICARUS_VISITOR_FORMAT
ICARUS_AST_VISITOR(void format(visitor::Format const *visitor) const,
                   { (*visitor)(this); });
#endif  // ICARUS_VISITOR_FORMAT

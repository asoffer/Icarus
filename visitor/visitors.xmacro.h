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
    ir::Results EmitValue(compiler::Compiler *visitor) const,
    { return visitor->EmitValue(this); });

ICARUS_AST_VISITOR(std::vector<ir::RegOr<ir::Addr>> EmitRef(
                       compiler::Compiler *visitor) const,
                   { return visitor->EmitRef(this); });

ICARUS_AST_VISITOR(void EmitCopyInit(compiler::Compiler *visitor,
                                     type::Typed<ir::Reg> reg)
                       const,
                   { visitor->EmitCopyInit(this, reg); });
ICARUS_AST_VISITOR(void EmitMoveInit(compiler::Compiler *visitor,
                                     type::Typed<ir::Reg> reg)
                       const,
                   { visitor->EmitMoveInit(this, reg); });
ICARUS_AST_VISITOR(compiler::VerifyResult VerifyType(
                       compiler::Compiler *visitor) const,
                   { return visitor->VerifyType(this); });
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
ICARUS_AST_VISITOR(void ExtractTokens(format::TokenExtractor *visitor) const,
                   { (*visitor)(this); });
#endif  // ICARUS_VISITOR_FORMAT

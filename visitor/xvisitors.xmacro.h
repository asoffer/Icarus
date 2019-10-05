#include "ast/methods/dump.xmacro.h"

ICARUS_AST_VISITOR(void match_expr(visitor::Match *visitor,
                                   visitor::MatchState *state) const,
                   { visitor->MatchExpr(this, state); });

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

ICARUS_AST_VISITOR(void assign_scope(visitor::AssignScope *visitor,
                                     core::Scope *scope),
                   { (*visitor)(this, scope); });
ICARUS_AST_VISITOR(void DependentDecls(visitor::DependentDecls *visitor,
                                       ast::Declaration const *d) const,
                   { (*visitor)(this, d); });

#include "ast/methods/dump.xmacro.h"
#include "match/match_expr.xmacro.h"
#include "module/assign_scope.xmacro.h"
#include "module/dependent_decls.xmacro.h"

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

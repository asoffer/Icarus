ICARUS_AST_VISITOR(ir::Results EmitIr(visitor::EmitIr const *visitor,
                                      Context *ctx) const,
                   { return visitor->Val(this, ctx); });
ICARUS_AST_VISITOR(std::vector<ir::RegisterOr<ir::Addr>> EmitLVal(
                       visitor::EmitIr const *visitor, Context *ctx) const,
                   { return visitor->Ref(this, ctx); });
ICARUS_AST_VISITOR(void EmitCopyInit(visitor::EmitIr const *visitor,
                                     type::Typed<ir::Reg> reg, Context *ctx)
                       const,
                   { visitor->CopyInit(this, reg, ctx); });
ICARUS_AST_VISITOR(void EmitMoveInit(visitor::EmitIr const *visitor,
                                     type::Typed<ir::Reg> reg, Context *ctx)
                       const,
                   { visitor->MoveInit(this, reg, ctx); });
ICARUS_AST_VISITOR(visitor::VerifyResult VerifyType(
                       visitor::VerifyType const *visitor, Context *ctx)
                       const,
                   { return (*visitor)(this, ctx); });
ICARUS_AST_VISITOR(void ExtractJumps(visitor::ExtractJumps *visitor) const,
                   { (*visitor)(this); });

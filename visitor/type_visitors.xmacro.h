#ifdef ICARUS_VISITOR_EMIT_IR
ICARUS_TYPE_VISITOR(void EmitDestroy(visitor::EmitIr const *visitor,
                                     ir::Reg reg, Context *ctx) const,
                    { visitor->Destroy(this, reg, ctx); });
ICARUS_TYPE_VISITOR(
    void EmitCopyAssign(visitor::EmitIr const *visitor,
                        type::Type const *from_type, ir::Results const &from,
                        ir::RegisterOr<ir::Addr> to, Context *ctx) const,
    {
      visitor->CopyAssign(this, to, type::Typed{from, from_type}, ctx);
    });
ICARUS_TYPE_VISITOR(
    void EmitMoveAssign(visitor::EmitIr const *visitor,
                        type::Type const *from_type, ir::Results const &from,
                        ir::RegisterOr<ir::Addr> to, Context *ctx) const,
    {
      visitor->MoveAssign(this, to, type::Typed{from, from_type}, ctx);
    });
ICARUS_TYPE_VISITOR(void EmitDefaultInit(visitor::EmitIr const *visitor,
                                         ir::Reg reg, Context *ctx) const,
                    { visitor->DefaultInit(this, reg, ctx); });
ICARUS_TYPE_VISITOR(void EmitPrint(visitor::EmitIr const *visitor,
                                   ir::Results const &val, Context *ctx) const,
                    { visitor->Print(this, val, ctx); });

ICARUS_TYPE_VISITOR(bool IsDefaultInitializable() const, {
  return visitor::TypeQuery::IsDefaultInitializable(this);
});
ICARUS_TYPE_VISITOR(bool IsCopyable() const,
                    { return visitor::TypeQuery::IsCopyable(this); });
ICARUS_TYPE_VISITOR(bool IsMovable() const,
                    { return visitor::TypeQuery::IsMovable(this); });
ICARUS_TYPE_VISITOR(bool HasDestructor() const,
                    { return visitor::TypeQuery::HasDestructor(this); });
#endif

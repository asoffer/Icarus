#ifdef ICARUS_VISITOR_EMIT_IR
ICARUS_TYPE_VISITOR(void EmitDestroy(visitor::TraditionalCompilation *visitor,
                                     ir::Reg reg) const,
                    { visitor->EmitDestroy(this, reg); });
ICARUS_TYPE_VISITOR(
    void EmitCopyAssign(visitor::TraditionalCompilation *visitor,
                        type::Type const *from_type, ir::Results const &from,
                        ir::RegOr<ir::Addr> to) const,
    {
      visitor->EmitCopyAssign(this, to, type::Typed{from, from_type});
    });
ICARUS_TYPE_VISITOR(
    void EmitMoveAssign(visitor::TraditionalCompilation *visitor,
                        type::Type const *from_type, ir::Results const &from,
                        ir::RegOr<ir::Addr> to) const,
    {
      visitor->EmitMoveAssign(this, to, type::Typed{from, from_type});
    });
ICARUS_TYPE_VISITOR(
    void EmitDefaultInit(visitor::TraditionalCompilation *visitor, ir::Reg reg)
        const,
    { visitor->EmitDefaultInit(this, reg); });

ICARUS_TYPE_VISITOR(void EmitPrint(visitor::TraditionalCompilation *visitor,
                                   ir::Results const &val) const,
                    { visitor->EmitPrint(this, val); });

ICARUS_TYPE_VISITOR(bool IsDefaultInitializable() const, {
  return visitor::TypeQuery::IsDefaultInitializable(this);
});
ICARUS_TYPE_VISITOR(bool IsCopyable() const,
                    { return visitor::TypeQuery::IsCopyable(this); });
ICARUS_TYPE_VISITOR(bool IsMovable() const,
                    { return visitor::TypeQuery::IsMovable(this); });
ICARUS_TYPE_VISITOR(bool HasDestructor() const,
                    { return visitor::TypeQuery::HasDestructor(this); });

ICARUS_TYPE_VISITOR(bool ReinterpretableAs(type::Type const *other) const, {
  return visitor::TypeQuery::ReinterpretableAs(this, other);
});

#endif

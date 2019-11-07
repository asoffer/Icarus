#include "module/extract_defining_modules.xmacro.h"

ICARUS_TYPE_VISITOR(void EmitDestroy(compiler::Compiler *visitor, ir::Reg reg)
                        const,
                    { visitor->EmitDestroy(this, reg); });
ICARUS_TYPE_VISITOR(
    void EmitCopyAssign(compiler::Compiler *visitor,
                        type::Type const *from_type, ir::Results const &from,
                        ir::RegOr<ir::Addr> to) const,
    {
      visitor->EmitCopyAssign(this, to, type::Typed{from, from_type});
    });
ICARUS_TYPE_VISITOR(
    void EmitMoveAssign(compiler::Compiler *visitor,
                        type::Type const *from_type, ir::Results const &from,
                        ir::RegOr<ir::Addr> to) const,
    {
      visitor->EmitMoveAssign(this, to, type::Typed{from, from_type});
    });
ICARUS_TYPE_VISITOR(void EmitDefaultInit(compiler::Compiler *visitor,
                                         ir::Reg reg) const,
                    { visitor->EmitDefaultInit(this, reg); });

ICARUS_TYPE_VISITOR(void EmitPrint(compiler::Compiler *visitor,
                                   ir::Results const &val) const,
                    { visitor->EmitPrint(this, val); });

#include "ir/cmd_buffer.h"

#include <string_view>
#include <type_traits>

#include "base/debug.h"

namespace ir {
struct CompiledFn;

#if defined(ICARUS_DEBUG)
#define DEBUG_CASES CASE(DebugIrCmd);
#else
#define DEBUG_CASES
#endif

#define CASES                                                                  \
  DEBUG_CASES                                                                  \
  CASE(PrintCmd);                                                              \
  CASE(AddCmd);                                                                \
  CASE(SubCmd);                                                                \
  CASE(MulCmd);                                                                \
  CASE(DivCmd);                                                                \
  CASE(ModCmd);                                                                \
  CASE(NegCmd);                                                                \
  CASE(NotCmd);                                                                \
  CASE(LtCmd);                                                                 \
  CASE(LeCmd);                                                                 \
  CASE(EqCmd);                                                                 \
  CASE(NeCmd);                                                                 \
  CASE(GeCmd);                                                                 \
  CASE(GtCmd);                                                                 \
  CASE(StoreCmd);                                                              \
  CASE(LoadCmd);                                                               \
  CASE(VariantCmd);                                                            \
  CASE(TupleCmd);                                                              \
  CASE(ArrowCmd);                                                              \
  CASE(PtrCmd);                                                                \
  CASE(BufPtrCmd);                                                             \
  CASE(JumpCmd);                                                               \
  CASE(XorFlagsCmd);                                                           \
  CASE(AndFlagsCmd);                                                           \
  CASE(OrFlagsCmd);                                                            \
  CASE(CastCmd);                                                               \
  CASE(RegisterCmd);                                                           \
  CASE(ReturnCmd);                                                             \
  CASE(EnumerationCmd);                                                        \
  CASE(StructCmd);                                                             \
  CASE(OpaqueTypeCmd);                                                         \
  CASE(SemanticCmd);                                                           \
  CASE(LoadSymbolCmd);                                                         \
  CASE(TypeInfoCmd);                                                           \
  CASE(AccessCmd);                                                             \
  CASE(VariantAccessCmd);                                                      \
  CASE(CallCmd);                                                               \
  CASE(BlockCmd);                                                              \
  CASE(ScopeCmd)

std::string CmdBuffer::to_string() const {
  // Come up with a better/more-permanent solution here.
  std::string s;
  /*
  auto iter = buf_.begin();
  while (iter < buf_.end()) {
    switch (iter.read<cmd_index_t>()) {
#define CASE(type)                                                             \
  case type::index:                                                            \
    s.append("\n" #type ": ");                                                 \
    s.append(type::DebugString(&iter));                                        \
    break
      CASES;
#undef CASE
    }
  }
  */
  return s;
}

#undef CASES
#undef DEBUG_CASES

}  // namespace ir

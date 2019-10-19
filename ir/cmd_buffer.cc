#include "ir/cmd_buffer.h"

#include <string_view>
#include <type_traits>

#include "backend/exec.h"
#include "base/debug.h"
#include "ir/basic_block.h"
#include "ir/cmd/basic.h"
#include "ir/cmd/call.h"
#include "ir/cmd/cast.h"
#include "ir/cmd/jumps.h"
#include "ir/cmd/load.h"
#include "ir/cmd/misc.h"
#include "ir/cmd/print.h"
#include "ir/cmd/register.h"
#include "ir/cmd/return.h"
#include "ir/cmd/scope.h"
#include "ir/cmd/store.h"
#include "ir/cmd/types.h"
#include "ir/compiled_fn.h"

namespace ir {

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

BasicBlock const* CmdBuffer::Execute(std::vector<ir::Addr> const& ret_slots,
                                     backend::ExecContext* ctx) const {
  auto iter = buf_.begin();
  DEBUG_LOG("dbg")(buf_);
  while (true) {
    DEBUG_LOG("dbg")(buf_.begin(), buf_.size());
    ASSERT(iter < buf_.end());
    auto cmd_index = iter.read<cmd_index_t>();
    switch (cmd_index) {
#define CASE(type)                                                             \
  case type::index: {                                                          \
    DEBUG_LOG("dbg")(#type);                                                   \
    if (auto result = type::Execute(&iter, ret_slots, ctx)) { return result; } \
  } break
      CASES;
#undef CASE
      default: UNREACHABLE(static_cast<int>(cmd_index));
    }
  }
}

void CmdBuffer::UpdateForInlining(Inliner const& inliner) {
  auto iter = buf_.begin();
  DEBUG_LOG("dbg")(buf_);

  while (iter < buf_.end()) {
    switch (iter.read<cmd_index_t>()) {
#define CASE(type)                                                             \
  case type::index:                                                            \
    DEBUG_LOG("dbg")(#type ": ", iter);                                        \
    type::UpdateForInlining(&iter, inliner);                                   \
    break
      CASES;
#undef CASE
    }
  }

  DEBUG_LOG("dbg")(buf_);
}

std::string CmdBuffer::to_string() const {
  // Come up with a better/more-permanent solution here.
  std::string s;
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
  return s;
}

#undef CASES
#undef DEBUG_CASES

size_t GetOffset(CompiledFn const* fn, Reg r) {
  return fn->compiler_reg_to_offset_.at(r);
}
}  // namespace ir

#include "ir/cmd_buffer.h"

#include <string_view>
#include <type_traits>

#include "base/debug.h"
#include "ir/cmd/basic.h"
#include "ir/cmd/jumps.h"
#include "ir/cmd/load.h"
#include "ir/cmd/print.h"
#include "ir/cmd/store.h"
#include "ir/cmd/types.h"
#include "ir/compiled_fn.h"

namespace ir {

BlockIndex CmdBuffer::Execute(std::vector<ir::Addr> const& ret_slots,
                              backend::ExecContext* ctx) {
  auto iter = buf_.begin();
  while (true) {
    DEBUG_LOG("dbg")(buf_.begin(), buf_.size());
    ASSERT(iter < buf_.end());
    switch (iter.read<uint8_t>()) {
#define CASE(type)                                                             \
  case type::index: {                                                          \
    DEBUG_LOG("dbg")(#type);                                                   \
    auto result = type::Execute(&iter, ret_slots, ctx);                        \
    if (result.has_value()) { return *result; }                                \
  } break
      CASE(LegacyCmd);
      CASE(PrintCmd);
      CASE(AddCmd);
      CASE(SubCmd);
      CASE(MulCmd);
      CASE(DivCmd);
      CASE(ModCmd);
      CASE(NegCmd);
      CASE(NotCmd);
      CASE(LtCmd);
      CASE(LeCmd);
      CASE(EqCmd);
      CASE(NeCmd);
      CASE(GeCmd);
      CASE(GtCmd);
      CASE(StoreCmd);
      CASE(LoadCmd);
      CASE(VariantCmd);
      CASE(TupleCmd);
      CASE(ArrowCmd);
      CASE(PtrCmd);
      CASE(BufPtrCmd);
      CASE(JumpCmd);
#undef CASE
      default: UNREACHABLE();
    }
  }
}

std::string CmdBuffer::to_string() {
  // Come up with a better/more-permanent solution here.
  return buf_.to_string();
}

size_t GetOffset(CompiledFn const* fn, Reg r) {
  return fn->compiler_reg_to_offset_.at(r);
}
}  // namespace ir

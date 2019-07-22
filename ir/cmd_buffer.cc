#include "ir/cmd_buffer.h"

#include <string_view>
#include <type_traits>

#include "base/debug.h"
#include "ir/cmd/arithmetic.h"
#include "ir/cmd/print.h"
#include "ir/compiled_fn.h"

namespace ir {
BlockIndex CmdBuffer::Execute(std::vector<ir::Addr> const& ret_slots,
                              backend::ExecContext* ctx) {
  auto iter = buf_.begin();
  while (true) {
    ASSERT(iter < buf_.end());
    switch (iter.read<uint8_t>()) {
#define CASE(type)                                                             \
  case type::index: {                                                          \
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
#undef CASE
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

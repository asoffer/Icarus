#ifndef ICARUS_IR_CMD_BUFFER_H
#define ICARUS_IR_CMD_BUFFER_H

#include <cstddef>

#include "base/untyped_buffer.h"
#include "core/arch.h"
#include "ir/cmd.h"
#include "backend/exec.h"

namespace ir {
// TODO blockindex can store its own nullopt.
struct LegacyCmd {
  constexpr static uint8_t index = std::numeric_limits<uint8_t>::max();
  static std::optional<BlockIndex> Execute(
      base::untyped_buffer::iterator* iter,
      std::vector<ir::Addr> const& ret_slots, backend::ExecContext* ctx) {
    auto block = ctx->ExecuteCmd(*iter->read<Cmd*>(), ret_slots);
    if (block == ir::BlockIndex{-2}) { return std::nullopt; }
    return block;
  }

 private:
  Cmd* ptr_;
};

struct CmdBuffer {
 public:
  template <typename CmdType>
    void append_index() {
    buf_.append(CmdType::index);
  }

  template <typename T>
  void append(T const& t) {
    buf_.append(t);
  }

  BlockIndex Execute(std::vector<ir::Addr> const& ret_slots,
                     backend::ExecContext* ctx);


  std::string to_string();

 private:
  base::untyped_buffer buf_;
};

size_t GetOffset(CompiledFn const* fn, Reg r);
}  // namespace ir

#endif  // ICARUS_IR_CMD_BUFFER_H

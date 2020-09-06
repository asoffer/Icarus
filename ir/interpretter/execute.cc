#include "ir/interpretter/execute.h"

#include <string_view>
#include <vector>

#include "absl/random/random.h"
#include "absl/strings/str_format.h"
#include "base/meta.h"
#include "ir/block_def.h"
#include "ir/instruction/instructions.h"
#include "ir/instruction/set.h"
#include "ir/interpretter/architecture.h"
#include "ir/interpretter/foreign.h"
#include "ir/interpretter/instructions.h"
#include "ir/jump.h"
#include "ir/read_only_data.h"
#include "ir/scope_def.h"
#include "ir/value/fn.h"
#include "ir/value/foreign_fn.h"
#include "ir/value/native_fn.h"
#include "type/opaque.h"
#include "type/primitive.h"

namespace interpretter {

// TODO rename the `arguments` parameter. It actually should be arguments and
// space for registers.
void Execute(ir::Fn fn, base::untyped_buffer arguments,
             absl::Span<ir::Addr const> ret_slots, ExecutionContext *ctx) {
  switch (fn.kind()) {
    case ir::Fn::Kind::Native: {
      StackFrame frame(fn.native(), std::move(arguments), &ctx->stack_);
      CallFn(fn.native(), &frame, ret_slots, ctx);
    } break;
    case ir::Fn::Kind::Builtin: {
      CallFn(fn.builtin(), arguments, ret_slots, ctx);
    } break;
    case ir::Fn::Kind::Foreign: {
      CallFn(fn.foreign(), arguments, ret_slots, &ctx->stack_);
    } break;
  }
}

void ExecutionContext::MemCpyRegisterBytes(void *dst, ir::Reg reg,
                                           size_t length) {
  std::memcpy(dst, current_frame()->regs_.raw(reg), length);
}

void ExecutionContext::ExecuteBlocks(absl::Span<ir::Addr const> ret_slots) {
  DEBUG_LOG("dbg-buffer")(*current_frame()->current_block());
  auto &buffer = current_frame()->fn_->byte_code();

  auto iter = buffer.begin();
  while (true) {
    ASSERT(iter < buffer.end());
    ir::cmd_index_t cmd_index = iter.read<ir::cmd_index_t>();
    DEBUG_LOG("dbg-buffer")(cmd_index);

    switch (cmd_index) {
      case ir::internal::kReturnInstruction: return;
      case ir::internal::kUncondJumpInstruction: {
        uintptr_t offset = iter.read<uintptr_t>();
        current_frame()->MoveTo(offset);
        iter = current_frame()->fn_->byte_code().begin();
        iter.skip(offset);
      } break;
      case ir::internal::kCondJumpInstruction: {
        ir::Reg r             = iter.read<ir::Reg>();
        uintptr_t true_block  = iter.read<uintptr_t>();
        uintptr_t false_block = iter.read<uintptr_t>();
        uintptr_t offset      = resolve<bool>(r) ? true_block : false_block;
        current_frame()->MoveTo(offset);
        iter = current_frame()->fn_->byte_code().begin();
        iter.skip(offset);
      } break;
      case ir::LoadInstruction::kIndex: {
        uint16_t num_bytes = iter.read<uint16_t>();
        bool is_reg        = iter.read<bool>();
        ir::Addr addr      = ReadAndResolve<ir::Addr>(is_reg, &iter, this);
        auto result_reg    = iter.read<ir::Reg>().get();
        DEBUG_LOG("load-instruction")(num_bytes, " ", addr, " ", result_reg);
        switch (addr.kind()) {
          case ir::Addr::Kind::Stack: {
            current_frame()->regs_.set_raw(result_reg, stack_.raw(addr.stack()),
                                           num_bytes);
          } break;
          case ir::Addr::Kind::ReadOnly: {
            auto handle = ir::ReadOnlyData.lock();
            current_frame()->regs_.set_raw(
                result_reg, handle->raw(addr.rodata()), num_bytes);
          } break;
          case ir::Addr::Kind::Heap: {
            current_frame()->regs_.set_raw(result_reg, addr.heap(), num_bytes);
          } break;
        }
      } break;

      default: kInstructions[cmd_index](&iter, this, ret_slots);
    }
  }
}

}  // namespace interpretter

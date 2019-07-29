#ifndef ICARUS_IR_CMD_LOAD_H
#define ICARUS_IR_CMD_LOAD_H

#include <string_view>

#include "ir/basic_block.h"
#include "ir/cmd/util.h"
#include "ir/cmd_buffer.h"
#include "ir/reg.h"
#include "type/util.h"

namespace ir {

struct LoadCmd {
  constexpr static cmd_index_t index = 14;

  struct control_bits {
    uint8_t primitive_type : 6;
    uint8_t reg : 1;
  };
  template <typename T>
  static control_bits MakeControlBits(bool reg) {
    control_bits result;
    result.primitive_type = PrimitiveIndex<T>();
    result.reg            = reg;
    return result;
  }

  static std::optional<BlockIndex> Execute(base::untyped_buffer::iterator* iter,
                                           std::vector<Addr> const& ret_slots,
                                           backend::ExecContext* ctx) {
    auto& frame = ctx->call_stack.top();
    auto ctrl   = iter->read<control_bits>();
    auto addr =
        ctrl.reg ? ctx->resolve<Addr>(iter->read<Reg>()) : iter->read<Addr>();
    auto result_reg = iter->read<Reg>();
    DEBUG_LOG("load")("addr = ", addr);
    DEBUG_LOG("load")("result_reg = ", result_reg);
    PrimitiveDispatch(ctrl.primitive_type, [&](auto tag) {
      using type = typename std::decay_t<decltype(tag)>::type;
      switch (addr.kind) {
        case ir::Addr::Kind::Stack:
          frame.regs_.set(GetOffset(frame.fn_, result_reg),
                          ctx->stack_.get<type>(addr.as_stack));
          break;
        case ir::Addr::Kind::ReadOnly: NOT_YET(); break;
        case ir::Addr::Kind::Heap:
          frame.regs_.set(GetOffset(frame.fn_, result_reg),
                          *static_cast<type*>(addr.as_heap));
      }
    });
    return std::nullopt;
  }

  static void UpdateForInlining(base::untyped_buffer::iterator* iter,
                                Inliner const& inliner) {
    auto ctrl = iter->read<control_bits>();
    if (ctrl.reg) {
      inliner.Inline(&iter->read<Reg>());
    } else {
      // TODO: Add core::LayoutRequirements so you can skip forward by the
      // appropriate amount without instantiating so many templates.
      PrimitiveDispatch(ctrl.primitive_type, [&](auto tag) {
        iter->read<typename std::decay_t<decltype(tag)>::type>();
      });
    }

    inliner.Inline(&iter->read<Reg>());  // Result value
  }
};

template <typename T>
TypedRegister<T> Load(RegOr<Addr> addr) {
  auto& blk = GetBlock();
  blk.cmd_buffer_.append_index<LoadCmd>();
  blk.cmd_buffer_.append(LoadCmd::MakeControlBits<T>(addr.is_reg_));
  if (addr.is_reg_) {
    blk.cmd_buffer_.append(addr.reg_);
  } else {
    blk.cmd_buffer_.append(addr.val_);
  }
  TypedRegister<T> result = MakeResult(type::Get<T>());
  blk.cmd_buffer_.append(result);
  DEBUG_LOG("load")(blk.cmd_buffer_.to_string());
  return result;
}

inline Reg Load(RegOr<Addr> r, type::Type const* t) {
  if (t->is<type::Function>()) { return Load<AnyFunc>(r); }
  return type::ApplyTypes<bool, int8_t, int16_t, int32_t, int64_t, uint8_t,
                          uint16_t, uint32_t, uint64_t, float, double,
                          type::Type const*, ir::EnumVal, ir::FlagsVal,
                          ir::Addr, std::string_view, ir::AnyFunc>(
      t, [&](auto type_holder) -> Reg {
        using T = typename decltype(type_holder)::type;
        return Load<T>(r);
      });
}

}  // namespace ir

#endif  // ICARUS_IR_CMD_LOAD_H

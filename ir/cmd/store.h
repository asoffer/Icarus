#ifndef ICARUS_IR_CMD_STORE_H
#define ICARUS_IR_CMD_STORE_H

#include <string_view>

#include "backend/exec.h"
#include "ir/basic_block.h"
#include "ir/cmd/util.h"
#include "ir/cmd_buffer.h"
#include "ir/reg.h"

namespace ir {

struct StoreCmd {
  constexpr static cmd_index_t index = 13;

  struct control_bits {
    uint8_t primitive_type : 6;
    uint8_t reg : 1;
    uint8_t reg_addr : 1;
  };

  template <typename T>
  static control_bits MakeControlBits(bool reg, bool addr) {
    control_bits result;
    result.primitive_type = PrimitiveIndex<T>();
    result.reg            = reg;
    result.reg_addr       = addr;
    return result;
  }

  static BasicBlock const* Execute(base::untyped_buffer::const_iterator* iter,
                                   std::vector<Addr> const& ret_slots,
                                   backend::ExecContext* ctx) {
    auto ctrl = iter->read<control_bits>();
    PrimitiveDispatch(ctrl.primitive_type, [&](auto tag) {
      using T = typename std::decay_t<decltype(tag)>::type;
      T val   = ctrl.reg ? ctx->resolve<T>(iter->read<Reg>()) : iter->read<T>();
      Addr addr = ctrl.reg_addr ? ctx->resolve<Addr>(iter->read<Reg>())
                                : iter->read<Addr>();
      static_assert(not std::is_same_v<T, void*>, "Not handling addresses yet");
      switch (addr.kind) {
        case Addr::Kind::Stack:
          DEBUG_LOG("store")(addr);
          ctx->stack_.set(addr.as_stack, val);
          break;
        case Addr::Kind::ReadOnly:
          NOT_YET(
              "Storing into read-only data seems suspect. Is it just for "
              "initialization?");
          break;
        case Addr::Kind::Heap:
          *ASSERT_NOT_NULL(static_cast<T*>(addr.as_heap)) = val;
      }
    });
    return nullptr;
  }

  static std::string DebugString(base::untyped_buffer::const_iterator* iter) {
    using base::stringify;
    std::string s;
    auto ctrl = iter->read<control_bits>();
    if (ctrl.reg) {
      s.append(stringify(iter->read<Reg>()));
    } else {
      // TODO: Add core::LayoutRequirements so you can skip forward by the
      // appropriate amount without instantiating so many templates.
      PrimitiveDispatch(ctrl.primitive_type, [&](auto tag) {
        s.append(stringify(
            iter->read<typename std::decay_t<decltype(tag)>::type>()));
      });
    }

    s.append(" ");
    if (ctrl.reg_addr) {
      s.append(stringify(iter->read<Reg>()));
    } else {
      // TODO: Add core::LayoutRequirements so you can skip forward by the
      // appropriate amount without instantiating so many templates.
      PrimitiveDispatch(ctrl.primitive_type, [&](auto tag) {
        s.append(stringify(
            iter->read<typename std::decay_t<decltype(tag)>::type>()));
      });
    }
    return s;
  }
};

template <typename T>
void Store(T r, RegOr<Addr> addr) {
  if constexpr (IsRegOr<T>::value) {
    auto& blk = *GetBuilder().CurrentBlock();
    blk.cmd_buffer_.append_index<StoreCmd>();
    blk.cmd_buffer_.append(
        StoreCmd::MakeControlBits<typename T::type>(r.is_reg(), addr.is_reg()));
    r.apply([&](auto v) { blk.cmd_buffer_.append(v); });
    addr.apply([&](auto v) {
      DEBUG_LOG("store")(v);
      blk.cmd_buffer_.append(v);
    });
  } else {
    Store(RegOr<T>(r), addr);
  }
}

}  // namespace ir

#endif  // ICARUS_IR_CMD_STORE_H

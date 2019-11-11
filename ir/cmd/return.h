#ifndef ICARUS_IR_CMD_RETURN_H
#define ICARUS_IR_CMD_RETURN_H

#include "absl/strings/str_cat.h"
#include "ir/cmd/util.h"
#include "ir/cmd_buffer.h"
#include "type/jump.h"

namespace ir {

struct ReturnCmd {
  constexpr static cmd_index_t index = 28;

  struct control_bits {
    uint8_t primitive_type : 6;
    uint8_t reg : 1;
    uint8_t only_get : 1;
  };

  template <typename T>
  static control_bits MakeControlBits(bool reg, bool only_get) {
    control_bits result;
    if (only_get) {
      result.primitive_type = 0;
      result.reg            = 0;
      result.only_get       = 1;
    } else {
      result.primitive_type = PrimitiveIndex<T>();
      result.reg            = reg;
      result.only_get       = 0;
    }
    return result;
  };

  static std::string DebugString(base::untyped_buffer::const_iterator* iter) {
    using base::stringify;
    auto ctrl  = iter->read<control_bits>();
    uint16_t n = iter->read<uint16_t>();

    if (ctrl.only_get) {
      return absl::StrCat(stringify(iter->read<Reg>()), " = get-ret ", n);
    }

    std::string s;
    if (ctrl.reg) {
      s = absl::StrCat(n, " ", stringify(iter->read<Reg>()));
    } else {
      // TODO: Add core::LayoutRequirements so you can skip forward by the
      // appropriate amount without instantiating so many templates.
      PrimitiveDispatch(ctrl.primitive_type, [&](auto tag) {
        s = absl::StrCat(
            n, " ",
            stringify(
                iter->read<typename std::decay_t<decltype(tag)>::type>()));
      });
    }
    return s;
  }
};

template <typename T>
void SetRet(uint16_t n, T val) {
  if constexpr (ir::IsRegOr<T>::value) {
    auto& blk = *GetBuilder().CurrentBlock();
    blk.cmd_buffer_.append_index<ReturnCmd>();
    blk.cmd_buffer_.append(
        ReturnCmd::MakeControlBits<typename T::type>(val.is_reg(), false));
    blk.cmd_buffer_.append(n);
    val.apply([&](auto v) { blk.cmd_buffer_.append(v); });
  } else if constexpr (base::IsTaggedV<T>) {
    static_assert(std::is_same_v<typename T::base_type, Reg>);
    SetRet(n, RegOr<typename T::tag_type>(val));
  } else {
    SetRet(n, RegOr<T>(val));
  }
}

inline void SetRet(uint16_t n, type::Typed<Results> const& r) {
  // if (r.type()->is<type::GenericStruct>()) {
  //   SetRet(n, r->get<AnyFunc>(0));
  if (r.type()->is<type::Jump>()) {
    // TODO currently this has to be implemented outside type::Apply because
    // that's in type.h which is wrong because it forces weird instantiation
    // order issues (type/type.h can't depend on type/jump.h).
    SetRet(n, r->get<AnyFunc>(0));
  } else {
    type::Apply(r.type(), [&](auto tag) {
      using T = typename decltype(tag)::type;
      // if constexpr (std::is_same_v<T, type::Struct const*>) {
      //   auto* t = GetBuilder().CurrentGroup()->type_->output[n];
      //   // TODO guaranteed move-elision
      //   visitor::EmitIr visitor;
      //   t->EmitMoveAssign(&visitor, t, r.get(), GetRet(n, t), ctx);
      //   visitor.CompleteDeferredBodies();
      // } else {
      SetRet(n, r->get<T>(0));
      // }
    });
  }
}

inline base::Tagged<Addr, Reg> GetRet(uint16_t n, type::Type const* t) {
  auto& blk = *GetBuilder().CurrentBlock();
  blk.cmd_buffer_.append(ReturnCmd::MakeControlBits<int>(false, true));
  blk.cmd_buffer_.append(n);
  Reg r = MakeResult(t);
  blk.cmd_buffer_.append(r);
  return r;
}

}  // namespace ir

#endif  // ICARUS_IR_CMD_RETURN_H

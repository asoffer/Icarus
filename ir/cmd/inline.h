#ifndef ICARUS_IR_CMD_INLINE_H
#define ICARUS_IR_CMD_INLINE_H

#include "ir/cmd/basic.h"
#include "ir/cmd/call.h"
#include "ir/cmd/cast.h"
#include "ir/cmd/jumps.h"
#include "ir/cmd/load.h"
#include "ir/cmd/misc.h"
#include "ir/cmd/phi.h"
#include "ir/cmd/print.h"
#include "ir/cmd/register.h"
#include "ir/cmd/return.h"
#include "ir/cmd/scope.h"
#include "ir/cmd/store.h"
#include "ir/cmd/types.h"
#include "ir/compiled_fn.h"
#include "ir/inliner.h"

namespace ir {
template <typename CmdType>
void InlineCmd(base::untyped_buffer::iterator *iter, Inliner const &inliner) {
  if constexpr (std::is_same_v<CmdType, PrintCmd>) {
    auto ctrl = iter->read<typename CmdType::control_bits>();
    if (ctrl.reg) {
      inliner.Inline(&iter->read<Reg>());
    } else {
      // TODO: Add core::LayoutRequirements so you can skip forward by the
      // appropriate amount without instantiating so many templates.
      PrimitiveDispatch(ctrl.primitive_type, [&](auto tag) {
        iter->read<typename std::decay_t<decltype(tag)>::type>();
      });
    }
  } else if constexpr (std::is_same_v<CmdType, NegCmd> or
                       std::is_same_v<CmdType, NotCmd> or
                       std::is_same_v<CmdType, PtrCmd> or
                       std::is_same_v<CmdType, BufPtrCmd>) {
    auto ctrl = iter->read<typename CmdType::control_bits>();
    // TODO: Add core::LayoutRequirements so you can skip forward by the
    // appropriate amount without instantiating so many templates.
    if (ctrl.reg0) {
      inliner.Inline(&iter->read<Reg>());
    } else {
      PrimitiveDispatch(ctrl.primitive_type, [&](auto tag) {
        iter->read<typename std::decay_t<decltype(tag)>::type>();
      });
    }
    // Result value
    inliner.Inline(&iter->read<Reg>(), GetType(ctrl.primitive_type));
  } else if constexpr (std::is_same_v<CmdType, AddCmd> or
                       std::is_same_v<CmdType, SubCmd> or
                       std::is_same_v<CmdType, MulCmd> or
                       std::is_same_v<CmdType, DivCmd> or
                       std::is_same_v<CmdType, ModCmd> or
                       std::is_same_v<CmdType, LtCmd> or
                       std::is_same_v<CmdType, LeCmd> or
                       std::is_same_v<CmdType, EqCmd> or
                       std::is_same_v<CmdType, NeCmd> or
                       std::is_same_v<CmdType, GeCmd> or
                       std::is_same_v<CmdType, GtCmd>) {
    auto ctrl = iter->read<typename CmdType::control_bits>();
    if (ctrl.reg0) {
      inliner.Inline(&iter->read<Reg>());
    } else {
      // TODO: Add core::LayoutRequirements so you can skip forward by the
      // appropriate amount without instantiating so many templates.
      PrimitiveDispatch(ctrl.primitive_type, [&](auto tag) {
        iter->read<typename std::decay_t<decltype(tag)>::type>();
      });
    }

    if (ctrl.reg1) {
      inliner.Inline(&iter->read<Reg>());
    } else {
      // TODO: Add core::LayoutRequirements so you can skip forward by the
      // appropriate amount without instantiating so many templates.
      PrimitiveDispatch(ctrl.primitive_type, [&](auto tag) {
        iter->read<typename std::decay_t<decltype(tag)>::type>();
      });
    }

    // Result value
    inliner.Inline(&iter->read<Reg>(), GetType(ctrl.primitive_type));
  } else if constexpr (std::is_same_v<CmdType, VariantCmd> or
                       std::is_same_v<CmdType, TupleCmd>) {
    internal::Deserialize<uint16_t, type::Type const *>(
        iter, [&inliner](Reg &reg) { inliner.Inline(&reg); });
    // Result value
    inliner.Inline(&iter->read<Reg>(), ::type::Type_);
  } else if constexpr (std::is_same_v<CmdType, StoreCmd>) {
    auto ctrl = iter->read<typename CmdType::control_bits>();
    if (ctrl.reg) {
      inliner.Inline(&iter->read<Reg>());
    } else {
      // TODO: Add core::LayoutRequirements so you can skip forward by the
      // appropriate amount without instantiating so many templates.
      PrimitiveDispatch(ctrl.primitive_type, [&](auto tag) {
        iter->read<typename std::decay_t<decltype(tag)>::type>();
      });
    }

    if (ctrl.reg_addr) {
      inliner.Inline(&iter->read<Reg>());
    } else {
      // TODO: Add core::LayoutRequirements so you can skip forward by the
      // appropriate amount without instantiating so many templates.
      PrimitiveDispatch(ctrl.primitive_type, [&](auto tag) {
        iter->read<typename std::decay_t<decltype(tag)>::type>();
      });
    }
  } else if constexpr (std::is_same_v<CmdType, LoadCmd>) {
    auto ctrl = iter->read<typename CmdType::control_bits>();
    if (ctrl.reg) {
      inliner.Inline(&iter->read<Reg>());
    } else {
      // TODO: Add core::LayoutRequirements so you can skip forward by the
      // appropriate amount without instantiating so many templates.
      PrimitiveDispatch(ctrl.primitive_type, [&](auto tag) {
        iter->read<typename std::decay_t<decltype(tag)>::type>();
      });
    }

    // Result value
    inliner.Inline(&iter->read<Reg>(), GetType(ctrl.primitive_type));
  } else if constexpr (std::is_same_v<CmdType, ArrowCmd>) {
    internal::Deserialize<uint16_t, type::Type const *>(
        iter, [&inliner](Reg &reg) { inliner.Inline(&reg); });
    internal::Deserialize<uint16_t, type::Type const *>(
        iter, [&inliner](Reg &reg) { inliner.Inline(&reg); });
    inliner.Inline(&iter->read<Reg>(), type::Type_);  // Result value
  } else if constexpr (std::is_same_v<CmdType, CallCmd>) {
    // RegOr<AnyFunc> r_fn;
    // if (cmd.call_.fn_.is_reg()) {
    //   auto iter = reg_relocs.find(cmd.call_.fn_.reg());
    //   if (iter == reg_relocs.end()) { goto next_block; }
    //   r_fn = iter->second.get<AnyFunc>(0).reg();
    // } else {
    //   r_fn = cmd.call_.fn_;
    // }

    // Results new_arg_results;
    // for (size_t i = 0; i < cmd.call_.arguments_->results().size(); ++i) {
    //   if (cmd.call_.arguments_->results().is_reg(i)) {
    //     auto iter =
    //         reg_relocs.find(cmd.call_.arguments_->results().get<Reg>(i));
    //     if (iter == reg_relocs.end()) { goto next_block; }
    //     new_arg_results.append(iter->second.GetResult(0));
    //   } else {
    //     new_arg_results.append(
    //         cmd.call_.arguments_->results().GetResult(i));
    //   }
    // }
    // Arguments new_args(cmd.call_.arguments_->type_,
    //                    std::move(new_arg_results));

    // if (cmd.call_.outs_) {
    //   OutParams outs;
    //   for (size_t i = 0; i < cmd.call_.outs_->regs_.size(); ++i) {
    //     if (cmd.call_.outs_->is_loc_[i]) {
    //       auto old_r = cmd.call_.outs_->regs_[i];
    //       auto iter  = reg_relocs.find(old_r);
    //       if (iter == reg_relocs.end()) { goto next_block; }
    //       // TODO reg_relocs.emplace(, op_fn(r0, r1));
    //     } else {
    //       auto r =
    //           Reserve(type::Int64);  // TODO this type is probably wrong.
    //       outs.is_loc_.push_back(false);
    //       outs.regs_.push_back(r);
    //       reg_relocs.emplace(cmd.call_.outs_->regs_[i], r);
    //     }
    //   }
    //   Call(r_fn, std::move(new_args), std::move(outs));
    // } else {
    //   Call(r_fn, std::move(new_args));
    // }
    NOT_YET();

  } else if constexpr (std::is_same_v<CmdType, ReturnCmd>) {
    auto ctrl = iter->read<typename CmdType::control_bits>();
    iter->read<uint16_t>();

    if (ctrl.only_get) {
      inliner.Inline(&iter->read<Reg>());
      return;
    }

    if (ctrl.reg) {
      inliner.Inline(&iter->read<Reg>());
    } else {
      // TODO: Add core::LayoutRequirements so you can skip forward by the
      // appropriate amount without instantiating so many templates.
      PrimitiveDispatch(ctrl.primitive_type, [&](auto tag) {
        iter->read<typename std::decay_t<decltype(tag)>::type>();
      });
    }
  } else if constexpr (std::is_same_v<CmdType, PhiCmd>) {
    NOT_YET();
  } else if constexpr (std::is_same_v<CmdType, JumpCmd>) {
    auto &kind = iter->read<typename CmdType::Kind>();
    switch (kind) {
      case CmdType::Kind::kRet:
        kind = CmdType::Kind::kUncond;
        // We have ensured that any return jump has enough space to hold an
        // unconditional jump so that this write wile inlining does not need a
        // reallocation. This ensures iterators remain valid.
        iter->write(inliner.landing());
        break;
      case CmdType::Kind::kUncond:
        inliner.Inline(iter->read<BasicBlock const *>());
        break;
      case CmdType::Kind::kCond: {
        iter->read<Reg>();
        inliner.Inline(iter->read<BasicBlock const *>());
        inliner.Inline(iter->read<BasicBlock const *>());
      } break;
      case CmdType::Kind::kChoose: NOT_YET();
      default: UNREACHABLE();
    }
  } else if constexpr (std::is_same_v<CmdType, ScopeCmd>) {
    NOT_YET();
  } else if constexpr (std::is_same_v<CmdType, BlockCmd>) {
    NOT_YET();
  } else if constexpr (std::is_same_v<CmdType, EnumerationCmd>) {
    iter->read<bool>();
    uint16_t num_enumerators = iter->read<uint16_t>();
    uint16_t num_specified   = iter->read<uint16_t>();
    iter->read<module::BasicModule *>();
    for (uint16_t i = 0; i < num_enumerators; ++i) {
      // TODO jump ahead.
      iter->read<std::string_view>();
    }

    for (uint16_t i = 0; i < num_specified; ++i) {
      iter->read<uint64_t>();  // index
      bool is_reg = iter->read<bool>();
      if (is_reg) {
        inliner.Inline(&iter->read<Reg>());
      } else {
        iter->read<EnumerationCmd::enum_t>();
      }
    }

    iter->read<Reg>();
  } else if constexpr (std::is_same_v<CmdType, StructCmd>) {
    auto num = iter->read<uint16_t>();
    iter->read<ast::Scope *>();
    iter->read<module::BasicModule *>();
    for (uint16_t i = 0; i < num; ++i) { iter->read<std::string_view>(); }
    internal::Deserialize<uint16_t, type::Type const *>(
        iter, [&inliner](Reg &reg) { inliner.Inline(&reg); });
    inliner.Inline(&iter->read<Reg>(), ::type::Type_);
  } else if constexpr (std::is_same_v<CmdType, OpaqueTypeCmd>) {
    iter->read<module::BasicModule const *>();
    inliner.Inline(&iter->read<Reg>());
  } else if constexpr (std::is_same_v<CmdType, ArrayCmd>) {
    auto ctrl_bits = iter->read<typename CmdType::control_bits>();
    if (ctrl_bits.length_is_reg) {
      inliner.Inline(&iter->read<Reg>());
    } else {
      iter->read<typename CmdType::length_t>();
    }

    if (ctrl_bits.type_is_reg) {
      inliner.Inline(&iter->read<Reg>());
    } else {
      iter->read<type::Type const *>();
    }
#if defined(ICARUS_DEBUG)
  } else if constexpr (std::is_same_v<CmdType, DebugIrCmd>) {
    NOT_YET();
#endif
  } else if constexpr (std::is_same_v<CmdType, CastCmd>) {
    iter->read<uint8_t>();
    iter->read<uint8_t>();
    inliner.Inline(&iter->read<Reg>());  // Input
    inliner.Inline(&iter->read<Reg>());  // Result value
  } else if constexpr (std::is_same_v<CmdType, SemanticCmd>) {
    size_t num_args = 0;
    switch (iter->read<typename CmdType::Kind>()) {
      case CmdType::Kind::Init: num_args = 1; break;
      case CmdType::Kind::Destroy: num_args = 1; break;
      case CmdType::Kind::Move: num_args = 2; break;
      case CmdType::Kind::Copy: num_args = 2; break;
    }

    switch (num_args) {
      case 1: {
        iter->read<type::Type const *>();
        inliner.Inline(&iter->read<Reg>());
      } break;
      case 2: {
        bool to_reg = iter->read<bool>();
        iter->read<type::Type const *>();
        inliner.Inline(&iter->read<Reg>());
        if (to_reg) {
          inliner.Inline(&iter->read<Reg>());
        } else {
          iter->read<Addr>();
        }
      } break;
      default: UNREACHABLE();
    }
  } else if constexpr (std::is_same_v<CmdType, LoadSymbolCmd>) {
    iter->read<std::string_view>();
    iter->read<type::Type const *>();
    inliner.Inline(&iter->read<Reg>());
  } else if constexpr (std::is_same_v<CmdType, TypeInfoCmd>) {
    auto ctrl_bits = iter->read<uint8_t>();
    if (ctrl_bits & 0x01) {
      inliner.Inline(&iter->read<Reg>());
    } else {
      iter->read<type::Type const *>();
    }
    inliner.Inline(&iter->read<Reg>());
  } else if constexpr (std::is_same_v<CmdType, AccessCmd>) {
    auto ctrl_bits = iter->read<typename CmdType::control_bits>();
    iter->read<type::Type const *>();

    if (ctrl_bits.reg_ptr) {
      inliner.Inline(&iter->read<Reg>());
    } else {
      iter->read<Addr>();
    }

    if (ctrl_bits.reg_index) {
      inliner.Inline(&iter->read<Reg>());
    } else {
      iter->read<int64_t>();
    }

    inliner.Inline(&iter->read<Reg>());

  } else if constexpr (std::is_same_v<CmdType, VariantAccessCmd>) {
    bool get_val = iter->read<bool>();
    bool is_reg  = iter->read<bool>();

    if (is_reg) {
      inliner.Inline(&iter->read<Reg>());
    } else {
      iter->read<Addr>();
    }

    if (get_val) { iter->read<type::Variant const *>(); }

    inliner.Inline(&iter->read<Reg>());
  }
}

}  // namespace ir

#endif  // ICARUS_IR_CMD_INLINE_H

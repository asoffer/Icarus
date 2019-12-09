#include "ir/inliner.h"

#include "base/macros.h"
#include "ir/builder.h"
#include "ir/cmd/basic.h"
#include "ir/cmd/call.h"
#include "ir/cmd/cast.h"
#include "ir/cmd/jump.h"
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
#include "ir/reg.h"
#include "ir/stack_frame_allocations.h"
#include "type/function.h"
#include "type/type.h"

namespace ir {
namespace {

struct JumpInliner {
  // Constructs an Inliner which uses `bldr` to inline jumps.
  static JumpInliner Make(Builder &bldr) {
    return JumpInliner(bldr, bldr.CurrentGroup()->reg_to_offset_.size(),
                       bldr.CurrentGroup()->blocks().size() - 1);
  }

  BasicBlock *CopyBlock(BasicBlock const *block_to_copy) {
    auto *block      = bldr_.AddBlock();
    *block           = *block_to_copy;
    block->incoming_ = {};
    block_updater_.emplace(block_to_copy, block);
    return block;
  }

  void Inline(Reg *r, type::Type const *t = nullptr) const {
    if (r->is_arg()) {
      *r = Reg{r->arg_value() + reg_offset_};
    } else if (r->is_out()) {
      // NOT_YET();
    } else {
      *r = Reg{r->value() + reg_offset_};
    }

    if (t) {
      DEBUG_LOG("inline_reserve")("Reserving t = ", t->to_string());
      auto arch = core::Interpretter();
      bldr_.CurrentGroup()->Reserve(*r, t->bytes(arch), t->alignment(arch));
    }
  }

  BasicBlock *CorrespondingBlock(BasicBlock const *b) const {
    return block_updater_.at(b);
  }
  void Inline(BasicBlock **b) const { *b = CorrespondingBlock(*b); }

  void MergeAllocations(internal::BlockGroup *group,
                        StackFrameAllocations const &allocs) {}

 private:
  friend struct ::ir::internal::BlockGroup;
  explicit JumpInliner(Builder &bldr, size_t reg_offset, size_t block_offset)
      : bldr_(bldr), reg_offset_(reg_offset), block_offset_(block_offset) {}

  absl::flat_hash_map<BasicBlock const *, BasicBlock *> block_updater_;
  Builder &bldr_;
  size_t reg_offset_   = 0;
  size_t block_offset_ = 0;
  BasicBlock *land_    = nullptr;
};

// Returns a `BasicBlock*` which is usually `nullptr`, but will be non-null to
// indicate that the block finished via a ChooseJump. The `BasicBlock*` returned
// is the chosen block.
template <typename CmdType>
void InlineCmd(base::untyped_buffer::iterator *iter, JumpInliner &inliner) {
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
    // Nothing to be done here, one-byte command.
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

}  // namespace

absl::flat_hash_map<std::string_view, BasicBlock *> Inline(
    Builder &bldr, Jump const *to_be_inlined,
    absl::Span<ir::Results const> arguments,
    LocalBlockInterpretation const &block_interp) {
  DEBUG_LOG("inliner")(*to_be_inlined);
  absl::flat_hash_map<std::string_view, BasicBlock *> result;

  // Note: It is important that the inliner is created before making registers
  // for each of the arguments, because creating the inliner looks at state on
  // the target function (counting which register it should start from), and
  // this should exclude the registers we create to hold the arguments.

  auto inliner = JumpInliner::Make(bldr);

  auto *start_block          = bldr.CurrentBlock();
  size_t inlined_start_index = bldr.CurrentGroup()->blocks().size();

  for (auto *block_to_be_inlined : to_be_inlined->blocks()) {
    // Copy the block and then scan it for references to things that need to
    // be changed with inlining (e.g., basic blocks or registers).
    //
    // Note that we need to copy all blocks first (or at least allocate them)
    // because we may request a jump downwards (i.e., to a block which we have
    // not yet seen). In other words, we have to make sure that any jump which
    // needs to be updated, the block mapping is already present.
    inliner.CopyBlock(block_to_be_inlined);
  }

  std::vector<Reg> arg_regs;
  arg_regs.reserve(to_be_inlined->type()->args().size());
  size_t i = 0;
  for (type::Type const *t : to_be_inlined->type()->args()) {
    type::Apply(t, [&](auto tag) -> Reg {
      using T = typename decltype(tag)::type;
      return MakeReg(arguments[i++].get<T>(0));
    });
    // TODO Handle types not covered by Apply (structs, etc).
  }

  std::string_view chosen_block;
  for (auto *block_to_be_inlined : to_be_inlined->blocks()) {
    auto *block = inliner.CorrespondingBlock(block_to_be_inlined);
    DEBUG_LOG("inliner-before")(*block_to_be_inlined);
    DEBUG_LOG("inliner-before")("[", block->cmd_buffer_.to_string(), "]");
    auto iter = block->cmd_buffer_.begin();
    // TODO understand and document why/how blocks can be empty here. I would
    // have suspected all blocks should be required
    while (iter != block->cmd_buffer_.end()) {
      switch (iter.read<cmd_index_t>()) {
#define ICARUS_IR_CMD_X(type)                                                  \
  case type::index: {                                                          \
    DEBUG_LOG("inliner")(#type ": ", iter);                                    \
    InlineCmd<type>(&iter, inliner);                                           \
  } break;
#include "ir/cmd/cmd.xmacro.h"
#undef ICARUS_IR_CMD_X
      }
    }

    block->jump_.Visit([&](auto &j) {
      using type = std::decay_t<decltype(j)>;
      if constexpr (std::is_same_v<type, JumpCmd::RetJump>) {
        // TODO somehow we end up creating extra blocks that aren't used.
        // These blocks by default have a return jump at the end. We need to
        // clean these up but in the mean time, we can just ignore them.
      } else if constexpr (std::is_same_v<type, JumpCmd::UncondJump>) {
        inliner.Inline(&j.block);
        j.block->incoming_.insert(block);
      } else if constexpr (std::is_same_v<type, JumpCmd::CondJump>) {
        inliner.Inline(&j.reg);
        inliner.Inline(&j.true_block);
        j.true_block->incoming_.insert(block);
        inliner.Inline(&j.false_block);
        j.false_block->incoming_.insert(block);
      } else if constexpr (std::is_same_v<type, JumpCmd::ChooseJump>) {
        std::string_view next_name;
        for (std::string_view name : j.blocks()) {
          if (name == "start" or name == "exit" or
              block_interp.block_node(name)) {
            next_name = name;
            break;
          }
        }

        // NOTE: `j` is no longer valid because we're overwriting it here.
        auto *entry_block   = bldr.AddBlock();
        bldr.CurrentBlock() = block;
        bldr.UncondJump(entry_block);
        result.emplace(next_name, entry_block);
      } else {
        static_assert(base::always_false<type>());
      }
    });
    DEBUG_LOG("inliner-after")(*block);
  }

  // TODO Merge allocations

  bldr.CurrentBlock() = start_block;
  bldr.UncondJump(bldr.CurrentGroup()->blocks()[inlined_start_index]);

  return result;
}

}  // namespace ir

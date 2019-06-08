#include "opt/combine_prints.h"

#include <algorithm>

#include "absl/strings/str_cat.h"
#include "ir/cmd.h"
#include "ir/compiled_fn.h"
#include "ir/str.h"

#ifdef DBG
namespace debug {
extern bool optimize_ir;
}  // namespace debug
#endif  // DBG

namespace opt {
namespace {
template <typename T>
void Combine(ir::Cmd** target, ir::Cmd* source, ir::RegisterOr<T> r) {
  if (r.is_reg_) {
    *target = nullptr;
  } else if (*target == nullptr) {
    *target = source;
  } else if ((*target)->op_code_ == ir::Op::PrintByteView &&
             !(*target)->byte_view_arg_.is_reg_) {
    if constexpr (std::is_same_v<T, bool>) {
      (*target)->byte_view_arg_.val_ = ir::SaveStringGlobally(absl::StrCat(
          (*target)->byte_view_arg_.val_, r.val_ ? "true" : "false"));
    } else if constexpr (std::is_same_v<T, type::Type const*>) {
      (*target)->byte_view_arg_.val_ = ir::SaveStringGlobally(
          absl::StrCat((*target)->byte_view_arg_.val_, r.val_->to_string()));
    } else {
      (*target)->byte_view_arg_.val_ = ir::SaveStringGlobally(
          absl::StrCat((*target)->byte_view_arg_.val_, r.val_));
    }
    source->op_code_ = ir::Op::Death;
  } else if ((*target)->op_code_ == ir::Op::PrintBool) {
    (*target)->op_code_ = ir::Op::PrintByteView;
    (*target)->byte_view_arg_.val_ =
        (*target)->bool_arg_.val_ ? "true" : "false";
    Combine(target, source, r);
 
  } else if ((*target)->op_code_ == ir::Op::PrintNat8) {
    (*target)->op_code_ = ir::Op::PrintByteView;
    (*target)->byte_view_arg_.val_ =
        ir::SaveStringGlobally(absl::StrCat((*target)->u8_arg_.val_));
    Combine(target, source, r);
  } else if ((*target)->op_code_ == ir::Op::PrintNat16) {
    (*target)->op_code_ = ir::Op::PrintByteView;
    (*target)->byte_view_arg_.val_ =
        ir::SaveStringGlobally(absl::StrCat((*target)->u16_arg_.val_));
    Combine(target, source, r);
  } else if ((*target)->op_code_ == ir::Op::PrintNat32) {
    (*target)->op_code_ = ir::Op::PrintByteView;
    (*target)->byte_view_arg_.val_ =
        ir::SaveStringGlobally(absl::StrCat((*target)->u32_arg_.val_));
    Combine(target, source, r);
  } else if ((*target)->op_code_ == ir::Op::PrintNat64) {
    (*target)->op_code_ = ir::Op::PrintByteView;
    (*target)->byte_view_arg_.val_ =
        ir::SaveStringGlobally(absl::StrCat((*target)->u64_arg_.val_));
    Combine(target, source, r);
  } else if ((*target)->op_code_ == ir::Op::PrintInt8) {
    (*target)->op_code_ = ir::Op::PrintByteView;
    (*target)->byte_view_arg_.val_ =
        ir::SaveStringGlobally(absl::StrCat((*target)->i8_arg_.val_));
    Combine(target, source, r);
  } else if ((*target)->op_code_ == ir::Op::PrintInt16) {
    (*target)->op_code_ = ir::Op::PrintByteView;
    (*target)->byte_view_arg_.val_ =
        ir::SaveStringGlobally(absl::StrCat((*target)->i16_arg_.val_));
    Combine(target, source, r);
  } else if ((*target)->op_code_ == ir::Op::PrintInt32) {
    (*target)->op_code_ = ir::Op::PrintByteView;
    (*target)->byte_view_arg_.val_ =
        ir::SaveStringGlobally(absl::StrCat((*target)->i32_arg_.val_));
    Combine(target, source, r);
  } else if ((*target)->op_code_ == ir::Op::PrintInt64) {
    (*target)->op_code_ = ir::Op::PrintByteView;
    (*target)->byte_view_arg_.val_ =
        ir::SaveStringGlobally(absl::StrCat((*target)->i64_arg_.val_));
    Combine(target, source, r);
  } else if ((*target)->op_code_ == ir::Op::PrintFloat32) {
    (*target)->op_code_ = ir::Op::PrintByteView;
    (*target)->byte_view_arg_.val_ =
        ir::SaveStringGlobally(absl::StrCat((*target)->float32_arg_.val_));
    Combine(target, source, r);
  } else if ((*target)->op_code_ == ir::Op::PrintFloat64) {
    (*target)->op_code_ = ir::Op::PrintByteView;
    (*target)->byte_view_arg_.val_ =
        ir::SaveStringGlobally(absl::StrCat((*target)->float64_arg_.val_));
    Combine(target, source, r);
  } else if ((*target)->op_code_ == ir::Op::PrintType) {
    (*target)->op_code_ = ir::Op::PrintByteView;
    (*target)->byte_view_arg_.val_ =
        ir::SaveStringGlobally((*target)->type_arg_.val_->to_string());
    Combine(target, source, r);
  } else {
    NOT_YET(static_cast<int>((*target)->op_code_));
  }
}
}  // namespace

void CombinePrints(ir::CompiledFn* fn) {
#ifdef DBG
  if (!debug::optimize_ir) return;
#endif  // DBG
  for (auto& block : fn->blocks_) {
    ir::Cmd* print_cmd = nullptr;
    for (auto& cmd : block.cmds_) {
      switch (cmd.op_code_) {
        case ir::Op::PrintBool: Combine(&print_cmd, &cmd, cmd.bool_arg_); break;
        case ir::Op::PrintInt8: Combine(&print_cmd, &cmd, cmd.i8_arg_); break;
        case ir::Op::PrintInt16: Combine(&print_cmd, &cmd, cmd.i16_arg_); break;
        case ir::Op::PrintInt32: Combine(&print_cmd, &cmd, cmd.i32_arg_); break;
        case ir::Op::PrintInt64: Combine(&print_cmd, &cmd, cmd.i64_arg_); break;
        case ir::Op::PrintNat8: Combine(&print_cmd, &cmd, cmd.u8_arg_); break;
        case ir::Op::PrintNat16: Combine(&print_cmd, &cmd, cmd.u16_arg_); break;
        case ir::Op::PrintNat32: Combine(&print_cmd, &cmd, cmd.u32_arg_); break;
        case ir::Op::PrintNat64: Combine(&print_cmd, &cmd, cmd.u64_arg_); break;
        case ir::Op::PrintFloat32: Combine(&print_cmd, &cmd, cmd.float32_arg_); break;
        case ir::Op::PrintFloat64: Combine(&print_cmd, &cmd, cmd.float64_arg_); break;
        case ir::Op::PrintType: Combine(&print_cmd, &cmd, cmd.type_arg_); break;
        case ir::Op::PrintEnum: /* not yet */ print_cmd = nullptr; break;
        case ir::Op::PrintFlags: /* not yet */ print_cmd = nullptr; break;
        case ir::Op::PrintAddr: /* not yet */ print_cmd = nullptr; break;
        case ir::Op::PrintByteView:
          Combine(&print_cmd, &cmd, cmd.byte_view_arg_);
         break;
        case ir::Op::PrintInterface: /* not yet */ print_cmd = nullptr; break;
        case ir::Op::Call: print_cmd = nullptr; break;
        default: break;
      }
    }

    // TODO probably best to have a no-op instead of reusing death.
    block.cmds_.erase(
        std::remove_if(
            std::begin(block.cmds_), std::end(block.cmds_),
            [](ir::Cmd const& cmd) { return cmd.op_code_ == ir::Op::Death; }),
        std::end(block.cmds_));
  }
}
}  // namespace opt

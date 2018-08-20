#include "property/property.h"

namespace prop {
std::pair<IR::Register, base::owned_ptr<IntProp>> IntProp::Make(
    IR::Cmd::LtInt const& lt_int, bool is_true) {
  if (lt_int.args_[0].is_reg_) {
    if (lt_int.args_[1].is_reg_) {
      return std::pair(IR::Register{-1}, nullptr);
    } else {
      auto int_prop    = base::make_owned<IntProp>();
      int_prop->lower_ = !is_true;
      int_prop->bound_ = lt_int.args_[1].val_ - (is_true ? 0 : 1);
      return std::pair(lt_int.args_[0].reg_, std::move(int_prop));
    }
  } else {
    if (lt_int.args_[1].is_reg_) {
      auto int_prop    = base::make_owned<IntProp>();
      int_prop->lower_ = is_true;
      int_prop->bound_ = lt_int.args_[0].val_ + (is_true ? 0 : 1);
      return std::pair(lt_int.args_[1].reg_, std::move(int_prop));
    } else {
      UNREACHABLE();
    }
  }
}
}  // namespace prop

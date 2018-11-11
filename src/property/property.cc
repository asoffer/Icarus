#include "property/property.h"

namespace prop {
// TODO deal with possibiltiy for overflow/underflow
std::pair<ir::Register, base::owned_ptr<IntProp>> IntProp::Make(
    ir::Cmd::LtInt const& lt_int, bool is_true) {
  if (lt_int.args_[0].is_reg_) {
    if (lt_int.args_[1].is_reg_) {
      return std::pair(ir::Register{-1}, nullptr);
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

std::pair<ir::Register, base::owned_ptr<IntProp>> IntProp::Make(
    ir::Cmd::LeInt const& le_int, bool is_true) {
  if (le_int.args_[0].is_reg_) {
    if (le_int.args_[1].is_reg_) {
      return std::pair(ir::Register{-1}, nullptr);
    } else {
      auto int_prop    = base::make_owned<IntProp>();
      int_prop->lower_ = !is_true;
      int_prop->bound_ = le_int.args_[1].val_ + (is_true ? 1 : 0);
      return std::pair(le_int.args_[0].reg_, std::move(int_prop));
    }
  } else {
    if (le_int.args_[1].is_reg_) {
      auto int_prop    = base::make_owned<IntProp>();
      int_prop->lower_ = is_true;
      int_prop->bound_ = le_int.args_[0].val_ - (is_true ? 1 : 0);
      return std::pair(le_int.args_[1].reg_, std::move(int_prop));
    } else {
      UNREACHABLE();
    }
  }
}

std::pair<ir::Register, base::owned_ptr<IntProp>> IntProp::Make(
    ir::Cmd::GtInt const& gt_int, bool is_true) {
  if (gt_int.args_[0].is_reg_) {
    if (gt_int.args_[1].is_reg_) {
      return std::pair(ir::Register{-1}, nullptr);
    } else {
      auto int_prop    = base::make_owned<IntProp>();
      int_prop->lower_ = is_true;
      int_prop->bound_ = gt_int.args_[1].val_ + (is_true ? 0 : 1);
      return std::pair(gt_int.args_[0].reg_, std::move(int_prop));
    }
  } else {
    if (gt_int.args_[1].is_reg_) {
      auto int_prop    = base::make_owned<IntProp>();
      int_prop->lower_ = !is_true;
      int_prop->bound_ = gt_int.args_[0].val_ - (is_true ? 0 : 1);
      return std::pair(gt_int.args_[1].reg_, std::move(int_prop));
    } else {
      UNREACHABLE();
    }
  }
}

std::pair<ir::Register, base::owned_ptr<IntProp>> IntProp::Make(
    ir::Cmd::GeInt const& ge_int, bool is_true) {
  if (ge_int.args_[0].is_reg_) {
    if (ge_int.args_[1].is_reg_) {
      return std::pair(ir::Register{-1}, nullptr);
    } else {
      auto int_prop    = base::make_owned<IntProp>();
      int_prop->lower_ = is_true;
      int_prop->bound_ = ge_int.args_[1].val_ - (is_true ? 1 : 0);
      return std::pair(ge_int.args_[0].reg_, std::move(int_prop));
    }
  } else {
    if (ge_int.args_[1].is_reg_) {
      auto int_prop    = base::make_owned<IntProp>();
      int_prop->lower_ = !is_true;
      int_prop->bound_ = ge_int.args_[0].val_ + (is_true ? 1 : 0);
      return std::pair(ge_int.args_[1].reg_, std::move(int_prop));
    } else {
      UNREACHABLE();
    }
  }
}
}  // namespace prop

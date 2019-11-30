#ifndef ICARUS_IR_REG_H
#define ICARUS_IR_REG_H

#include <string>

#include "base/debug.h"

namespace ir {
// Reg:
// Represents a register in the intermediate representation.
//
// Registers may refer to registers holding values inside a function, function
// arguments, or function outputs.
struct Reg {
 private:
  using underlying_type = uint64_t;

 public:
  constexpr Reg() = default;

  ICARUS_CONSTEXPR explicit Reg(underlying_type val) : val_(val) {
    ASSERT(is_arg() == false);
    ASSERT(is_out() == false);
  }

  ICARUS_CONSTEXPR static Reg Arg(underlying_type val) {
    ASSERT((val & arg_mask) == 0u);
    return MakeReg(val | arg_mask);
  }

  ICARUS_CONSTEXPR static Reg Out(underlying_type val) {
    ASSERT((val & out_mask) == 0u);
    return MakeReg(val | out_mask);
  }

  constexpr bool is_arg() const { return val_ & arg_mask; }
  constexpr bool is_out() const { return val_ & out_mask; }

  ICARUS_CONSTEXPR auto arg_value() const {
    ASSERT(is_arg() == true);
    return val_ & ~arg_mask;
  }

  ICARUS_CONSTEXPR auto out_value() const {
    ASSERT(is_out() == true);
    return val_ & ~out_mask;
  }

  ICARUS_CONSTEXPR auto value() const {
    ASSERT(is_arg() == false);
    ASSERT(is_out() == false);
    return val_;
  }

  template <typename H>
  friend H AbslHashValue(H h, Reg r) {
    return H::combine(std::move(h), r.val_);
  }

  friend std::string stringify(Reg r);

  friend ICARUS_CONSTEXPR bool operator==(Reg lhs, Reg rhs) {
    return lhs.val_ == rhs.val_;
  }

 private:
  constexpr static Reg MakeReg(underlying_type val) {
    Reg r;
    r.val_ = val;
    return r;
  }

  constexpr static underlying_type arg_mask =
      underlying_type{1} << (std::numeric_limits<underlying_type>::digits - 1);
  constexpr static underlying_type out_mask =
      underlying_type{1} << (std::numeric_limits<underlying_type>::digits - 2);

  underlying_type val_ = (std::numeric_limits<underlying_type>::max)();
};

ICARUS_CONSTEXPR bool operator!=(Reg lhs, Reg rhs) { return not(lhs == rhs); }

}  // namespace ir

#endif  // ICARUS_IR_REG_H

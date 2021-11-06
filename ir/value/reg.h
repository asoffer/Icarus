#ifndef ICARUS_IR_VALUE_REG_H
#define ICARUS_IR_VALUE_REG_H

#include <ostream>
#include <string>

#include "base/debug.h"
#include "base/extend.h"
#include "base/extend/absl_hash.h"

namespace ir {
// Reg:
// Represents a register in the intermediate representation.
//
// Registers may refer to registers holding values inside a function, function
// arguments, or function outputs.
//
// TODO: These registers technically store parameters rather than arguments, and
// we should rectify the naming at some point.
struct Reg : base::Extend<Reg, 1>::With<base::AbslHashExtension> {
 private:
  using underlying_type = uint64_t;

 public:
  constexpr Reg() = default;

  enum class Kind { Value = 0, Output = 1, Argument = 2 };

  explicit Reg(underlying_type val) : val_(val) {
    ASSERT(is_arg() == false);
    ASSERT(is_out() == false);
  }

  Kind kind() const {
    return static_cast<Kind>(
        val_ >> (std::numeric_limits<underlying_type>::digits - 2));
  }

  static Reg Arg(underlying_type val) {
    ASSERT((val & arg_mask) == 0u);
    return MakeReg(val | arg_mask);
  }

  static Reg Out(underlying_type val) {
    ASSERT((val & out_mask) == 0u);
    return MakeReg(val | out_mask);
  }

  constexpr bool is_arg() const { return val_ & arg_mask; }
  constexpr bool is_out() const { return val_ & out_mask; }

  auto arg_value() const {
    ASSERT(is_arg() == true);
    return val_ & ~arg_mask;
  }

  auto out_value() const {
    ASSERT(is_out() == true);
    return val_ & ~out_mask;
  }

  auto value() const {
    ASSERT(is_arg() == false);
    ASSERT(is_out() == false);
    return val_;
  }

  friend std::ostream& operator<<(std::ostream& os, Reg r) {
    if (r.is_arg()) { return os << "arg." << r.arg_value(); }
    if (r.is_out()) { return os << "out." << r.out_value(); }
    return os << "r." << r.value();
  }

 private:
  friend base::EnableExtensions;

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

}  // namespace ir

#endif  // ICARUS_IR_VALUE_REG_H

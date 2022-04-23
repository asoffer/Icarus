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
struct Reg : base::Extend<Reg, 1>::With<base::AbslHashExtension> {
 private:
  using underlying_type = uint64_t;

 public:
  constexpr Reg() = default;

  enum class Kind { Value = 0, Output = 1, Parameter = 2, StackAllocation = 3 };

  explicit Reg(underlying_type value) : value_(value) {
    ASSERT(kind() == Kind::Value);
  }

  // Constructs a register with the given value representing a subroutine
  // parameter.
  static Reg Parameter(underlying_type value) {
    return MakeReg(Kind::Parameter, value);
  }

  // Constructs a register with the given value representing a subroutine
  // output.
  static Reg Output(underlying_type value) {
    return MakeReg(Kind::Output, value);
  }

  // Constructs a register with the given value holding the address of a stack
  // allocation made during the subroutine execution.
  static Reg StackAllocation(underlying_type value) {
    return MakeReg(Kind::StackAllocation, value);
  }

  constexpr Kind kind() const {
    return static_cast<Kind>(
        value_ >> (std::numeric_limits<underlying_type>::digits - 2));
  }

  template <Kind k>
  constexpr bool is() const {
    return kind() == k;
  }

  template <Kind k>
  constexpr underlying_type as() const {
    ASSERT(kind() == k);
    return raw_value();
  }

  // Returns the value ignoring the associated kind.
  constexpr underlying_type raw_value() const {
    return value_ & ~KindMask;
  }

  friend std::ostream& operator<<(std::ostream& os, Reg r) {
    static constexpr std::array Prefixes{"r.", "out.", "param.", "alloc."};
    return os << Prefixes[static_cast<underlying_type>(r.kind())]
              << (r.value_ & ~KindMask);
  }

 private:
  friend base::EnableExtensions;

  static Reg MakeReg(Kind k, underlying_type value) {
    ASSERT((value & KindMask) == 0u);
    Reg r;
    r.value_ = Mask(k) | value;
    return r;
  }

  static constexpr underlying_type Mask(Kind k) {
    return (static_cast<underlying_type>(k)
            << (std::numeric_limits<underlying_type>::digits - 2));
  }

  static constexpr underlying_type KindMask =
      underlying_type{3} << (std::numeric_limits<underlying_type>::digits - 2);

  underlying_type value_ = (std::numeric_limits<underlying_type>::max)();
};

}  // namespace ir

#endif  // ICARUS_IR_VALUE_REG_H

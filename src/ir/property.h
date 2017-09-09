#ifndef ICARUS_IR_PROPERTY_H
#define ICARUS_IR_PROPERTY_H

#include "../base/debug.h"
#include "ir.h"

namespace IR {
// TODO make these unreachable rather than default to unsafe
template <typename Number> struct SafeMath {
  static Number Add(Number lhs, Number rhs) {
    WARN_ONCE("Using unsafe default of SafeMath::Add");
    return lhs + rhs;
  }

  static Number Sub(Number lhs, Number rhs) {
    WARN_ONCE("Using unsafe default of SafeMath::Sub");
    return lhs - rhs;
  }

  static Number Mul(Number lhs, Number rhs) {
    WARN_ONCE("Using unsafe default of SafeMath::Mul");
    return lhs * rhs;
  }
};

template <> struct SafeMath<i32> {
  static i32 Add(i32 lhs, i32 rhs) {
    auto result = static_cast<i64>(lhs) + static_cast<i64>(rhs);
    if (result > std::numeric_limits<i32>::max()) {
      return std::numeric_limits<i32>::max();
    } else if (result < std::numeric_limits<i32>::lowest()) {
      return std::numeric_limits<i32>::lowest();
    } else {
      return static_cast<i32>(result);
    }
  }

  static i32 Sub(i32 lhs, i32 rhs) {
    auto result = static_cast<i64>(lhs) - static_cast<i64>(rhs);
    if (result > std::numeric_limits<i32>::max()) {
      return std::numeric_limits<i32>::max();
    } else if (result < std::numeric_limits<i32>::lowest()) {
      return std::numeric_limits<i32>::lowest();
    } else {
      return static_cast<i32>(result);
    }
  }

  static i32 Mul(i32 lhs, i32 rhs) {
    auto result = static_cast<i64>(lhs) * static_cast<i64>(rhs);
    if (result > std::numeric_limits<i32>::max()) {
      return std::numeric_limits<i32>::max();
    } else if (result < std::numeric_limits<i32>::lowest()) {
      return std::numeric_limits<i32>::lowest();
    } else {
      return static_cast<i32>(result);
    }
  }
};

namespace property {
template <typename Number> struct Range : Property {
  Range() {}
  Range(Number min_val, Number max_val)
      : min_(min_val), max_(max_val) {}

  static Range Get(Func *fn, IR::Val arg) {
    return arg.value.is<Register>()
               ? fn->properties_[arg.value.as<Register>()]->as<Range<Number>>()
               : Range<Number>(arg.value.as<Number>(), arg.value.as<Number>());
  }

  Validity Validate(const Val &val) const override {
    return min_ <= val.value.as<Number>() && val.value.as<Number>() <= max_
               ? Validity::Always
               : Validity::Never;
  }

  void WriteTo(std::ostream &os) const override {
    os << "Range[" << min_ << ", " << max_ << "]";
  }

  bool Implies(const Property *prop) const override {
    if (!prop->is<Range<Number>>()) { return false; }
    const auto &range_prop = prop->as<Range<Number>>();
    return min_ >= range_prop.min_ && max_ <= range_prop.max_;
  }

  // Inclusive bounds
  Number min_ = std::numeric_limits<i32>::lowest();
  Number max_ = std::numeric_limits<i32>::max();
};

template <typename Number>
Range<Number> operator+(const Range<Number> &lhs, const Range<Number> &rhs) {
  return Range<Number>(SafeMath<Number>::Add(lhs.min_, rhs.min_),
                       SafeMath<Number>::Add(lhs.max_, rhs.max_));
}

template <typename Number>
Range<Number> operator-(const Range<Number> &lhs, const Range<Number> &rhs) {
  return Range<Number>(SafeMath<Number>::Sub(lhs.min_, rhs.min_),
                       SafeMath<Number>::Sub(lhs.max_, rhs.max_));
}

template <typename Number>
Range<Number> operator*(Range<Number> lhs, const Range<Number> &rhs) {
  auto new_min = SafeMath<Number>::Mul(lhs.min_, rhs.min_);
  auto new_max = new_min;

  for (auto val : {SafeMath<Number>::Mul(lhs.min_, rhs.max_),
                   SafeMath<Number>::Mul(lhs.max_, rhs.min_),
                   SafeMath<Number>::Mul(lhs.max_, rhs.max_)}) {
    std::tie(new_min, new_max) =
        std::make_pair(std::min(new_min, val), std::max(new_max, val));
  }
  return Range<Number>(new_min, new_max);
}

struct BoolProperty: Property {
  BoolProperty(bool b) : kind(b ? Kind::True : Kind::False) {}

  Validity Validate(const Val &val) const override {
    if (val.value.as<bool>()) {
      switch (kind) {
      case Kind::True: return Validity::Always;
      case Kind::False: return Validity::Never;
      default: NOT_YET();
      }
    } else if (!val.value.as<bool>()) {
      switch (kind) {
      case Kind::True: return Validity::Never;
      case Kind::False: return Validity::Always;
      default: NOT_YET();
      }
    }
    NOT_YET();
  }

  void WriteTo(std::ostream &os) const override {
    switch (kind) {
    case Kind::True: os << "bool true"; break;
    case Kind::False: os << "bool false"; break;
    case Kind::Register: os << "bool eq " << reg; break;
    case Kind::NegatedRegister: os << "bool ne " << reg; break;
    }
  }

  bool Implies(const Property *prop) const override {
    if (!prop->is<BoolProperty>()) { return false; }
    // TODO is this really what I mean by "implies"?
    switch (prop->as<BoolProperty>().kind) {
    case Kind::True: return kind == Kind::True;
    case Kind::False: return true;
    default: NOT_YET();
    }
  }

  Register reg;
  enum class Kind : char { True, False, Register, NegatedRegister } kind;
};
} // namespace property
} // namespace IR

#endif // ICARUS_IR_PROPERTY_H

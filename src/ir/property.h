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

enum class Validity : char { Always, MaybeNot, Unknown, Never };

namespace property {
struct Property : public base::Cast<Property> {
  Property() {}
  virtual ~Property() {}
  virtual Property *Clone() const                  = 0;
  virtual Validity Validate(const Val &val) const  = 0;
  virtual void WriteTo(std::ostream &os) const     = 0;
  virtual bool Implies(const Property &prop) const = 0;
};

inline std::ostream &operator<<(std::ostream &os, const Property &prop) {
  prop.WriteTo(os);
  return os;
}

template <typename Number> struct Range : Property {
  Range() {}
  Range(Number min_val, Number max_val) : min_(min_val), max_(max_val) {}
  ~Range() override {}
  Range<Number> *Clone() const { return new Range<Number>(*this); }
  Validity Validate(const Val &val) const override {
    return min_ <= val.value.as<Number>() && val.value.as<Number>() <= max_
               ? Validity::Always
               : Validity::Never;
  }

  void WriteTo(std::ostream &os) const override {
    os << "Range[" << min_ << ", " << max_ << "]";
  }

  bool Implies(const Property &prop) const override {
    if (!prop.is<Range<Number>>()) { return false; }
    const auto &range_prop = prop.as<Range<Number>>();
    return min_ >= range_prop.min_ && max_ <= range_prop.max_;
  }


  static base::owned_ptr<Range<Number>>
  StrongMerge(const Range<Number> &lhs, const Range<Number>& rhs) {
    return base::make_owned<Range<Number>>(std::max(lhs.min_, rhs.min_),
                                           std::min(lhs.max_, rhs.max_));
  }

  static base::owned_ptr<Range<Number>>
  WeakMerge(const std::vector<Range<Number>> &props) {
    if (props.empty()) { return base::make_owned<Range<Number>>(); }

    // TODO this is not the most efficient way to do this because there are lots
    // of early exit criteria, but I'm lazy and this is unlikely to stick
    // around (we need to be able to merge properties generically).

    auto result = base::make_owned<Range<Number>>(props.front());
    for (const auto &prop : props) {
      result->min_ = std::min(result->min_, prop.min_);
      result->max_ = std::max(result->max_, prop.max_);
    }
    return result;
  }

  // Inclusive bounds
  Number min_ = std::numeric_limits<Number>::lowest();
  Number max_ = std::numeric_limits<Number>::max();
};

template <typename Number>
base::owned_ptr<Range<Number>>
operator+(base::owned_ptr<Range<Number>> lhs,
          const base::owned_ptr<Range<Number>> &rhs) {
  lhs->min_ = SafeMath<Number>::Add(lhs->min_, rhs->min_);
  lhs->max_ = SafeMath<Number>::Add(lhs->max_, rhs->max_);
  return lhs;
}

template <typename Number>
base::owned_ptr<Range<Number>>
operator-(base::owned_ptr<Range<Number>> lhs,
          const base::owned_ptr<Range<Number>> &rhs) {
  lhs->min_ = SafeMath<Number>::Sub(lhs->min_, rhs->max_);
  lhs->max_ = SafeMath<Number>::Sub(lhs->max_, rhs->min_);
  return lhs;
}

template <typename Number>
base::owned_ptr<Range<Number>> operator-(base::owned_ptr<Range<Number>> num) {
  auto tmp  = num->min_;
  num->min_ = -num->max_;
  num->max_ = -tmp;
  return num;
}

template <typename Number>
base::owned_ptr<Range<Number>>
operator*(const base::owned_ptr<Range<Number>> &lhs,
          const base::owned_ptr<Range<Number>> &rhs) {
  auto new_min = SafeMath<Number>::Mul(lhs->min_, rhs->min_);
  auto new_max = new_min;

  for (auto val : {SafeMath<Number>::Mul(lhs->min_, rhs->max_),
                   SafeMath<Number>::Mul(lhs->max_, rhs->min_),
                   SafeMath<Number>::Mul(lhs->max_, rhs->max_)}) {
    std::tie(new_min, new_max) =
        std::make_pair(std::min(new_min, val), std::max(new_max, val));
  }
  return base::make_owned<Range<Number>>(new_min, new_max);
}

struct BoolProperty : Property {
  BoolProperty() : kind(Kind::Unknown) {}
  explicit BoolProperty(bool b) : kind(b ? Kind::True : Kind::False) {}
  BoolProperty *Clone() const { return new BoolProperty(*this); }
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
    case Kind::Unknown: os << "bool unknown"; break;
    case Kind::True: os << "bool true"; break;
    case Kind::False: os << "bool false"; break;
    case Kind::Reg: os << "bool eq " << reg; break;
    case Kind::NegReg: os << "bool ne " << reg; break;
    }
  }

  bool Implies(const Property &prop) const override {
    if (!prop.is<BoolProperty>()) { return false; }
    // TODO is this really what I mean by "implies"?
    switch (prop.as<BoolProperty>().kind) {
    case Kind::True: return kind == Kind::True;
    case Kind::False: return kind == Kind::False;
    case Kind::Reg:
      return kind == Kind::Reg && prop.as<BoolProperty>().reg == reg;
    case Kind::NegReg:
      return kind == Kind::NegReg && prop.as<BoolProperty>().reg == reg;
    case Kind::Unknown: return true;
    }
    UNREACHABLE();
  }


  static base::owned_ptr<BoolProperty>
  StrongMerge(const BoolProperty &lhs, const BoolProperty& rhs) {
    if (lhs.kind == Kind::True || rhs.kind == Kind::True) {
      return base::make_owned<BoolProperty>(true);
    }
    else if (lhs.kind == Kind::False || rhs.kind == Kind::False) { 
      return base::make_owned<BoolProperty>(false);
    } else if (lhs.kind == Kind::Reg && rhs.kind == Kind::Reg) {
      auto prop = base::make_owned<BoolProperty>();
      if (lhs.reg == rhs.reg) { prop->reg = lhs.reg; }
      return prop;
    } else if (lhs.kind == Kind::NegReg && rhs.kind == Kind::NegReg) {
      auto prop = base::make_owned<BoolProperty>();
      if (lhs.reg == rhs.reg) { prop->reg = lhs.reg; }
      return prop;
    } else {
      // TODO you can do better than this, but you at least know this much.
      // You'll do better once you can hold many bool properties at once.
      return base::make_owned<BoolProperty>(lhs);
    }
  }

  static base::owned_ptr<BoolProperty>
  WeakMerge(const std::vector<BoolProperty> &props) {
    if (props.empty()) { return base::make_owned<BoolProperty>(); }
    // TODO this is not the most efficient way to do this because there are lots
    // of early exit criteria, but I'm lazy and this is unlikely to stick
    // around (we need to be able to merge properties generically).
    Kind expected_kind = props[0].kind;
    std::unordered_set<Register> regs;
    for (const auto &prop : props) {
      if (prop.kind == Kind::Unknown || prop.kind != expected_kind) {
        return base::make_owned<BoolProperty>();
      }
      regs.insert(prop.reg);
    }

    if (regs.size() != 1) { return base::make_owned<BoolProperty>(); }
    auto result  = base::make_owned<BoolProperty>();
    result->kind = expected_kind;
    result->reg  = *regs.begin();
    return result;
  }

  Register reg;
  enum class Kind : char { Unknown, True, False, Reg, NegReg } kind;
};

inline base::owned_ptr<BoolProperty> operator!(
    base::owned_ptr<BoolProperty> val) {
  switch (val->kind) {
  case BoolProperty::Kind::Unknown: break;
  case BoolProperty::Kind::True: val->kind  = BoolProperty::Kind::False; break;
  case BoolProperty::Kind::False: val->kind = BoolProperty::Kind::True; break;
  case BoolProperty::Kind::Reg: val->kind   = BoolProperty::Kind::NegReg; break;
  case BoolProperty::Kind::NegReg: val->kind = BoolProperty::Kind::Reg; break;
  }
  return val;
}

inline base::owned_ptr<BoolProperty>
operator^(base::owned_ptr<BoolProperty> lhs,
          const base::owned_ptr<BoolProperty> &rhs) {
  switch (lhs->kind) {
  case BoolProperty::Kind::Unknown: return lhs;
  case BoolProperty::Kind::True: return !std::move(rhs);
  case BoolProperty::Kind::False: return rhs;
  case BoolProperty::Kind::Reg:
  case BoolProperty::Kind::NegReg: {
    // TODO Can we do better than this? Is it worth it?
    return base::make_owned<BoolProperty>();
  }
  }
  UNREACHABLE();
}
template <typename Number>
base::owned_ptr<BoolProperty>
operator<(const base::owned_ptr<Range<Number>> &lhs,
          const base::owned_ptr<Range<Number>> &rhs) {
  if (lhs->max_ < rhs->min_) { return base::make_owned<BoolProperty>(true); }
  if (rhs->max_ <= lhs->min_) { return base::make_owned<BoolProperty>(false); }
  return base::make_owned<BoolProperty>();
}

template <typename Number>
base::owned_ptr<BoolProperty>
operator<=(const base::owned_ptr<Range<Number>> &lhs,
           const base::owned_ptr<Range<Number>> &rhs) {
  if (lhs->max_ <= rhs->min_) { return base::make_owned<BoolProperty>(true); }
  if (rhs->max_ < lhs->min_) { return base::make_owned<BoolProperty>(false); }
  return base::make_owned<BoolProperty>();
}

template <typename Number>
base::owned_ptr<BoolProperty>
operator>(const base::owned_ptr<Range<Number>> &lhs,
          const base::owned_ptr<Range<Number>> &rhs) {
  if (lhs->min_ > rhs->max_) { return base::make_owned<BoolProperty>(true); }
  if (rhs->min_ >= lhs->max_) { return base::make_owned<BoolProperty>(false); }
  return base::make_owned<BoolProperty>();
}

template <typename Number>
base::owned_ptr<BoolProperty>
operator>=(const base::owned_ptr<Range<Number>> &lhs,
           const base::owned_ptr<Range<Number>> &rhs) {
  if (lhs->min_ >= rhs->max_) { return base::make_owned<BoolProperty>(true); }
  if (rhs->min_ > lhs->max_) { return base::make_owned<BoolProperty>(false); }
  return base::make_owned<BoolProperty>();
}
} // namespace property
} // namespace IR

#endif // ICARUS_IR_PROPERTY_H

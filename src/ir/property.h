#ifndef ICARUS_IR_PROPERTY_H
#define ICARUS_IR_PROPERTY_H

#include "../base/debug.h"
#include "../base/util.h"

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
  virtual void WriteTo(std::ostream &os) const     = 0;
  virtual bool Implies(const Property &prop) const = 0;
  virtual void Neg()                               = 0;
  std::string to_string() const {
    std::stringstream ss;
    WriteTo(ss);
    return ss.str();
  }
};

inline std::ostream &operator<<(std::ostream &os, const Property &prop) {
  prop.WriteTo(os);
  return os;
}

/*
enum class BoundType : char { Lt, Le, Ge, Gt };
template <typename Number> struct Bound : Property {
  BoundType type_;
  Number num_;

  Bound()              = delete;
  Bound(Bound &&)      = default;
  Bound(const Bound &) = default;
  Bound(Boundtype::Type type, Number num) : type_(type), num_(std::move(num)) {}
};
*/

struct PropertySet {
  static PropertySet WeakMerge(const std::vector<const PropertySet *> &props);
  static PropertySet StrongMerge(const std::vector<const PropertySet *> &props);
  bool Implies(const PropertySet &props) const;

  PropertySet() = default;
  PropertySet(const PropertySet& prop_set) {
    props_.reserve(prop_set.props_.size());
    for (const auto &prop : prop_set.props_) {
      props_.emplace_back(prop->Clone());
    }
  }

  PropertySet &operator=(PropertySet prop_set) {
    props_ = std::move(prop_set).props_;
    return *this;
  }

  std::vector<std::unique_ptr<Property>> props_;
};

template <typename Number> struct Range : public Property {
  Range() {}
  Range(Number min_val, Number max_val) : min_(min_val), max_(max_val) {}
  ~Range() override {}
  Range<Number> *Clone() const { return new Range<Number>(*this); }

  void WriteTo(std::ostream &os) const override {
    os << "Range[" << min_ << ", " << max_ << "]";
  }

  bool Implies(const Property &prop) const override {
    if (!prop.is<Range<Number>>()) { return false; }
    const auto &range_prop = prop.as<Range<Number>>();
    return min_ >= range_prop.min_ && max_ <= range_prop.max_;
  }

  virtual void Neg() {
    using std::swap;
    swap(min_, max_);
    min_ = -min_;
    max_ = -max_;
    // TODO what about handling the fact that you can express lower negative
    // numbers than positive?
  }

  // Inclusive bounds
  Number min_ = std::numeric_limits<Number>::lowest();
  Number max_ = std::numeric_limits<Number>::max();
};

inline std::unique_ptr<Property>
operator+(const std::unique_ptr<Property> &lhs,
          const std::unique_ptr<Property> &rhs) {
  ASSERT_TYPE(Range<i32>, lhs);
  ASSERT_TYPE(Range<i32>, rhs);
  auto lhs_int = lhs->as<Range<i32>>();
  auto rhs_int = rhs->as<Range<i32>>();
  return std::make_unique<Range<i32>>(
      SafeMath<i32>::Add(lhs_int.min_, rhs_int.min_),
      SafeMath<i32>::Add(lhs_int.max_, rhs_int.max_));
}

inline std::unique_ptr<Property>
operator-(const std::unique_ptr<Property> &lhs,
          const std::unique_ptr<Property> &rhs) {
  ASSERT_TYPE(Range<i32>, lhs);
  ASSERT_TYPE(Range<i32>, rhs);
  auto lhs_int = lhs->as<Range<i32>>();
  auto rhs_int = rhs->as<Range<i32>>();
  return std::make_unique<Range<i32>>(
      SafeMath<i32>::Sub(lhs_int.min_, rhs_int.max_),
      SafeMath<i32>::Sub(lhs_int.max_, rhs_int.min_));
}

template <typename Number>
std::unique_ptr<Range<Number>> operator-(std::unique_ptr<Range<Number>> num) {
  auto tmp  = num->min_;
  num->min_ = -num->max_;
  num->max_ = -tmp;
  return num;
}

inline std::unique_ptr<Property>
operator*(const std::unique_ptr<Property> &lhs,
          const std::unique_ptr<Property> &rhs) {
  ASSERT_TYPE(Range<i32>, lhs);
  ASSERT_TYPE(Range<i32>, rhs);
  auto lhs_int = lhs->as<Range<i32>>();
  auto rhs_int = rhs->as<Range<i32>>();

  auto new_min = SafeMath<i32>::Mul(lhs_int.min_, rhs_int.min_);
  auto new_max = new_min;

  for (auto val : {SafeMath<i32>::Mul(lhs_int.min_, rhs_int.max_),
                   SafeMath<i32>::Mul(lhs_int.max_, rhs_int.min_),
                   SafeMath<i32>::Mul(lhs_int.max_, rhs_int.max_)}) {
    std::tie(new_min, new_max) =
        std::pair(std::min(new_min, val), std::max(new_max, val));
  }
  return std::make_unique<Range<i32>>(new_min, new_max);
}

struct BoolProperty : Property {
  BoolProperty() : kind(Kind::Unknown) {}
  explicit BoolProperty(bool b) : kind(b ? Kind::True : Kind::False) {}
  BoolProperty *Clone() const { return new BoolProperty(*this); }

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

  virtual void Neg() {
    switch (kind) {
    case Kind::Unknown: return;
    case Kind::True: kind = Kind::False; return;
    case Kind::False: kind = Kind::True; return;
    case Kind::Reg: kind = Kind::NegReg; return;
    case Kind::NegReg: kind = Kind::Reg; return;
    }
  }

  Register reg;
  enum class Kind : char { Unknown, True, False, Reg, NegReg } kind;
};

/*
inline std::unique_ptr<Property>
operator^(std::unique_ptr<Property> lhs, const std::unique_ptr<Property> &rhs) {
  ASSERT_TYPE(BoolProperty, lhs);
  ASSERT_TYPE(BoolProperty, rhs);
  switch (lhs->as<BoolProperty>().kind) {
  case BoolProperty::Kind::Unknown: return lhs;
  case BoolProperty::Kind::True: {
    // TODO can do this more efficiently avoiding copies to/from stack/heap.
    auto prop = std::make_unique<BoolProperty>(rhs->as<BoolProperty>());
    prop->Neg();
    return prop;
  }
  case BoolProperty::Kind::False: return rhs;
  case BoolProperty::Kind::Reg:
  case BoolProperty::Kind::NegReg: {
    // TODO Can we do better than this? Is it worth it?
    return std::unique_ptr<BoolProperty>();
  }
  }
  UNREACHABLE();
}

inline std::unique_ptr<Property>
operator<(const std::unique_ptr<Property> &lhs,
          const std::unique_ptr<Property> &rhs) {
  ASSERT_TYPE(Range<i32>, lhs);
  ASSERT_TYPE(Range<i32>, rhs);
  auto lhs_int = lhs->as<Range<i32>>();
  auto rhs_int = rhs->as<Range<i32>>();

  if (lhs_int.max_ < rhs_int.min_) {
    return std::make_unique<BoolProperty>(true);
  }
  if (rhs_int.max_ <= lhs_int.min_) {
    return std::make_unique<BoolProperty>(false);
  }
  return std::make_unique<BoolProperty>();
}

inline std::unique_ptr<Property>
operator<=(const std::unique_ptr<Property> &lhs,
           const std::unique_ptr<Property> &rhs) {
  ASSERT_TYPE(Range<i32>, lhs);
  ASSERT_TYPE(Range<i32>, rhs);
  auto lhs_int = lhs->as<Range<i32>>();
  auto rhs_int = rhs->as<Range<i32>>();

  if (lhs_int.max_ <= rhs_int.min_) {
    return std::make_unique<BoolProperty>(true);
  }
  if (rhs_int.max_ < lhs_int.min_) {
    return std::make_unique<BoolProperty>(false);
  }
  return std::make_unique<BoolProperty>();
}

inline std::unique_ptr<Property>
operator>(const std::unique_ptr<Property> &lhs,
          const std::unique_ptr<Property> &rhs) {
  ASSERT_TYPE(Range<i32>, lhs);
  ASSERT_TYPE(Range<i32>, rhs);
  auto lhs_int = lhs->as<Range<i32>>();
  auto rhs_int = rhs->as<Range<i32>>();

  if (lhs_int.min_ > rhs_int.max_) {
    return std::make_unique<BoolProperty>(true);
  }
  if (rhs_int.min_ >= lhs_int.max_) {
    return std::make_unique<BoolProperty>(false);
  }
  return std::make_unique<BoolProperty>();
}

inline std::unique_ptr<Property>
operator>=(const std::unique_ptr<Property> &lhs,
           const std::unique_ptr<Property> &rhs) {
  ASSERT_TYPE(Range<i32>, lhs);
  ASSERT_TYPE(Range<i32>, rhs);
  auto lhs_int = lhs->as<Range<i32>>();
  auto rhs_int = rhs->as<Range<i32>>();

  if (lhs_int.min_ >= rhs_int.max_) {
    return std::make_unique<BoolProperty>(true);
  }
  if (rhs_int.min_ > lhs_int.max_) {
    return std::make_unique<BoolProperty>(false);
  }
  return std::make_unique<BoolProperty>();
}
*/
} // namespace property
} // namespace IR

#endif // ICARUS_IR_PROPERTY_H

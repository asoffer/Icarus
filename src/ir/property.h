#ifndef ICARUS_IR_PROPERTY_H
#define ICARUS_IR_PROPERTY_H

#include "ir.h"

namespace IR {
struct IntProperty : public Property {
  IntProperty(i32 min_val = std::numeric_limits<i32>::lowest(),
              i32 max_val = std::numeric_limits<i32>::max())
      : min_(min_val), max_(max_val) {}
  ~IntProperty() override {}

  Validity Validate(const Val &val) const override {
    return min_ <= val.value.as<i32>() && val.value.as<i32>() <= max_
               ? Validity::Always
               : Validity::Never;
  }

  void WriteTo(std::ostream &os) const override {
    os << "Range[" << min_ << ", " << max_ << "]";
  }

  virtual bool Implies(const Property *prop) const {
    if (!prop->is<IntProperty>()) { return false; }
    const auto &int_prop = prop->as<IntProperty>();
    return min_ >= int_prop.min_ && max_ <= int_prop.max_;
  }

  // Inclusive bounds
  i32 min_ = std::numeric_limits<i32>::lowest();
  i32 max_ = std::numeric_limits<i32>::max();
};

struct RealProperty : public Property {
  // TODO deal with overflow
  RealProperty(double min_val = std::numeric_limits<double>::lowest(),
              double max_val = std::numeric_limits<double>::max())
      : min_(min_val), max_(max_val) {}
  ~RealProperty() override {}

  Validity Validate(const Val &val) const override {
    LOG << min_ << " <= " << val.value.as<double>() << " <= " << max_;
    return min_ <= val.value.as<double>() && val.value.as<double>() <= max_
               ? Validity::Always
               : Validity::Never;
  }

  void WriteTo(std::ostream &os) const override {
    os << "Range[" << min_ << ", " << max_ << "]";
  }

  virtual bool Implies(const Property *prop) const {
    if (!prop->is<RealProperty>()) { return false; }
    const auto &real_prop = prop->as<RealProperty>();
    return min_ >= real_prop.min_ && max_ <= real_prop.max_;
  }

  // Inclusive bounds
  double min_ = std::numeric_limits<double>::lowest();
  double max_ = std::numeric_limits<double>::max();
};

} // namespace IR

#endif // ICARUS_IR_PROPERTY_H

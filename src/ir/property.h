#ifndef ICARUS_IR_PROPERTY_H
#define ICARUS_IR_PROPERTY_H

#include "ir.h"

namespace IR {
struct IntProperty : public Property {
  IntProperty(const Cursor &loc, i64 min_val, i64 max_val)
      : Property(loc), min_(min_val), max_(max_val) {}
  ~IntProperty() override {}

  Validity Validate(const Val &val) const override {
    return min_ <= val.value.as<i64>() && val.value.as<i64>() <= max_
               ? Validity::Always
               : Validity::Never;
  }

  void WriteTo(std::ostream &os) const override {
    os << "Range[" << max_ << ", " << min_ << "]";
  }

  virtual bool Implies(const Property *prop) const {
    if (!prop->is<IntProperty>()) { return false; }
    const auto &int_prop = prop->as<IntProperty>();
    return min_ >= int_prop.min_ && max_ <= int_prop.max_;
  }

  std::unique_ptr<Property> Add(const Val &val) const override {
    if (!val.value.is<i64>()) { return nullptr; }
    // TODO overflow?
    return std::make_unique<IntProperty>(Cursor{}, min_ + val.value.as<i64>(),
                                         max_ + val.value.as<i64>());
  }

  // Inclusive bounds
  i64 min_ = std::numeric_limits<i64>::min();
  i64 max_ = std::numeric_limits<i64>::max();
};

} // namespace IR

#endif // ICARUS_IR_PROPERTY_H

#ifndef ICARUS_PROPERTY_PROPERTY_H
#define ICARUS_PROPERTY_PROPERTY_H

#include <iosfwd>
#include <string>

#include "base/util.h"
#include "base/owned_ptr.h"
#include "ir/cmd.h"

namespace prop {
struct Property : public base::Cast<Property> {
  virtual ~Property() {}
  virtual Property *Clone() const       = 0;
  virtual std::string to_string() const = 0;
};

inline std::ostream &operator<<(std::ostream &os, const Property &p) {
  return os << p.to_string();
}

struct BoolProp : public Property {
  BoolProp() {}
  BoolProp(bool b) : can_be_true_(b), can_be_false_(!b) {}
  ~BoolProp() override {}

  std::string to_string() const override {
    return can_be_true_ ? (can_be_false_ ? "TF" : "T-")
                        : (can_be_false_ ? "-F" : "--");
  }

  BoolProp *Clone() const override {
    auto *result          = new BoolProp;
    result->can_be_true_  = can_be_true_;
    result->can_be_false_ = can_be_false_;
    return result;
  }

  static BoolProp Bottom() {
    BoolProp val;
    val.can_be_true_  = false;
    val.can_be_false_ = false;
    return val;
  }

  void operator|=(const BoolProp &p) {
    can_be_true_ |= p.can_be_true_;
    can_be_false_ |= p.can_be_false_;
  }

  bool can_be_true_  = true;
  bool can_be_false_ = true;
};

struct IntProp : public Property {
  static std::pair<IR::Register, base::owned_ptr<IntProp>> Make(
      IR::Cmd::LtInt const &lt_int, bool is_true);

  ~IntProp() override {}

  IntProp *Clone() const override {
    auto *result   = new IntProp;
    result->lower_ = lower_;
    result->bound_ = bound_;
    return result;
  }

  std::string to_string() const override {
    return (lower_ ? ">" : "<") + std::to_string(bound_);
  }

  bool lower_;
  int bound_;
};
}  // namespace prop

#endif  // ICARUS_PROPERTY_PROPERTY_H

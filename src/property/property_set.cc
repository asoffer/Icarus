#include "property/property_set.h"

#include "property/property.h"

namespace prop {
namespace {
// When combining the information available between two properties, we overwrite
// the left-hand side property with any new information gleanable from the
// right-hand side. There are several possible outcomes.
enum class Combine {
  Same,     // The properties are equal
  Merge,    // The new property completely subsumes both the left-hand and
            // right-hand sides.
  Partial,  // The new property has more information than the right-hand side,
            // but the right-hand side still has some useful information that
            // isn't expressed in the new property.
  None      // The new property is identical to the left-hand side before
            // combination
};

Combine combine(Property *lhs, Property const *rhs) {
  if (lhs->is<BoolProp>() && rhs->is<BoolProp>()) {
    auto &l   = lhs->as<BoolProp>();
    auto &r   = rhs->as<BoolProp>();
    auto prev = l;
    if (l.can_be_true_ == r.can_be_true_ &&
        l.can_be_false_ == r.can_be_false_) {
      return Combine::Same;
    }

    l.can_be_true_ &= r.can_be_true_;
    l.can_be_false_ &= r.can_be_false_;
    return (l.can_be_true_ == prev.can_be_true_ &&
            l.can_be_false_ == prev.can_be_false_)
               ? Combine::None
               : Combine::Merge;
  }
  // TODO handling all pairs is not scalable (or even possible with user-defined
  // properties.
  return Combine::None;
}
}  // namespace

bool PropertySet::add(base::owned_ptr<Property> prop) {
  if (props_.empty()) {
    props_.insert(std::move(prop));
    return true;
  }

  bool change      = false;
  bool saw_partial = true;
  while (saw_partial) {
    saw_partial = false;
    auto iter   = props_.begin();
    // TODO write down the exact invariants that this guarantees. Something like
    // "every pair of properties is incomparbale."
    while (iter != props_.end()) {
      switch (combine(prop.get(), iter->get())) {
        case Combine::Same: return false;
        case Combine::Merge:
          change = true;
          props_.erase(iter);
          break;
        case Combine::Partial:
          saw_partial = true;
          change      = true;
          ++iter;
          break;
        case Combine::None: ++iter; break;
      }
    }
  }
  props_.insert(std::move(prop));
  return change;
}

bool PropertySet::add(PropertySet const &prop_set) {
  bool change = false;
  for (auto const &p : prop_set.props_) { change |= add(p); }
  return change;
}

void PropertySet::accumulate(Property *prop) const {
  ASSERT(prop != nullptr);
  bool changed = true;
  while (changed) {
    changed = false;
    for (auto const &p : props_) {
      switch (combine(prop, p.get())) {
        case Combine::Merge:
        case Combine::Partial: changed = true;
        default:;
      }
    }
  }
}

}  // namespace prop

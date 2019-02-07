#ifndef ICARUS_PROPERTY_PROPERTY_SET_H
#define ICARUS_PROPERTY_PROPERTY_SET_H

#include <iosfwd>
#include <string>

#include "base/container/bag.h"
#include "base/owned_ptr.h"
#include "base/util.h"

namespace prop {
struct Property;

struct PropertySet {
  // TODO rvalue reference overload too
  bool add(base::owned_ptr<Property> prop);
  bool add(PropertySet const &prop_set);

  void accumulate(Property *prop) const;

  base::bag<base::owned_ptr<Property>> props_;
};

inline std::ostream &operator<<(std::ostream &os, const PropertySet &props) {
  NOT_YET();
}
}  // namespace prop

#endif  // ICARUS_PROPERTY_PROPERTY_SET_H

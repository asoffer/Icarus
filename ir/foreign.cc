#include "ir/foreign.h"

#include "base/container/unordered_map.h"
#include "base/guarded.h"
#include "type/type.h"

namespace ir {
static base::guarded<base::unordered_map<void *, type::Type const *>>
    foreign_objs;

Foreign::Foreign(void *obj, type::Type const *t) : obj_(obj) {
  // TODO what if two calls to foreign claim it's a different type? Should this
  // be allowed? Is it already checked?
  foreign_objs.lock()->emplace(obj, t);
}

type::Type const *Foreign::type() const { return foreign_objs.lock()->at(obj_); }

}  // namespace ir

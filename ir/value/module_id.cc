#include "ir/value/module_id.h"

#include "base/flyweight_map.h"
#include "base/guarded.h"
#include "base/no_destructor.h"

namespace ir {

static base::NoDestructor<
    base::guarded<base::flyweight_map<frontend::CanonicalFileName>>>
    ids;

ModuleId ModuleId::FromFile(frontend::CanonicalFileName const &filename) {
  return ModuleId(ids->lock()->get(filename));
}

frontend::CanonicalFileName const& ModuleId::filename() const {
  return ids->lock()->get(id_);
}

}  // namespace ir

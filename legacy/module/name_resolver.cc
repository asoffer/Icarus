#include "module/name_resolver.h"

#include "nth/debug/debug.h"

namespace module {

UniqueId NameResolver::operator()(ModuleName const& name) const {
  auto iter = map_.find(name);
  NTH_ASSERT(iter != map_.end());
  return iter->second;
}

}  // namespace module

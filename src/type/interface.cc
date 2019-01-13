#include "type/interface.h"

#include <mutex>
#include <utility>

#include "architecture.h"
#include "base/container/map.h"
#include "base/guarded.h"
#include "context.h"
#include "ir/arguments.h"
#include "ir/components.h"
#include "ir/func.h"
#include "module.h"
#include "type/function.h"
#include "type/pointer.h"

namespace type {

void Interface::defining_modules(
    std::unordered_set<::Module const *> *modules) const {
  modules->insert(defining_module());
}

bool Interface::matches(Type const *t) const {
  // TODO implement
  return true;
}

}  // namespace type

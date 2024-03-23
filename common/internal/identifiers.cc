#include "common/internal/identifiers.h"

#include <string>

#include "nth/utility/no_destructor.h"

namespace ic::internal_common {
namespace {

nth::NoDestructor<nth::flyweight_set<std::string>> ids_;

}  // namespace

nth::flyweight_set<std::string> &Identifiers() { return *ids_; }

}  // namespace ic::internal_common

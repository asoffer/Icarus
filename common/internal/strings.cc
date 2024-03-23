#include "common/internal/strings.h"

#include <string>

#include "nth/utility/no_destructor.h"

namespace ic::internal_common {
namespace {

nth::NoDestructor<nth::flyweight_map<std::string, Constant>> strings_;

}  // namespace

nth::flyweight_map<std::string, Constant>& Strings() { return *strings_; }

}  // namespace ic::internal_common

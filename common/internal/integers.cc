#include "common/internal/integers.h"

#include "nth/utility/no_destructor.h"

namespace ic::internal_common {
namespace {

nth::NoDestructor<nth::flyweight_map<nth::integer, Constant>> integers_;

}  // namespace

nth::flyweight_map<nth::integer, Constant>& Integers() { return *integers_; }

}  // namespace ic::internal_common

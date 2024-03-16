#ifndef ICARUS_TYPE_CAST_H
#define ICARUS_TYPE_CAST_H

#include "common/any_value.h"
#include "common/type.h"

namespace ic::type {

bool ImplicitCast(AnyValue const& from, Type to);

}  // namespace ic::type

#endif // ICARUS_TYPE_CAST_H

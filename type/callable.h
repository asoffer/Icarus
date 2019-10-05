#ifndef ICARUS_TYPE_CALLABLE_H
#define ICARUS_TYPE_CALLABLE_H

#include "type/type.h"

namespace type {
struct Callable : public Type {
#include ICARUS_TYPE_VISITOR_METHODS
};
}  // namespace type

#endif  // ICARUS_TYPE_CALLABLE_H

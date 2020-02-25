#ifndef ICARUS_IR_READ_ONLY_DATA_H
#define ICARUS_IR_READ_ONLY_DATA_H

#include "base/untyped_buffer.h"

namespace ir {
// TODO Access to this is thread compatible but we have no external locks to take!
inline base::untyped_buffer ReadOnlyData;
}  // namespace ir

#endif  //  ICARUS_IR_READ_ONLY_DATA_H

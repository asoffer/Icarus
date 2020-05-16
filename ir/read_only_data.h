#ifndef ICARUS_IR_READ_ONLY_DATA_H
#define ICARUS_IR_READ_ONLY_DATA_H

#include "base/global.h"
#include "base/untyped_buffer.h"

namespace ir {

inline base::Global<base::untyped_buffer> ReadOnlyData;

}  // namespace ir

#endif  //  ICARUS_IR_READ_ONLY_DATA_H

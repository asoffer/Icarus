#ifndef ICARUS_FRONTEND_SOURCE_LINE_H
#define ICARUS_FRONTEND_SOURCE_LINE_H

#include "base/strong_types.h"

namespace frontend {

ICARUS_BASE_DEFINE_STRONG_TYPE(LineNum, uint32_t{0},  //
                               base::EnableRawArithmetic,
                               base::EnableComparisons);

}  // namespace frontend

#endif  // ICARUS_FRONTEND_SOURCE_LINE_H

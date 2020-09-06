#include "ir/interpretter/execute.h"

#include <cstring>
#include <utility>

#include "absl/types/span.h"
#include "base/untyped_buffer.h"
#include "ir/value/addr.h"
#include "ir/value/fn.h"
#include "ir/value/reg.h"

namespace interpretter {

void ExecutionContext::MemCpyRegisterBytes(void *dst, ir::Reg reg,
                                           size_t length) {
  std::memcpy(dst, current_frame()->regs_.raw(reg), length);
}

}  // namespace interpretter

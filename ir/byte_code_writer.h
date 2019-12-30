#ifndef ICARUS_IR_BYTE_CODE_WRITER_H
#define ICARUS_IR_BYTE_CODE_WRITER_H

#include "base/untyped_buffer.h"

namespace ir {

struct ByteCodeWriter {
  template <typename T>
  void Write(T val) {
    buf_->append(val);
  }
  base::untyped_buffer* buf_;
};

}  // namespace ir

#endif  // ICARUS_IR_BYTE_CODE_WRITER_H

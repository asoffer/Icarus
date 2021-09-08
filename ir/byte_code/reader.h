#ifndef ICARUS_IR_BYTE_CODE_READER_H
#define ICARUS_IR_BYTE_CODE_READER_H


#include <cstddef>

#include "absl/types/span.h"
#include "base/serialize.h"
#include "ir/byte_code/byte_code.h"

namespace ir {

struct ByteCodeReader {
  explicit ByteCodeReader(ByteCode const* byte_code)
      : ByteCodeReader(ASSERT_NOT_NULL(byte_code)->begin()) {}

  absl::Span<std::byte const> read_bytes(size_t num_bytes) {
    auto span = absl::MakeConstSpan(iter_.raw(), num_bytes);
    iter_.skip(num_bytes);
    return span;
  }

  template <typename T>
  static T DeserializeTo(base::untyped_buffer::const_iterator& iter) {
    ByteCodeReader d(iter);
    T result = base::Deserialize<T>(d);
    iter     = d.iter_;
    return result;
  }

 private:
  explicit ByteCodeReader(base::untyped_buffer::const_iterator iter)
      : iter_(iter) {}

  base::untyped_buffer::const_iterator iter_;
};

}  // namespace ir

#endif  // ICARUS_IR_BYTE_CODE_READER_H

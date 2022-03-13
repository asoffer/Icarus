#ifndef ICARUS_IR_BYTE_CODE_BYTE_CODE_H
#define ICARUS_IR_BYTE_CODE_BYTE_CODE_H

#include <cstddef>
#include <utility>
#include <variant>
#include <vector>

#include "absl/types/span.h"
#include "base/untyped_buffer.h"
#include "core/type_contour.h"
#include "type/type.h"

namespace ir {

struct ByteCode {
  struct Header {
    size_t num_registers;
    size_t num_parameters;
    size_t num_outputs;
    std::vector<std::variant<core::TypeContour, type::Type>> stack_allocations;
  };

  explicit ByteCode(Header h) : header_(std::move(h)) {}

  Header const& header() const { return header_; }
  auto begin() const { return buffer_.begin(); }
  size_t size() const { return buffer_.size(); }

 private:
  friend struct ByteCodeWriter;

  void write_bytes(absl::Span<std::byte const> bytes) {
    buffer_.write(buffer_.size(), bytes.data(), bytes.size());
  }

  size_t append_block_slot() {
    size_t n = buffer_.size();
    buffer_.append_bytes(sizeof(uintptr_t));
    return n;
  }

  void set(size_t write_at_offset, uintptr_t offset_to_write) {
    buffer_.set(write_at_offset, offset_to_write);
  }

  Header header_;
  base::untyped_buffer buffer_;
};

}  // namespace ir

#endif  // ICARUS_IR_BYTE_CODE_BYTE_CODE_H

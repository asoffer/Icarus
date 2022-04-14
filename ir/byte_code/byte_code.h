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

  size_t num_registers() const { return buffer_.get<size_t>(0); }
  size_t num_parameters() const { return buffer_.get<size_t>(sizeof(size_t)); }
  size_t num_outputs() const { return buffer_.get<size_t>(2 * sizeof(size_t)); }
  size_t num_stack_allocations() const {
    return buffer_.get<size_t>(3 * sizeof(size_t));
  }

  core::TypeContour stack_allocation(size_t i) const {
    return buffer_.get<core::TypeContour>(4 * sizeof(size_t) +
                                          i * sizeof(core::TypeContour));
  }

  ByteCode() = default;
  explicit ByteCode(Header const& h) {
    buffer_.append(h.num_registers);
    buffer_.append(h.num_parameters);
    buffer_.append(h.num_outputs);
    buffer_.append(h.stack_allocations.size());
    for (auto const& entry : h.stack_allocations) {
      std::visit(
          [&](auto e) {
            if constexpr (base::meta<decltype(e)> == base::meta<type::Type>) {
              auto tc = core::TypeContour(e.bytes(core::Host),
                                          e.alignment(core::Host));
              buffer_.append(tc);
            } else {
              buffer_.append(e);
            }
          },
          entry);
    }
    initial_size_ = buffer_.size();
  }

  auto begin() const {
    auto iter = buffer_.begin();
    iter.skip(initial_size_);
    return iter;
  }

  base::untyped_buffer_view raw_buffer() const { return buffer_; }

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

  base::untyped_buffer buffer_;
  size_t initial_size_;
};

}  // namespace ir

#endif  // ICARUS_IR_BYTE_CODE_BYTE_CODE_H

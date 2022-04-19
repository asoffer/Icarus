#ifndef ICARUS_IR_BYTE_CODE_BYTE_CODE_VIEW_H
#define ICARUS_IR_BYTE_CODE_BYTE_CODE_VIEW_H

#include <cstddef>
#include <utility>
#include <vector>

#include "absl/types/span.h"
#include "core/type_contour.h"
#include "ir/byte_code/byte_code.h"
#include "type/type.h"

namespace ir {

struct ByteCodeView {
  struct Header {
    size_t num_registers;
    size_t num_parameters;
    size_t num_outputs;
    std::vector<std::variant<core::TypeContour, type::Type>> stack_allocations;
  };

  size_t num_registers() const { return byte_code_.get<size_t>(0); }
  size_t num_parameters() const {
    return byte_code_.get<size_t>(sizeof(size_t));
  }
  size_t num_outputs() const {
    return byte_code_.get<size_t>(2 * sizeof(size_t));
  }
  size_t num_stack_allocations() const {
    return byte_code_.get<size_t>(3 * sizeof(size_t));
  }

  core::TypeContour stack_allocation(size_t i) const {
    return byte_code_.get<core::TypeContour>(4 * sizeof(size_t) +
                                             i * sizeof(core::TypeContour));
  }

  auto begin() const {
    auto iter = byte_code_.begin();
    iter.skip(initial_size_);
    return iter;
  }

  ByteCodeView() = default;
  ByteCodeView(ByteCode const& bc)
      : byte_code_(bc.raw_buffer().data(), bc.raw_buffer().size()),
        initial_size_(std::distance(bc.raw_buffer().data(), bc.begin().raw())) {
  }
  explicit ByteCodeView(std::string_view sv)
      : byte_code_(reinterpret_cast<std::byte const*>(sv.data()), sv.size()) {
    initial_size_ = 4 * sizeof(size_t) +
                    num_stack_allocations() * sizeof(core::TypeContour);
  }
  explicit ByteCodeView(base::untyped_buffer_view buffer) : byte_code_(buffer) {
    initial_size_ = 4 * sizeof(size_t) +
                    num_stack_allocations() * sizeof(core::TypeContour);
  }

 private:
  base::untyped_buffer_view byte_code_;
  size_t initial_size_;
};

}  // namespace ir

#endif  // ICARUS_IR_BYTE_CODE_BYTE_CODE_VIEW_H

#include "ir/interpreter/stack_frame.h"

#include "absl/cleanup/cleanup.h"
#include "ir/interpreter/architecture.h"
#include "type/type.h"

namespace interpreter {
namespace {

core::Bytes GetBytes(core::Arch const& arch, type::Type t) {
  return t.bytes(arch);
}

core::Bytes GetBytes(core::Arch const& arch, core::TypeContour tc) {
  return tc.bytes();
}

core::Alignment GetAlignment(core::Arch const& arch, type::Type t) {
  return t.alignment(arch);
}

core::Alignment GetAlignment(core::Arch const& arch, core::TypeContour tc) {
  return tc.alignment();
}

}  // namespace

StackFrame::~StackFrame() { stack_.Deallocate(frame_size_); }

StackFrame::StackFrame(Summary const& s, Stack& stack)
    : stack_(stack),
      frame_size_(0),
      byte_code_(nullptr),
      summary_(s),
      data_(base::untyped_buffer::MakeFull(register_count() *
                                           register_value_size)) {}

StackFrame::StackFrame(ir::ByteCode const* bc, Stack& stack)
    : stack_(stack),
      byte_code_(ASSERT_NOT_NULL(bc)),
      summary_({
          .num_parameters = byte_code_->header().num_parameters,
          .num_registers  = byte_code_->header().num_registers,
          .num_outputs    = byte_code_->header().num_outputs,
          .num_stack_allocations =
              byte_code_->header().stack_allocations.size(),
      }),
      data_(base::untyped_buffer::MakeFull(register_count() *
                                           register_value_size)) {
  core::Bytes next_reg_loc = core::Bytes(0);

  // Offsets from the front of this stack frame for each stack allocation.
  std::vector<size_t> stack_offsets;
  stack_offsets.reserve(byte_code_->header().stack_allocations.size());

  for (auto const& t : byte_code_->header().stack_allocations) {
    std::visit(
        [&](auto const& entry) {
          next_reg_loc =
              core::FwdAlign(next_reg_loc, GetAlignment(kArchitecture, entry));
              stack_offsets.push_back(next_reg_loc.value());
          next_reg_loc += GetBytes(kArchitecture, entry);
        },
        t);
  }

  frame_size_            = next_reg_loc.value();
  ir::addr_t frame_start = stack_.Allocate(frame_size_);
  for (size_t i = 0; i < stack_offsets.size(); ++i) {
    set(ir::Reg::StackAllocation(i), frame_start + stack_offsets[i]);
  }
}

// Tuning parameters for stack allocation in the interpreter.
static constexpr size_t kMinStackSegmentSizeInBytes = 4096;

Stack::Stack() {
  auto& segment  = segments_.emplace_back();
  segment.buffer = base::untyped_buffer::MakeFull(kMinStackSegmentSizeInBytes);
  segment.capacity = segment.buffer.size();
  end_             = segment.buffer.raw(0);
}

std::byte* Stack::Allocate(size_t bytes) {
  if (segments_.back().capacity >= bytes) {
    segments_.back().capacity -= bytes;
  } else {
    auto& segment  = segments_.emplace_back();
    segment.buffer = base::untyped_buffer::MakeFull(
        std::max(kMinStackSegmentSizeInBytes, 2 * bytes));
    segment.capacity = segment.buffer.size() - bytes;
    end_             = segment.buffer.raw(0);
  }

  std::byte* result = end_;
  end_              = result + bytes;

  return result;
}

void Stack::Deallocate(size_t bytes) {
  ptrdiff_t distance = end_ - segments_.back().buffer.raw(0);
  ASSERT(distance >= bytes);
  if (distance > bytes) {
    segments_.back().capacity += bytes;
    end_ -= bytes;
  } else if (bytes != 0) {
    if (segments_.size() != 1) { segments_.pop_back(); }
    auto& segment = segments_.back();
    end_ = segment.buffer.raw(0) + (segment.buffer.size() - segment.capacity);
  }
}

}  // namespace interpreter

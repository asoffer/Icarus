#include "ir/interpreter/stack_frame.h"

#include "ir/interpreter/architecture.h"
#include "type/function.h"
#include "type/type.h"

namespace interpreter {

namespace {

size_t NumRegisters(ir::Fn fn) {
  switch (fn.kind()) {
    case ir::Fn::Kind::Native: return fn.native()->num_regs();
    default: return 0;
  }
}

size_t NumOutputs(ir::Fn fn) { return fn.type()->return_types().size(); }

}  // namespace

StackFrame::~StackFrame() { stack_.Deallocate(frame_size_); }

StackFrame::StackFrame(ir::Scope s, Stack& stack)
    : stack_(stack),
      frame_size_(0),
      sizes_({.num_registers  = s->num_regs(),
              .num_parameters = s->params().size(),
              // TODO: 1 for the vector of blocks, but there might also be real
              // returns.
              .num_outputs           = 1,
              .num_stack_allocations = s->num_allocs()}),
      data_(base::untyped_buffer::MakeFull(
          (sizes_.num_registers + sizes_.num_parameters + sizes_.num_outputs +
           sizes_.num_stack_allocations) *
          register_value_size)) {
  absl::flat_hash_map<ir::Reg, size_t> offsets;

  byte_code_iter_          = s.byte_code_iterator();
  core::Bytes next_reg_loc = core::Bytes(0);
  s->for_each_alloc(kArchitecture, [&](core::TypeContour tc, ir::Reg r) {
    next_reg_loc = core::FwdAlign(next_reg_loc, tc.alignment());
    offsets.emplace(r, next_reg_loc.value());
    next_reg_loc += tc.bytes();
  });

  frame_size_            = next_reg_loc.value();
  ir::addr_t frame_start = stack_.Allocate(frame_size_);

  for (auto [reg, offset] : offsets) { set(reg, frame_start + offset); }
}

StackFrame::StackFrame(ir::Fn fn, Stack& stack)
    : stack_(stack),
      frame_size_(0),
      sizes_({.num_registers         = NumRegisters(fn),
              .num_parameters        = fn.num_parameters(),
              .num_outputs           = NumOutputs(fn),
              .num_stack_allocations = fn.kind() == ir::Fn::Kind::Native
                                           ? fn.native()->num_allocs()
                                           : 0}),
      data_(base::untyped_buffer::MakeFull(
          (sizes_.num_registers + sizes_.num_parameters + sizes_.num_outputs +
           sizes_.num_stack_allocations) *
          register_value_size)) {
  absl::flat_hash_map<ir::Reg, size_t> offsets;

  if (fn.kind() == ir::Fn::Kind::Native) {
    byte_code_iter_          = fn.native().byte_code_iterator();
    core::Bytes next_reg_loc = core::Bytes(0);
    fn.native()->for_each_alloc(
        kArchitecture, [&](core::TypeContour tc, ir::Reg r) {
          next_reg_loc = core::FwdAlign(next_reg_loc, tc.alignment());
          offsets.emplace(r, next_reg_loc.value());
          next_reg_loc += tc.bytes();
        });

    frame_size_            = next_reg_loc.value();
    ir::addr_t frame_start = stack_.Allocate(frame_size_);

    for (auto [reg, offset] : offsets) { set(reg, frame_start + offset); }
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

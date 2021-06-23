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

StackFrame::StackFrame(ir::Fn fn, Stack& stack)
    : fn_(fn),
      stack_(stack),
      frame_size_(0),
      regs_({.num_registers  = NumRegisters(fn_),
             .num_parameters = fn_.num_parameters(),
             .num_outputs    = NumOutputs(fn_)}) {
  absl::flat_hash_map<ir::Reg, size_t> offsets;

  if (fn_.kind() == ir::Fn::Kind::Native) {
    core::Bytes next_reg_loc = core::Bytes(0);
    fn_.native()->for_each_alloc(
        kArchitecture, [&](core::TypeContour tc, ir::Reg r) {
          next_reg_loc = core::FwdAlign(next_reg_loc, tc.alignment());
          offsets.emplace(r, next_reg_loc.value());
          next_reg_loc += tc.bytes();
        });

    frame_size_        = next_reg_loc.value();
    ir::addr_t frame_start = stack_.Allocate(frame_size_);

    for (auto [reg, offset] : offsets) { regs_.set(reg, frame_start + offset); }
  }
}

Stack::Stack() {
  auto& segment  = segments_.emplace_back();
  segment.buffer = base::untyped_buffer::MakeFull(kMinStackSegmentSizeInBytes);
  segment.capacity = segment.buffer.size();
  end_             = segment.buffer.raw(0);
}

char* Stack::Allocate(size_t bytes) {
  if (segments_.back().capacity >= bytes) {
    segments_.back().capacity -= bytes;
  } else {
    auto& segment  = segments_.emplace_back();
    segment.buffer = base::untyped_buffer::MakeFull(
        std::max(kMinStackSegmentSizeInBytes, 2 * bytes));
    segment.capacity = segment.buffer.size() - bytes;
    end_             = segment.buffer.raw(0);
  }

  char* result = end_;
  end_         = result + bytes;

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

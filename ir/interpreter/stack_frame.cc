#include "ir/interpreter/stack_frame.h"

namespace ir::interpreter {
namespace {

std::byte* IncrementedBy(std::byte* p, size_t register_count) {
  return p + StackFrame::register_size * register_count;
}

std::array<std::byte*, 4> MakeStarts(StackFrame::Summary const& summary,
                                     std::byte* start) {
  std::byte* value     = start;
  std::byte* output    = IncrementedBy(value, summary.num_registers);
  std::byte* parameter = IncrementedBy(output, summary.num_outputs);
  std::byte* alloc     = IncrementedBy(parameter, summary.num_parameters);
  return std::array<std::byte*, 4>{value, output, parameter, alloc};
}

}  // namespace

StackFrame::StackFrame(Summary const& summary, std::string& fatal_error)
    : frame_(
          base::untyped_buffer::MakeFull(summary.required_stack_space.value())),
      registers_(base::untyped_buffer::MakeFull(summary.num_registers *
                                                register_size)),
      starts_(MakeStarts(summary, registers_.raw(0))),
      fatal_error_(fatal_error) {}

std::byte* StackFrame::find(Reg r) {
  auto kind = static_cast<std::underlying_type_t<Reg::Kind>>(r.kind());
  return starts_[kind] + register_size * r.raw_value();
}

std::byte const* StackFrame::find(Reg r) const {
  auto kind = static_cast<std::underlying_type_t<Reg::Kind>>(r.kind());
  return starts_[kind] + register_size * r.raw_value();
}

void StackFrame::set_raw(ir::Reg r, void const* src, uint16_t num_bytes) {
  ASSERT(num_bytes <= register_size);
  std::memcpy(find(r), src, num_bytes);
}

}  // namespace ir::interpreter

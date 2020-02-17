#ifndef ICARUS_IR_OUT_PARAMS_H
#define ICARUS_IR_OUT_PARAMS_H

#include <vector>

#include "absl/types/span.h"
#include "ir/byte_code_writer.h"
#include "ir/value/reg.h"

namespace ir {
// `OutParams` represent a collection of output parameters from a function or
// jump. Outputs for register-sized values are returned in registers. For all
// others, the caller allocates stack space and passes the address as the output
// parameter.
struct OutParams {
  explicit OutParams() = default;
  explicit OutParams(std::vector<Reg> regs) : regs_(std::move(regs)) {}

  absl::Span<Reg> regs() { return absl::MakeSpan(regs_); }
  absl::Span<Reg const> regs() const { return regs_; }

  Reg operator[](size_t n) const { return regs_[n]; }
  size_t size() const { return regs_.size(); }
  bool empty() const { return regs_.empty(); }

  void WriteByteCode(ByteCodeWriter *writer) const {
    writer->Write<uint16_t>(regs_.size());
    for (Reg r : regs_) { writer->Write(r); }
  }

 private:
  std::vector<Reg> regs_;
};
}  // namespace ir

#endif  // ICARUS_IR_OUT_PARAMS_H

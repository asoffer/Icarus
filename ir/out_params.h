#ifndef ICARUS_IR_OUT_PARAMS_H
#define ICARUS_IR_OUT_PARAMS_H

#include <vector>

#include "absl/types/span.h"
#include "base/extend.h"
#include "base/extend/serialize.h"
#include "ir/value/reg.h"

namespace ir {
// `OutParams` represent a collection of output parameters from a function or
// jump. Outputs for register-sized values are returned in registers. For all
// others, the caller allocates stack space and passes the address as the output
// parameter.
struct OutParams
    : base::Extend<OutParams, 1>::With<base::BaseSerializeExtension> {
  explicit OutParams() = default;
  explicit OutParams(std::vector<Reg> regs) : regs_(std::move(regs)) {}

  absl::Span<Reg> regs() { return absl::MakeSpan(regs_); }
  absl::Span<Reg const> regs() const { return regs_; }

  Reg operator[](size_t n) const { return regs_[n]; }
  size_t size() const { return regs_.size(); }
  bool empty() const { return regs_.empty(); }

  auto begin() const { return regs_.begin(); }
  auto begin() { return regs_.begin(); }
  auto end() const { return regs_.end(); }
  auto end() { return regs_.end(); }

 private:
  friend base::EnableExtensions;

  std::vector<Reg> regs_;
};
}  // namespace ir

#endif  // ICARUS_IR_OUT_PARAMS_H

#ifndef ICARUS_IR_STACK_FRAME_ALLOCATIONS_H
#define ICARUS_IR_STACK_FRAME_ALLOCATIONS_H

#include <vector>

#include "absl/container/flat_hash_map.h"
#include "type/type.h"

namespace ir {

// Represents all of the stack allocations made within a given function.
// TODO: Currently when deciding how much stack space is necessary, we ensure
// that each allocated item does not overlap with any others. Ideally we could
// also track lifetimes and reuse space effectively (and have a sanitizer mode
// that poisons rather than reuses space).
struct StackFrameAllocations {
  void allocate(type::Type const* t, Reg r) { allocs_.emplace_back(t, r); }

  template <typename Fn>
  void for_each(Fn&& f) const {
    for (auto const& [t, reg] : allocs_) { f(t, reg); }
  }

  friend std::ostream& operator<<(std::ostream& os,
                                  StackFrameAllocations const& s) {
    for (auto const& [t, reg] : s.allocs_) {
      os << "  " << stringify(reg) << ": " << t->to_string() << "\n";
    }
    return os;
  }

 private:
  std::vector<std::pair<type::Type const *, Reg>> allocs_;
  // TODO In the short-term, use stack-space according to when the things were
  // allocated, just dropping them on the stack as needed. In the future, think
  // about how they may overlap. What their lifetime is, etc.
};

}  // namespace ir

#endif  // ICARUS_IR_STACK_FRAME_ALLOCATIONS_H

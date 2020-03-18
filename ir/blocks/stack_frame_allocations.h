#ifndef ICARUS_IR_BLOCKS_STACK_FRAME_ALLOCATIONS_H
#define ICARUS_IR_BLOCKS_STACK_FRAME_ALLOCATIONS_H

#include <utility>
#include <vector>

#include "type/type.h"

namespace ir {

// StackFrameAllocations:
//
// Represents all of the stack allocations made within a given block group.
// TODO: Currently when deciding how much stack space is necessary, we ensure
// that each allocated item does not overlap with any others. Ideally we could
// also track lifetimes and reuse space effectively (and have a sanitizer mode
// that poisons rather than reuses space).
struct StackFrameAllocations {
  // Adds a new stack allocation of type `t` assigned to register `r`.
  //
  // TODO This should be in charge of handling the register assignment rather
  // than accepting it.
  void allocate(type::Type const* t, Reg r) { allocs_.emplace_back(t, r); }

  // Iterate through each allocation, calling `f` on each (type, register)-pair.
  // Note that we intentionally do not use `type::Typed<ir::Reg>`, because the
  // register represents the address of a stored value of the given type, so it
  // would be misleading. The register has a value which is a pointer to this
  // type rather than the type itself.
  template <typename Fn>
  void for_each(Fn&& f) const {
    for (auto const & [ t, reg ] : allocs_) { f(t, reg); }
  }

  friend std::ostream& operator<<(std::ostream& os,
                                  StackFrameAllocations const& s) {
    for (auto const & [ t, reg ] : s.allocs_) {
      os << "  " << stringify(reg) << ": " << t->to_string() << "\n";
    }
    return os;
  }

 private:
  std::vector<std::pair<type::Type const*, Reg>> allocs_;
};

}  // namespace ir

#endif  // ICARUS_IR_BLOCKS_STACK_FRAME_ALLOCATIONS_H

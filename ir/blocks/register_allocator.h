#ifndef ICARUS_IR_BLOCKS_REGISTER_ALLOCATOR_H
#define ICARUS_IR_BLOCKS_REGISTER_ALLOCATOR_H

#include <iostream>
#include <utility>
#include <vector>

#include "ir/value/reg.h"
#include "type/type.h"

namespace ir {

// RegisterAllocator:
//
// This name is a bit misleading, because this struct does not handle
// machine/architecture-specific register allocation. Rather, it is responsible
// for handing out registers in the intermediate representation of which there
// are an unbounded number. It is also responsible for managing stack
// allocations (again, in the intermediate representation), and assigning
// registers to each stack allocation.
//
// TODO It would be nice if this class also tracked lifetimes of stack
// allocations (maybe by also accepting scopes to which the allocations are
// tied) so that the interpretter could reuse stack space for allocations with
// non-overlapping lifetimes.
struct RegisterAllocator {
  // Constructs a register allocatior to be used with a group that has
  // `num_input_regs` already allocated to inputs (input parameters or state).
  explicit RegisterAllocator(size_t num_input_regs)
      : num_regs_(num_input_regs), num_args_(num_input_regs) {}

  // Returns a `Reg` with the next available register number.
  Reg Reserve() { return Reg(num_regs_++); }

  // Adds a new stack allocation of type `t` and returns the `Reg` representing
  // the register to which it was assigned.
  Reg StackAllocate(type::Type t) {
    auto r = Reserve();
    allocs_.emplace_back(t, r);
    return r;
  }

  // Returns the number of registers requested through this `RegisterAllocator`.
  constexpr size_t num_regs() const { return num_regs_; }

  // Returns the number of registers corresponding to inputs (either parameters
  // or implicit state).
  constexpr size_t num_args() const { return num_args_; }

  // Merge allocations from another RegisterAllocator. This method is used when
  // inlining another group into this one. The callable `f` is applied to each
  // allocated register in the to-be-inlined group to update it so that it's
  // value does not conflict with that a register value in `*this`. Typically
  // this is just incrementing it by a particular offset.
  template <typename Fn>
  void MergeFrom(RegisterAllocator const& a, Fn&& f) {
    num_regs_ += a.num_regs_;
    for (auto const& [t, reg] : a.allocs_) { allocs_.emplace_back(t, f(reg)); }
  }

  // Iterate through each allocation, calling `f` on each (type, register)-pair.
  // Note that we intentionally do not use `type::Typed<ir::Reg>`, because the
  // register represents the address of a stored value of the given type, so it
  // would be misleading. The register has a value which is a pointer to this
  // type rather than the type itself.
  template <typename Fn>
  void for_each_alloc(Fn&& f) const {
    for (auto const& [t, reg] : allocs_) { f(t, reg); }
  }

  friend std::ostream& operator<<(std::ostream& os,
                                  RegisterAllocator const& a) {
    for (auto const& [t, reg] : a.allocs_) {
      os << "  " << stringify(reg) << ": " << t << "\n";
    }
    return os;
  }

 private:
  size_t num_regs_;
  size_t num_args_;
  std::vector<std::pair<type::Type, Reg>> allocs_;
};

}  // namespace ir

#endif  // ICARUS_IR_BLOCKS_REGISTER_ALLOCATOR_H

#ifndef ICARUS_IR_BLOCKS_REGISTER_ALLOCATOR_H
#define ICARUS_IR_BLOCKS_REGISTER_ALLOCATOR_H

#include <concepts>
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
// tied) so that the interpreter could reuse stack space for allocations with
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

  // Adds a new stack allocation for a type with the given contour (size and
  // alignment) and returns the `Reg` representing the register to which it was
  // assigned.
  Reg StackAllocate(core::TypeContour tc) {
    auto r = Reserve();
    raw_allocs_.emplace_back(tc, r);
    return r;
  }

  // Returns the number of registers requested through this `RegisterAllocator`.
  constexpr size_t num_regs() const { return num_regs_; }

  // Returns the number of registers corresponding to inputs (either parameters
  // or implicit state).
  constexpr size_t num_args() const { return num_args_; }

  // Returns the number of stack allocations.
  size_t num_allocs() const { return allocs_.size() + raw_allocs_.size(); }

  // Merge allocations from another RegisterAllocator. This method is used when
  // inlining another group into this one. The callable `f` is applied to each
  // allocated register from `a`, so that its value does not conflict with a
  // register in `*this`. It is up to the caller to ensure register values do
  // not consflict.
  void MergeFrom(RegisterAllocator const& a, std::invocable<ir::Reg&> auto&& f) {
    num_regs_ += a.num_regs_;
    for (auto const& [t, reg] : a.allocs_) {
      Reg r = reg;
      f(r);
      allocs_.emplace_back(t, r);
    }
    for (auto const& [tc, reg] : a.raw_allocs_) {
      Reg r = reg;
      f(r);
      raw_allocs_.emplace_back(tc, r);
    }
  }

  // Iterate through each allocation, calling `f` on each allocated register
  void for_each_alloc(std::invocable<type::Type, ir::Reg> auto&& f) const {
    for (auto const& [t, reg] : allocs_) { f(t, reg); }
    // TODO: raw allocs
  }

  void for_each_alloc(
      core::Arch a, std::invocable<core::TypeContour, ir::Reg> auto&& f) const {
    for (auto const& [t, reg] : allocs_) {
      f(core::TypeContour(t.bytes(a), t.alignment(a)), reg);
    }
    for (auto const& [tc, reg] : raw_allocs_) { f(tc, reg); }
  }

  friend std::ostream& operator<<(std::ostream& os,
                                  RegisterAllocator const& a) {
    for (auto const& [t, reg] : a.allocs_) {
      os << "  " << reg << ": " << t << "\n";
    }

    for (auto const& [tc, reg] : a.raw_allocs_) {
      os << "  " << reg << ": " << tc.bytes() << ", " << tc.alignment() << "\n";
    }
    return os;
  }

 private:
  size_t num_regs_;
  size_t num_args_;
  std::vector<std::pair<type::Type, Reg>> allocs_;
  std::vector<std::pair<core::TypeContour, Reg>> raw_allocs_;
};

}  // namespace ir

#endif  // ICARUS_IR_BLOCKS_REGISTER_ALLOCATOR_H

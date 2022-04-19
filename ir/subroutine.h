#ifndef ICARUS_IR_SUBROUTINE_H
#define ICARUS_IR_SUBROUTINE_H

#include <concepts>
#include <iostream>
#include <memory>
#include <vector>

#include "base/ptr_span.h"
#include "base/strong_types.h"
#include "core/alignment.h"
#include "core/bytes.h"
#include "core/parameters.h"
#include "core/type_contour.h"
#include "ir/basic_block.h"
#include "ir/blocks/register_allocator.h"
#include "ir/value/reg.h"
#include "type/callable.h"

namespace ir {

// Subroutine:
//
// Represents a collection of `BasicBlock`s which make sense together as a
// coherent entity. One might normally call such a collection of `BasicBlock`s a
// function, but Icarus has at least one other useful example: A `Jump`. The IR
// for a Jump is largely similar to that of a function with a few differences. A
// `Subroutine` represents the parts common to both.
struct Subroutine {
  Subroutine() : alloc_(0) {}
  Subroutine(Subroutine const &) = delete;
  Subroutine(Subroutine &&)      = default;
  Subroutine &operator=(Subroutine const &) = delete;
  Subroutine &operator=(Subroutine &&) = default;

  explicit Subroutine(type::Callable const *type);

  base::PtrSpan<BasicBlock const> blocks() const { return blocks_; }
  base::PtrSpan<BasicBlock> blocks() { return blocks_; }
  auto &mutable_blocks() { return blocks_; }

  BasicBlock const *entry() const { return blocks()[0]; }
  BasicBlock *entry() { return blocks()[0]; }

  template <typename... Args>
  BasicBlock *AppendBlock(Args &&... args) {
    return blocks_
        .emplace_back(std::make_unique<BasicBlock>(std::forward<Args>(args)...))
        .get();
  }

  core::Parameters<type::QualType> const &parameters() const { return type_->parameters(); }
  type::Callable const *type() const { return type_; }

  template <std::invocable<type::Type, ir::Reg> Fn>
  void for_each_alloc(Fn &&f) const {
    alloc_.for_each_alloc(std::forward<Fn>(f));
  }

  template <std::invocable<core::TypeContour, ir::Reg> Fn>
  void for_each_alloc(core::Arch a, Fn &&f) const {
    alloc_.for_each_alloc(a, std::forward<Fn>(f));
  }

  Reg Reserve() { return alloc_.Reserve(); }
  Reg Alloca(type::Type t);
  Reg Alloca(core::TypeContour tc);

  template <std::invocable<ir::Reg &> F>
  void MergeAllocationsFrom(Subroutine const &from, F &&f) {
    alloc_.MergeFrom(from.alloc_, std::forward<F>(f));
  }

  constexpr size_t num_regs() const { return alloc_.num_regs(); }
  constexpr size_t num_args() const { return alloc_.num_args(); }
  size_t num_allocs() const { return alloc_.num_allocs(); }

  friend std::ostream &operator<<(std::ostream &os, Subroutine const &b);

 private:
  type::Callable const *type_;
  std::vector<std::unique_ptr<BasicBlock>> blocks_;
  RegisterAllocator alloc_;
};

}  // namespace ir

#endif  // ICARUS_IR_SUBROUTINE_H

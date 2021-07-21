#ifndef ICARUS_IR_BLOCKS_GROUP_H
#define ICARUS_IR_BLOCKS_GROUP_H

#include <concepts>
#include <iostream>
#include <memory>
#include <vector>

#include "ast/ast_fwd.h"
#include "base/ptr_span.h"
#include "base/strong_types.h"
#include "core/alignment.h"
#include "core/bytes.h"
#include "core/params.h"
#include "ir/blocks/basic.h"
#include "ir/blocks/register_allocator.h"
#include "ir/value/reg.h"
#include "type/type_fwd.h"
#include "type/typed_value.h"

namespace ir {
namespace internal {

// BlockGroupBase:
//
// Represents a collection of `BasicBlock`s which make sense together as a
// coherent entity. One might normally call such a collection of `BasicBlock`s a
// function, but Icarus has at least one other useful example: A `Jump`. The IR
// for a Jump is largely similar to that of a function with a few differences. A
// `BlockGroupBase` represents the parts common to both.
struct BlockGroupBase {
  // TODO We should not need to store anything to do with the AST here.
  // TODO blocks need to know how to handle initial values for their parameters.
  BlockGroupBase(BlockGroupBase const &) = delete;
  BlockGroupBase(BlockGroupBase &&)      = default;
  BlockGroupBase &operator=(BlockGroupBase const &) = delete;
  BlockGroupBase &operator=(BlockGroupBase &&) = default;

  explicit BlockGroupBase(
      core::Params<type::Typed<ast::Declaration const *>> params,
      size_t num_state_args = 0);

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

  core::Params<type::Typed<ast::Declaration const *>> const &params() const {
    return params_;
  }

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

  void MergeAllocationsFrom(BlockGroupBase const &from,
                            std::invocable<ir::Reg &> auto &&f) {
    alloc_.MergeFrom(from.alloc_, std::forward<decltype(f)>(f));
  }

  constexpr size_t num_regs() const { return alloc_.num_regs(); }
  constexpr size_t num_args() const { return alloc_.num_args(); }
  size_t num_allocs() const { return alloc_.num_allocs(); }

  friend std::ostream &operator<<(std::ostream &os, BlockGroupBase const &b);

 private:
  core::Params<type::Typed<ast::Declaration const *>> params_;
  std::vector<std::unique_ptr<BasicBlock>> blocks_;
  RegisterAllocator alloc_;
};

}  // namespace internal

template <typename T>
struct BlockGroup : internal::BlockGroupBase {
  BlockGroup(T const *t,
             core::Params<type::Typed<ast::Declaration const *>> params,
             size_t num_state_args = 0)
      : internal::BlockGroupBase(std::move(params), num_state_args), type_(t) {}

  T const *type() const { return type_; }

  friend std::ostream &operator<<(std::ostream &os, BlockGroup const &b) {
    return os << "\n"
              << &b << ": " << b.type()->to_string()
              << static_cast<internal::BlockGroupBase const &>(b);
  }

 private:
  T const *type_;
};

}  // namespace ir

#endif  // ICARUS_IR_BLOCKS_GROUP_H

#ifndef ICARUS_IR_BLOCKS_GROUP_H
#define ICARUS_IR_BLOCKS_GROUP_H

#include <iostream>
#include <memory>
#include <vector>

#include "ast/ast_fwd.h"
#include "base/move_func.h"
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

  base::untyped_buffer const &byte_code() const {
    ASSERT(byte_code_.size() != 0u);
    return byte_code_;
  }

  template <typename InstructionSet>
  void WriteByteCode() {
    ByteCodeWriter writer(&byte_code_);
    ASSERT(byte_code_.size() == 0u);
    for (auto &block : blocks_) {
      block->WriteByteCode<InstructionSet>(&writer);
    }
    writer.MakeReplacements();
  }

  template <typename Fn>
  void for_each_alloc(Fn &&f) {
    alloc_.for_each_alloc(std::forward<Fn>(f));
  }

  Reg Reserve() { return alloc_.Reserve(); }
  Reg Alloca(type::Type t);

  constexpr size_t num_regs() const { return alloc_.num_regs(); }
  constexpr size_t num_args() const { return alloc_.num_args(); }

  // TODO: Remove mutability. Track work items non-intrusively.
  mutable base::move_func<void()> *work_item = nullptr;

  friend std::ostream &operator<<(std::ostream &os, BlockGroupBase const &b);

 private:
  friend struct ir::InstructionInliner;

  core::Params<type::Typed<ast::Declaration const *>> params_;
  std::vector<std::unique_ptr<BasicBlock>> blocks_;
  RegisterAllocator alloc_;

  base::untyped_buffer byte_code_;
};

}  // namespace internal

template <typename T>
struct BlockGroup : internal::BlockGroupBase {
  BlockGroup(T const *t,
             core::Params<type::Typed<ast::Declaration const *>> params,
             size_t num_state_args = 0)
      : internal::BlockGroupBase(std::move(params)), type_(t) {}

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

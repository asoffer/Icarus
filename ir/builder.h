#ifndef ICARUS_IR_BUILDER_H
#define ICARUS_IR_BUILDER_H

#include "base/debug.h"
#include "base/scope.h"
#include "base/tag.h"
#include "ir/addr.h"
#include "ir/basic_block.h"
#include "ir/reg.h"
#include "type/typed_value.h"

namespace ir {
namespace internal {
struct BlockGroup;
}  // namespace internal

struct Builder {
  BasicBlock* AddBlock();

  internal::BlockGroup*& CurrentGroup() { return current_.group_; }
  BasicBlock*& CurrentBlock() { return current_.block_; }

  base::Tagged<Addr, Reg> Alloca(type::Type const* t);
  base::Tagged<Addr, Reg> TmpAlloca(type::Type const* t);

  // Apply the callable to each temporary in reverse order, and clear the list
  // of temporaries.
  template <typename Fn>
  void FinishTemporariesWith(Fn&& fn) {
    for (auto iter = current_.temporaries_to_destroy_.rbegin();
         iter != current_.temporaries_to_destroy_.rend(); ++iter) {
      fn(*iter);
    }
    current_.temporaries_to_destroy_.clear();
  }

  constexpr bool more_stmts_allowed() const {
    return current_.more_stmts_allowed_;
  }
  constexpr void allow_more_stmts() { current_.more_stmts_allowed_ = true; }
  constexpr void disallow_more_stmts() { current_.more_stmts_allowed_ = false; }

  ICARUS_PRIVATE
  friend struct SetCurrentFunc;
  friend struct SetTemporaries;

  struct State {
    internal::BlockGroup* group_ = nullptr;
    BasicBlock* block_;

    // Temporaries need to be destroyed at the end of each statement.
    // This is a pointer to a buffer where temporary allocations can register
    // themselves for deletion.
    std::vector<type::Typed<Reg>> temporaries_to_destroy_;
    bool more_stmts_allowed_ = true;
  } current_;
};

Builder& GetBuilder();

struct SetCurrentFunc : public base::UseWithScope {
  SetCurrentFunc(internal::BlockGroup* fn);
  ~SetCurrentFunc();

 private:
  internal::BlockGroup* old_group_;
  BasicBlock* old_block_;
};

struct SetTemporaries : public base::UseWithScope {
  SetTemporaries(Builder& bldr) : bldr_(bldr) {
    old_temporaries_ = std::exchange(bldr_.current_.temporaries_to_destroy_,
                                     std::vector<type::Typed<Reg>>{});
    old_more_stmts_allowed_ =
        std::exchange(bldr_.current_.more_stmts_allowed_, true);
  }
  ~SetTemporaries() {}

 private:
  std::vector<type::Typed<Reg>> old_temporaries_;
  bool old_more_stmts_allowed_;
  Builder& bldr_;
};

}  // namespace ir

#endif  // ICARUS_IR_BUILDER_H

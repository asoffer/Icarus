#ifndef ICARUS_IR_BLOCK_H
#define ICARUS_IR_BLOCK_H

#include <iostream>

#include "base/debug.h"

namespace ast {
struct BlockLiteral;
}  // namespace ast

namespace ir {
struct Block {
 public:
  constexpr Block() = default;
  constexpr static Block Start() { return Block(nullptr); }
  constexpr static Block Exit() { return Block(0x1); }

  constexpr Block(ast::BlockLiteral const *lit)
      : data_(reinterpret_cast<uintptr_t>(lit)) {}

  friend std::ostream &operator<<(std::ostream &os, Block b) {
    return os << "block(" << b.data_ << ")";
  }
  friend constexpr bool operator==(Block lhs, Block rhs) {
    return lhs.data_ == rhs.data_;
  }

  template <typename H>
  friend H AbslHashValue(H h, Block b) {
    return H::combine(std::move(h), b.data_);
  }

  // operator->?
  ast::BlockLiteral const *get() const {
    ASSERT(*this != Start());
    ASSERT(*this != Exit());
    return reinterpret_cast<ast::BlockLiteral const *>(data_);
  }

 private:
  constexpr Block(uintptr_t val) : data_(val){};
  uintptr_t data_ = 0;
};

struct BlockSequence {
 public:
  // TODO construct with some current function context that we can attach this
  // to so it doesn't leak. Or get your story straight about what is going to
  // stay around until for how long so you can prove this doesn't leak (or stays
  // around forever so you don't care).
  BlockSequence() : seq_(new std::vector<Block>) {}

  void append(Block b) { seq_->push_back(b); }

  template <typename H>
  friend H AbslHashValue(H h, BlockSequence b) {
    return H::combine(std::move(h), *b.seq_);
  }

  friend bool operator==(BlockSequence const &lhs, BlockSequence const &rhs) {
    return *lhs.seq_ == *rhs.seq_;
  }

  friend std::ostream &operator<<(std::ostream &os, BlockSequence b) {
    if (b.seq_->empty()) { return os << "block-seq()"; }
    os << "block-seq(" << b.seq_->at(0);
    for (size_t i = 1; i < b.seq_->size(); ++i) { os << ", " << b.seq_->at(i); }
    return os << ")";
  }

  BlockSequence &operator|=(BlockSequence rhs) {
    seq_->insert(seq_->end(), rhs.seq_->begin(), rhs.seq_->end());
    return *this;
  }

  size_t size() const { return seq_->size(); }
  Block at(size_t i) const { return seq_->at(i); }

  // private:
  std::vector<Block> *seq_ = nullptr;
};

constexpr bool operator!=(Block lhs, Block rhs) { return !(lhs == rhs); }
inline bool operator!=(BlockSequence lhs, BlockSequence rhs) {
  return !(lhs == rhs);
}

}  // namespace ir

#endif  // ICARUS_IR_BLOCK_H

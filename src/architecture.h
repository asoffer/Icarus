#ifndef ICARUS_ARCHITECTURE_H
#define ICARUS_ARCHITECTURE_H

#include <cstddef>

#include "base/types.h"
#include "ir/val.h"

namespace type {
struct Type;
}  // namespace type

namespace ir {
struct Val;
}  // namespace ir

// We only support architectures on which a byte is 8 bits, and assume all
// alignments are powers of two.
struct Architecture {
  size_t alignment(const type::Type *t) const;
  size_t bytes(const type::Type *t) const;

  size_t MoveForwardToAlignment(const type::Type *t, size_t index) const {
    return ((index - 1) | (alignment(t) - 1)) + 1;
  }

  // TODO skip the last alignment requirement?
  ir::RegisterOr<i32> ComputeArrayLength(ir::RegisterOr<i32> len,
                                         const type::Type *t) const;

  i32 ComputeArrayLength(i32 len, const type::Type *t) const {
    return len * static_cast<i32>(MoveForwardToAlignment(t, bytes(t)));
  }

  // TODO pull Addr into it's own header so we can compute it's size and have
  // this be constexpr without pulling in all of val.h
  static constexpr Architecture InterprettingMachine() {
    return Architecture{sizeof(ir::Addr), alignof(ir::Addr)};
  }

  static constexpr Architecture CompilingMachine() {
    return Architecture{sizeof(void *), alignof(void *)};
  }

  size_t ptr_bytes_;
  size_t ptr_align_;
};

#endif  // ICARUS_ARCHITECTURE_H

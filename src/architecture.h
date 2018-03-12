#ifndef ICARUS_ARCHITECTURE_H
#define ICARUS_ARCHITECTURE_H

#include "type/type.h"
#include <cstddef>

namespace IR {
struct Val;
} // namespace IR

// We only support architectures on which a byte is 8 bits, and assume all
// alignments are powers of two.
struct Architecture {
  size_t alignment(const type::Type *t) const;
  size_t bytes(const type::Type *t) const;

  size_t MoveForwardToAlignment(const type::Type *t, size_t index) const {
    return ((index - 1) | (alignment(t) - 1)) + 1;
  }

  // TODO skip the last alignment requirement?
  IR::Val ComputeArrayLength(const IR::Val &len, const type::Type *t) const;

  i32 ComputeArrayLength(i32 len, const type::Type *t) const {
    return len * static_cast<i32>(MoveForwardToAlignment(t, bytes(t)));
  }

  static constexpr Architecture InterprettingMachine() {
    return Architecture{sizeof(IR::Addr), alignof(IR::Addr)};
  }

  static constexpr Architecture CompilingMachine() {
    return Architecture{sizeof(void *), alignof(void *)};
  }

  size_t ptr_bytes_;
  size_t ptr_align_;
};

#endif // ICARUS_ARCHITECTURE_H

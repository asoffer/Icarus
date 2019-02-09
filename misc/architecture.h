#ifndef ICARUS_ARCHITECTURE_H
#define ICARUS_ARCHITECTURE_H

#include <cstddef>

#include "ir/addr.h"
#include "ir/register.h"

// Many objects here are only callable at compile-time and so while their size
// and alignment make sense as questions with answers at compile-time, they
// should not be accessed at run-time. This is a restriction that should be made
// on calls to `bytes` or `alignment` rather than checked here. Any attempt to
// use the Architecture structure assumes the uses is valid.

namespace type {
struct Type;
}  // namespace type

// We only support architectures on which a byte is 8 bits, and assume all
// alignments are powers of two.
struct Architecture {
  size_t alignment(type::Type const *t) const;
  size_t bytes(type::Type const *t) const;

  size_t MoveForwardToAlignment(type::Type const *t, size_t index) const {
    return ((index - 1) | (alignment(t) - 1)) + 1;
  }

  ir::RegisterOr<int32_t> ComputeArrayLength(ir::RegisterOr<int32_t> len,
                                         type::Type const *t) const;

  int32_t ComputeArrayLength(int32_t len, type::Type const *t) const {
    return len * static_cast<int32_t>(MoveForwardToAlignment(t, bytes(t)));
  }

  static constexpr Architecture InterprettingMachine() {
    return Architecture{sizeof(ir::Addr), alignof(ir::Addr)};
  }

  static constexpr Architecture CompilingMachine() {
    return Architecture{sizeof(void *), alignof(void *)};
  }

  size_t ptr_bytes_;
  size_t ptr_align_;
  constexpr static size_t local_ptr_bytes_ = sizeof(void *);
  constexpr static size_t local_ptr_align_ = alignof(void *);
};

#endif  // ICARUS_ARCHITECTURE_H

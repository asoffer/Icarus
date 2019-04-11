#ifndef ICARUS_CORE_ARCH_H
#define ICARUS_CORE_ARCH_H

#include "ir/addr.h"
#include "ir/any_func.h"
#include "core/alignment.h"
#include "core/bytes.h"

namespace core {

struct Arch {
  Bytes const ptr_bytes;
  Alignment const ptr_alignment;

  Bytes const fn_ptr_bytes;
  Alignment const fn_ptr_alignment;
};

constexpr Arch Host() {
  return Arch{/* ptr_bytes        = */ Bytes{sizeof(void*)},
              /* ptr_alignment    = */ Alignment{alignof(void*)},
              /* fn_ptr_bytes     = */ Bytes{sizeof(void (*)())},
              /* fn_ptr_alignment = */ Alignment{alignof(void (*)())}};
}

constexpr Arch Interpretter() {
  return Arch{/* ptr_bytes        = */ Bytes{sizeof(ir::Addr)},
              /* ptr_alignment    = */ Alignment{alignof(ir::Addr)},
              /* fn_ptr_bytes     = */ Bytes{sizeof(ir::AnyFunc)},
              /* fn_ptr_alignment = */ Alignment{alignof(ir::AnyFunc)}};
}

constexpr Bytes FwdAlign(Bytes b, Alignment a) {
  return Bytes{((b.value() - 1u) | (a.value() - 1u)) + 1u};
}

}  // namespace core

#endif  // ICARUS_CORE_ARCH_H

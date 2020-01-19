#ifndef ICARUS_CORE_ARCH_H
#define ICARUS_CORE_ARCH_H

#include "core/alignment.h"
#include "core/bytes.h"
#include "core/type_contour.h"

namespace core {

struct Arch {
  template <typename PtrType, typename FnType>
  static constexpr Arch Get() {
    return Arch(TypeContour::Get<PtrType>(), TypeContour::Get<FnType>());
  }

  constexpr TypeContour pointer() const { return pointer_; }

  // C/C++ would refer to this as a function pointer.
  constexpr TypeContour function() const { return function_; }

 private:
  constexpr Arch(TypeContour ptr, TypeContour fn)
      : pointer_(ptr), function_(fn) {}

  TypeContour pointer_, function_;
};

inline constexpr Arch Host = Arch::Get<void *, void (*)()>();

constexpr Bytes FwdAlign(Bytes b, Alignment a) {
  return Bytes{((b.value() - 1u) | (a.value() - 1u)) + 1u};
}

}  // namespace core

#endif  // ICARUS_CORE_ARCH_H

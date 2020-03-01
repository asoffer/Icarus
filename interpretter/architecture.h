#ifndef ICARUS_INTERPRETTER_ARCHITECTURE_H
#define ICARUS_INTERPRETTER_ARCHITECTURE_H

#include "core/arch.h"
#include "ir/value/addr.h"
#include "ir/value/fn.h"

namespace interpretter {

// `kArchitecture` is the constant defining the architecture for the
// interpretter. It may be different from the host architecture because, for
// example, it uses `ir::Addr` as its pointer type rather than `void *`.
inline constexpr auto kArchitecture = core::Arch::Get<ir::Addr, ir::Fn>();

}  // namespace interpretter

#endif // ICARUS_INTERPRETTER_ARCHITECTURE_H

#ifndef ICARUS_IR_INTERPRETER_ARCHITECTURE_H
#define ICARUS_IR_INTERPRETER_ARCHITECTURE_H

#include "core/arch.h"
#include "ir/value/addr.h"
#include "ir/value/fn.h"

namespace interpreter {

// `kArchitecture` is the constant defining the architecture for the
// interpreter. It may be different from the host architecture because, for
// example, it uses `ir::addr_t` as its pointer type rather than `void *`.
inline constexpr auto kArchitecture = core::Arch::Get<ir::addr_t, ir::Fn>();

}  // namespace interpreter

#endif  // ICARUS_IR_INTERPRETER_ARCHITECTURE_H

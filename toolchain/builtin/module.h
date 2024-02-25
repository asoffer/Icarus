#ifndef ICARUS_TOOLCHAIN_BUILTIN_MODULE_H
#define ICARUS_TOOLCHAIN_BUILTIN_MODULE_H

#include <span>
#include <string_view>

#include "ir/module.h"

namespace ic::builtin {

void PopulateModule(Module& builtin);

}  // namespace ic

#endif  // ICARUS_TOOLCHAIN_BUILTIN_MODULE_H

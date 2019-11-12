#ifndef ICARUS_IR_CMD_EXECUTE_H
#define ICARUS_IR_CMD_EXECUTE_H

#include <dlfcn.h>
#include <type_traits>
#include <vector>

#include "backend/exec.h"
#include "base/untyped_buffer.h"
#include "compiler/compiler.h"
#include "ir/addr.h"
#include "ir/cmd/basic.h"
#include "ir/cmd/call.h"
#include "ir/cmd/cast.h"
#include "ir/cmd/jumps.h"
#include "ir/cmd/load.h"
#include "ir/cmd/misc.h"
#include "ir/cmd/phi.h"
#include "ir/cmd/print.h"
#include "ir/cmd/register.h"
#include "ir/cmd/return.h"
#include "ir/cmd/scope.h"
#include "ir/cmd/store.h"
#include "ir/cmd/types.h"
#include "ir/compiled_fn.h"

#endif  // ICARUS_IR_CMD_EXECUTE_H

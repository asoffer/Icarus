#include "context.h"

#include "module.h"
#include "base/debug.h"

Context::Context(Module* mod) : mod_(ASSERT_NOT_NULL(mod)) {}


#include "visitor/traditional_compilation.h"

#include "ir/builder.h"
#include "ir/results.h"
#include "visitor/emit_ir.h"
#include "visitor/verify_type.h"

namespace visitor {

TraditionalCompilation::TraditionalCompilation(Module *mod)
    : mod_(mod), ctx_(mod), bldr_(ir::GetBuilder()) {}

}  // namespace visitor

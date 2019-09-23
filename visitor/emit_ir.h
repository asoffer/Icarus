#ifndef ICARUS_VISITOR_EMIT_IR_H
#define ICARUS_VISITOR_EMIT_IR_H

#include <vector>

#include "ast/ast_fwd.h"
#include "base/debug.h"
#include "ir/addr.h"
#include "ir/reg.h"
#include "ir/reg_or.h"
#include "type/type_fwd.h"
#include "type/typed_value.h"
#include "visitor/deferred_body.h"

struct Module;

namespace ir {
struct Builder;
struct Results;
}  // namespace ir

#endif  // ICARUS_VISITOR_EMIT_IR_H

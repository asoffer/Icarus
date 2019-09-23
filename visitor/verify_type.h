#ifndef ICARUS_VISITOR_VERIFY_TYPE_H
#define ICARUS_VISITOR_VERIFY_TYPE_H

#include <iostream>

#include "ast/ast_fwd.h"
#include "base/debug.h"
#include "visitor/verify_result.h"

namespace visitor {
struct TraditionalCompilation;

VerifyResult VerifyBody(TraditionalCompilation *visitor,
                        ast::FunctionLiteral const *node);
void VerifyBody(TraditionalCompilation *visitor, ast::JumpHandler const *node);

}  // namespace visitor

#endif  // ICARUS_VISITOR_VERIFY_TYPE_H

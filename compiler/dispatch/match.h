#ifndef ICARUS_COMPILER_DISPATCH_MATCH_H
#define ICARUS_COMPILER_DISPATCH_MATCH_H

#include "ast/ast_fwd.h"
#include "base/expected.h"
#include "compiler/verify_result.h"
#include "core/fn_args.h"
#include "core/fn_params.h"
#include "type/typed_value.h"

namespace compiler {

struct FailedMatch {};

base::expected<core::FnParams<type::Typed<ast::Declaration const *>>,
               FailedMatch>
MatchArgsToParams(
    core::FnParams<type::Typed<ast::Declaration const *>> const &params,
    core::FnArgs<compiler::VerifyResult> const &args);

}  // namespace compiler

#endif  // ICARUS_COMPILER_DISPATCH_MATCH_H

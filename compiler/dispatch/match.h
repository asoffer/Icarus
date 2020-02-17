#ifndef ICARUS_COMPILER_DISPATCH_MATCH_H
#define ICARUS_COMPILER_DISPATCH_MATCH_H

#include "ast/ast_fwd.h"
#include "base/expected.h"
#include "core/fn_args.h"
#include "core/params.h"
#include "core/params_ref.h"
#include "type/qual_type.h"
#include "type/typed_value.h"

namespace compiler {

struct FailedMatch {};

base::expected<core::Params<type::Typed<ast::Declaration const *>>,
               FailedMatch>
MatchArgsToParams(
    core::ParamsRef<type::Typed<ast::Declaration const *>> params,
    core::FnArgs<type::QualType> const &args);

}  // namespace compiler

#endif  // ICARUS_COMPILER_DISPATCH_MATCH_H

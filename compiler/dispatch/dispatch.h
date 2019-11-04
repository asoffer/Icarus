#ifndef ICARUS_COMPILER_DISPATCH_DISPATCH_H
#define ICARUS_COMPILER_DISPATCH_DISPATCH_H

#include "ast/overload_set.h"
#include "base/expected.h"
#include "compiler/compiler.h"
#include "compiler/dispatch/match.h"
#include "compiler/verify_result.h"
#include "core/fn_args.h"

namespace compiler {

struct DispatchTable {
  static base::expected<DispatchTable> Verify(
      Compiler *compiler, ast::OverloadSet const &os,
      core::FnArgs<VerifyResult> const &args);
};

}  // namespace compiler

#endif  // ICARUS_COMPILER_DISPATCH_DISPATCH_H

#ifndef ICARUS_COMPILER_DISPATCH_H
#define ICARUS_COMPILER_DISPATCH_H

#include "ast/overload_set.h"
#include "base/expected.h"
#include "compiler/compiler.h"
#include "compiler/verify_result.h"
#include "core/fn_args.h"

namespace compiler {

struct DispatchTable {
  static base::expected<DispatchTable> Verify(
      Compiler *compiler, ast::OverloadSet const &os,
      core::FnArgs<VerifyResult> const &args);
};

}  // namespace compiler

#endif  // ICARUS_COMPILER_DISPATCH_H

cc_lib_target(
    name = "dispatch",
    cfgs = ["match", "compile"],
    intf_deps = [
        "//ast:overload_set",
        "//base:expected",
        "//core:fn_args",
    ],
    impl_deps = [
        "//backend:eval",
        "//core:fn_params",
        "//ir:any_func",
        "//type:function",
        "//type:type",
        "//type:typed_value",
    ],
    test_deps = [],
)



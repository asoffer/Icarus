#ifndef ICARUS_COMPILER_DISPATCH_TABLE_H
#define ICARUS_COMPILER_DISPATCH_TABLE_H

#include <optional>

#include "absl/container/flat_hash_map.h"
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

 private:
  absl::flat_hash_map<ast::Expression const *,
                      core::FnParams<type::Type const *>>
      table_;

  std::optional<
      absl::flat_hash_map<ast::Expression const *, type::Type const *>>
      result_type_;
};

}  // namespace compiler

#endif  // ICARUS_COMPILER_DISPATCH_TABLE_H

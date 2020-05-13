#ifndef ICARUS_COMPILER_DISPATCH_FN_CALL_TABLE_H
#define ICARUS_COMPILER_DISPATCH_FN_CALL_TABLE_H

#include <optional>

#include "absl/container/flat_hash_map.h"
#include "ast/overload_set.h"
#include "base/expected.h"
#include "compiler/dispatch/overload.h"
#include "core/fn_args.h"
#include "core/params.h"
#include "ir/value/value.h"
#include "type/callable.h"
#include "type/qual_type.h"
#include "type/type.h"
#include "type/typed_value.h"

namespace compiler {
struct Compiler;  // TODO move into it's own header.

// TODO it looks like we actually won't be sharing much between function-call
// and jump in terms of the interface. Should probably inline TableImpl and
// separate out these headers.
struct FnCallDispatchTable {
  static std::optional<ir::Value> Emit(
      Compiler *c, ast::OverloadSet const &os,
      core::FnArgs<type::Typed<ir::Value>> const &args);

 private:
  static type::QualType ComputeResultQualType(
      absl::flat_hash_map<ast::Expression const *, internal::ExprData> const
          &table);
  static type::QualType ComputeResultQualType(
      absl::Span<type::Function const *const> &fn_types);

  absl::flat_hash_map<ast::Expression const *, internal::ExprData> table_;
  type::QualType result_type_;
};

}  // namespace compiler

#endif  // ICARUS_COMPILER_DISPATCH_FN_CALL_TABLE_H

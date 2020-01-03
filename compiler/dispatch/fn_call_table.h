#ifndef ICARUS_COMPILER_DISPATCH_FN_CALL_TABLE_H
#define ICARUS_COMPILER_DISPATCH_FN_CALL_TABLE_H

#include <optional>

#include "absl/container/flat_hash_map.h"
#include "ast/overload_set.h"
#include "base/expected.h"
#include "compiler/dispatch/overload.h"
#include "core/fn_args.h"
#include "core/fn_params.h"
#include "type/qual_type.h"
#include "type/type.h"
#include "type/typed_value.h"

namespace compiler {
struct Compiler;  // TODO move into it's own header.

// TODO it looks like we actually won't be sharing much between function-call
// and jump in terms of the interface. Should probably inline TableImpl and
// separate out these headers.
struct FnCallDispatchTable {
  static base::expected<FnCallDispatchTable> Verify(
      Compiler *compiler, ast::OverloadSet const &os,
      core::FnArgs<type::QualType> const &args);

  type::QualType result_qual_type() const { return result_type_; }

  ir::Results EmitCall(
      Compiler *compiler,
      core::FnArgs<type::Typed<ir::Results>> const &args) const;

 private:
  static type::QualType ComputeResultQualType(
      absl::flat_hash_map<ast::Expression const *, internal::ExprData> const
          &table);

  absl::flat_hash_map<ast::Expression const *, internal::ExprData> table_;
  type::QualType result_type_;
};

}  // namespace compiler

#endif  // ICARUS_COMPILER_DISPATCH_FN_CALL_TABLE_H

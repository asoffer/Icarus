#ifndef ICARUS_COMPILER_DISPATCH_PARAMETERS_AND_ARGUMENTS_H
#define ICARUS_COMPILER_DISPATCH_PARAMETERS_AND_ARGUMENTS_H

#include "absl/container/flat_hash_map.h"
#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/dispatch/overload.h"
#include "core/fn_params.h"
#include "type/cast.h"
#include "type/type.h"
#include "type/typed_value.h"

namespace compiler {

std::vector<core::FnArgs<type::Type const *>> ExpandedFnArgs(
    core::FnArgs<VerifyResult> const &fnargs);

template <typename IgnoredType>
bool ParamsCoverArgs(
    core::FnArgs<VerifyResult> const &args,
    absl::flat_hash_map<IgnoredType, internal::ExprData> const &table) {
  auto expanded_fnargs = ExpandedFnArgs(args);
  for (auto const &expanded_arg : expanded_fnargs) {
    for (auto const & [ k, v ] : table) {
      bool callable = core::IsCallable(
          v.params, args,
          [](VerifyResult arg, type::Typed<ast::Declaration const *> param) {
            return type::CanCast(arg.type(), param.type());
          });
      if (callable) { goto next_expanded_arg; }
    }
    return false;
  next_expanded_arg:;
  }
  return true;
}

core::FnParams<type::Typed<ast::Declaration const *>> ExtractParams(
    Compiler *compiler, ast::Expression const *expr);

}  // namespace compiler

#endif  // ICARUS_COMPILER_DISPATCH_PARAMETERS_AND_ARGUMENTS_H

#ifndef ICARUS_COMPILER_DISPATCH_PARAMETERS_AND_ARGUMENTS_H
#define ICARUS_COMPILER_DISPATCH_PARAMETERS_AND_ARGUMENTS_H

#include "absl/container/flat_hash_map.h"
#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/dispatch/overload.h"
#include "core/fn_params.h"
#include "ir/results.h"
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
  DEBUG_LOG("ParamsCoverArgs")("Unexpanded args: ", args.to_string());

  auto expanded_fnargs = ExpandedFnArgs(args);
  for (auto const &expanded_arg : expanded_fnargs) {
    DEBUG_LOG("ParamsCoverArgs")("Expansion: ", expanded_arg.to_string());
    for (auto const & [ k, v ] : table) {
      // TODO take constness into account for callability.
      bool callable =
          core::IsCallable(v.params, expanded_arg,
                           [](type::Type const *arg,
                              type::Typed<ast::Declaration const *> param) {
                             bool result = type::CanCast(arg, param.type());
                             DEBUG_LOG("ParamsCoverArgs")
                             ("    ... CanCast(", arg->to_string(), ", ",
                              param.type()->to_string(), ") = ", result);
                             return result;
                           });
      DEBUG_LOG("ParamsCoverArgs")(" Callable: ", callable);
      if (callable) { goto next_expanded_arg; }
    }
    return false;
  next_expanded_arg:;
  }
  return true;
}

core::FnParams<type::Typed<ast::Declaration const *>> ExtractParams(
    Compiler *compiler, ast::Expression const *expr);

ir::Results PrepareArg(ir::Builder &bldr, type::Typed<ir::Results> const &arg,
                       type::Type const *param_type);

}  // namespace compiler

#endif  // ICARUS_COMPILER_DISPATCH_PARAMETERS_AND_ARGUMENTS_H

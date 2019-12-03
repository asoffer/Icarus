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

template <typename TableType, typename ParamAccessor>
bool ParamsCoverArgs(core::FnArgs<VerifyResult> const &args,
                     TableType const &table, ParamAccessor &&get_params) {
  DEBUG_LOG("ParamsCoverArgs")("Unexpanded args: ", args.to_string());

  auto expanded_fnargs = ExpandedFnArgs(args);
  for (auto const &expanded_arg : expanded_fnargs) {
    DEBUG_LOG("ParamsCoverArgs")("Expansion: ", expanded_arg.to_string());
    for (auto const & [ k, v ] : table) {
      static_assert(
          std::is_same_v<
              decltype(get_params(k, v)),
              core::FnParams<type::Typed<ast::Declaration const *>> const &>);
      auto const &params = get_params(k, v);
      DEBUG_LOG("ParamsCoverArgs")("Params: ", stringify(params));

      // TODO take constness into account for callability.
      bool callable =
          core::IsCallable(params, expanded_arg,
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

// Given arguments `args` for a function-call with parameters `params`, emits
// the necessary code to prepare the arguments for being called (without
// actually calling). This means performing any necessary conversions, including
// handling variants. Note that the arguments passed in may not be of a type
// which the parameters can directly accept. This is because `args` are
// evaluated once for an entire overload set, but `PrepareCallArguments` is
// called for each particular overload. So for example,
//
// ```
// f ::= (n: int64) -> () {}
// f ::= (b: bool) -> () {}
//
// x := int64 | bool = true
// f(x)
// ```
//
// For each overload of `f`, the argument will be have type `int64 | bool`, even
// though for each overload (one with `int64`, and one with `bool`), the call
// cannot be made directly. In such cases it is assumed that the variant holds a
// value correctly bindable to the parameters. It is the responsibility of the
// caller of this function to ensure that code has already been emitted to
// guard for this situation.
std::vector<ir::Results> PrepareCallArguments(
    Compiler *compiler,
    core::FnParams<type::Typed<ast::Declaration const *>> const &params,
    core::FnArgs<type::Typed<ir::Results>> const &args);

}  // namespace compiler

#endif  // ICARUS_COMPILER_DISPATCH_PARAMETERS_AND_ARGUMENTS_H

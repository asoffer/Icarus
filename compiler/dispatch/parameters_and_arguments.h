#ifndef ICARUS_COMPILER_DISPATCH_PARAMETERS_AND_ARGUMENTS_H
#define ICARUS_COMPILER_DISPATCH_PARAMETERS_AND_ARGUMENTS_H

#include "absl/container/flat_hash_map.h"
#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/dispatch/overload.h"
#include "core/params.h"
#include "ir/value/value.h"
#include "type/cast.h"
#include "type/type.h"
#include "type/typed_value.h"

namespace compiler {

std::vector<core::FnArgs<type::Type const *>> ExpandedFnArgs(
    core::FnArgs<type::QualType> const &fnargs);

template <typename TableType, typename ParamAccessor>
bool ParamsCoverArgs(core::FnArgs<type::QualType> const &args,
                     TableType const &table, ParamAccessor &&get_params) {
  LOG("ParamsCoverArgs","Unexpanded args: %s", args.to_string());

  auto expanded_fnargs = ExpandedFnArgs(args);
  for (auto const &expanded_arg : expanded_fnargs) {
    LOG("ParamsCoverArgs", "Expansion: %s", expanded_arg.to_string());
    LOG("ParamsCoverArgs", "table.size(): %u", table.size());
    for (auto const &[k, v] : table) {
      static_assert(std::is_same_v<std::decay_t<decltype(get_params(k, v))>,
                                   core::Params<type::QualType>>);
      auto params = get_params(k, v);
      std::stringstream ss;
      ss << params;
      LOG("ParamsCoverArgs", "Params: %s", ss.str());

      // TODO take constness into account for callability.
      bool callable =
          core::IsCallable(core::ParamsRef(params), expanded_arg,
                           [](type::Type const *arg, type::QualType param) {
                             bool result = type::CanCast(arg, param.type());
                             LOG("ParamsCoverArgs",
                                 "    ... CanCast(%s, %s) = %s",
                                 arg->to_string(), param.type()->to_string(),
                                 result ? "true" : "false");
                             return result;
                           });
      LOG("ParamsCoverArgs", " Callable: %s", callable ? "true" : "false");
      if (callable) { goto next_expanded_arg; }
    }
    return false;
  next_expanded_arg:;
  }
  return true;
}

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
//
// This function assumes that all arguments are present (i.e., we are not
// relying on any defaulted parameters). Any such parameter should be used to
// fill the arguments before calling this function.
std::vector<ir::Value> PrepareCallArguments(
    Compiler *compiler, type::Type const *state_ptr_type,
    core::Params<type::QualType> const &params,
    core::FnArgs<type::Typed<ir::Value>> const &args);

}  // namespace compiler

#endif  // ICARUS_COMPILER_DISPATCH_PARAMETERS_AND_ARGUMENTS_H

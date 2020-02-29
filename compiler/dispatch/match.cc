#include "compiler/dispatch/match.h"

#include "ast/ast.h"
#include "type/cast.h"
#include "type/type.h"

namespace compiler {
namespace {

std::optional<FailedMatch> MatchPositionalArgsToParams(
    core::ParamsRef<type::Typed<ast::Declaration const *>> params,
    core::FnArgs<type::QualType> const &args,
    core::Params<type::Typed<ast::Declaration const *>> *matched_params) {
  if (args.size() > params.size()) { return FailedMatch{}; }
  for (size_t i = 0; i < args.pos().size(); ++i) {
    auto const &param      = params[i];
    type::Type const *meet = type::Meet(args.at(i).type(), param.value.type());
    if (not meet) { return FailedMatch{}; }
    matched_params->append(param.name, type::Typed(param.value.get(), meet),
                           param.flags);
  }
  return std::nullopt;
}

std::optional<FailedMatch> MatchNamedArgsToParams(
    core::ParamsRef<type::Typed<ast::Declaration const *>> params,
    core::FnArgs<type::QualType> const &args,
    core::Params<type::Typed<ast::Declaration const *>> *matched_params) {
  for (size_t i = args.pos().size(); i < params.size(); ++i) {
    auto const &param = params[i];
    DEBUG_LOG("match")
    ("Matching param in position ", i, "(name = ", param.name, ")");
    if (auto *result = args.at_or_null(param.name)) {
      if (param.value.get() and
          (*param.value)->flags() & ast::Declaration::f_IsConst) {
        NOT_YET();
      } else {
        type::Type const *meet = type::Meet(result->type(), param.value.type());
        if (not meet) { return FailedMatch{}; }
        matched_params->append(param.name, type::Typed(param.value.get(), meet),
                               param.flags);
      }
    } else {
      // No argument provided by that name? This could be because we have
      // default parameters or an empty variadic pack.
      // TODO: Handle variadic packs.
      if (param.flags & core::HAS_DEFAULT) {
        matched_params->append(param.name, param.value, param.flags);
      } else {
        return FailedMatch{};
      }
    }
  }
  return std::nullopt;
}

}  // namespace

base::expected<core::Params<type::Typed<ast::Declaration const *>>, FailedMatch>
MatchArgsToParams(
    core::Params<type::Typed<ast::Declaration const *>> const &params,
    core::FnArgs<type::QualType> const &args) {
  if (args.size() > params.size()) { return FailedMatch{}; }

  core::Params<type::Typed<ast::Declaration const *>> matched_params;
  if (auto failure =
          MatchPositionalArgsToParams(params, args, &matched_params)) {
    return *failure;
  }
  if (auto failure = MatchNamedArgsToParams(params, args, &matched_params)) {
    return *failure;
  }
  return matched_params;
}

}  // namespace compiler

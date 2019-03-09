#include "ast/dispatch/arg_resolution.h"

#include <map>
#include <iostream>

#include "ast/declaration.h"
#include "backend/eval.h"
#include "ir/results.h"
#include "misc/context.h"
#include "type/cast.h"

namespace ast {

ir::Results ArgResolution::Results(
    core::FnParams<Expression *> *const_params,
    std::unordered_map<Expression *, ir::Results const *> const &expr_map,
    Context *ctx) const {
  ir::Results results;
  for (size_t i = 0; i < entries_.size(); ++i) {
    auto entry = entries_.at(i);
    if (entry.defaulted()) {
      Expression *default_expr = (*ASSERT_NOT_NULL(const_params)).at(i).value;
      // TODO this would more suitably take results as a pointer and append to
      // it.
      results.append(ASSERT_NOT_NULL(entry.type)
                         ->PrepareArgument(ctx->type_of(default_expr),
                                           default_expr->EmitIr(ctx), ctx));
    } else {
      auto *t = entry.type;
      if (entry.expansion_index != -1) {
        t = entry.type->as<type::Tuple>().entries_.at(entry.expansion_index);
      }
      // TODO probably always want to pass something like a ResultView?
      results.append(ASSERT_NOT_NULL(entry.type)
                         ->PrepareArgument(t,
                                           expr_map.at(entry.expr)
                                               ->GetResult(std::max(
                                                   0, entry.expansion_index)),
                                           ctx));
    }
  }
  return results;
}

std::ostream &operator<<(std::ostream &os, ArgResolution const &res) {
  std::map<int, std::vector<std::pair<int, int>>> remapping;
  for (auto const &entry : res.entries_) {
    if (entry.expr == nullptr) { continue; }
    remapping[entry.parameter_index].emplace_back(entry.argument_index,
                                                  entry.expansion_index);
  }

  return os << base::stringify(remapping);
}

base::expected<type::Type const *, CallObstruction>
ArgResolution::TypeFromDefaultDecl(Declaration &decl, Context *ctx) {
  if (decl.IsDefaultInitialized()) {
    return CallObstruction::NoDefault(decl.id_);
  }
  // TODO The order for evaluating these is wrong. Defaults may need to
  // be intermixed with non-defaults.
  auto result = decl.VerifyType(ctx);
  // TODO deal with the case where the initial value isn't a constant.
  if (!result.const_) { NOT_YET("log an error."); }

  ctx->bound_constants_.constants_.emplace(
      &decl, backend::Evaluate(decl.init_val.get(), ctx)[0]);
  return result.type_;
}

type::Type const *ArgResolution::TypeFromDecl(Declaration const &decl,
                                              Context *ctx) {
  return ctx->type_of(&decl);
}

base::expected<type::Type const *, CallObstruction>
ArgResolution::MeetWithBoundType(ArgResolution::Entry const &entry,
                                 type::Type const *input_type, Context *ctx) {
  auto *bound_type = ctx->type_of(entry.expr);
  if (entry.expansion_index != -1) {
    bound_type =
        bound_type->as<type::Tuple>().entries_.at(entry.expansion_index);
  }

  ASSIGN_OR(return CallObstruction::TypeMismatch(entry.parameter_index,
                                                 bound_type, input_type),
                   auto &match, type::Meet(bound_type, input_type));
  return &match;
}

}  // namespace ast

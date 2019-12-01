#include "compiler/dispatch/fn_call_table.h"

#include "compiler/compiler.h"
#include "compiler/dispatch/parameters_and_arguments.h"
#include "compiler/dispatch/match.h"
#include "ir/out_params.h"
#include "ir/results.h"
#include "type/cast.h"
#include "type/function.h"
#include "type/type.h"

namespace compiler {
namespace internal {

std::pair<ir::Results, ir::OutParams> SetReturns(
    ExprData const &expr_data, absl::Span<type::Type const *> final_out_types) {
  auto const & [ type, params ] = expr_data;
  auto const &ret_types         = type->as<type::Function>().output;
  ir::Results results;
  ir::OutParams out_params;
  for (type::Type const *ret_type : ret_types) {
    if (ret_type->is_big()) {
      NOT_YET();
    } else {
      out_params.AppendReg(ret_type);
      results.append(out_params.regs_.back());
    }
  }
  return std::pair<ir::Results, ir::OutParams>(std::move(results),
                                               std::move(out_params));
}

ir::Results EmitCallOneOverload(
    Compiler *compiler, ast::Expression const *fn, ExprData const &data,
    core::FnArgs<type::Typed<ir::Results>> const &args) {
  auto const & [ type, params ] = data;
  std::vector<ir::Results> arg_results;
  // TODO prep args (if it's a variant, e.g.)
  for (auto arg : args.pos()) { arg_results.push_back(arg.get()); }
  for (size_t i = args.pos().size(); i < params.size(); ++i) {
    auto const &param = params.at(i);
    if (auto *arg = args.at_or_null(param.name)) {
      arg_results.push_back(arg->get());
    } else {
      arg_results.push_back(ir::Results{compiler->Visit(
          ASSERT_NOT_NULL(param.value.get()->init_val()), EmitValueTag{})});
    }
  }
  auto[out_results, out_params] = SetReturns(data, {});
  compiler->builder().Call(
      compiler->Visit(fn, EmitValueTag{}).get<ir::AnyFunc>(0),
      &compiler->type_of(fn)->as<type::Function>(), arg_results, out_params);
  return std::move(out_results);
}

}  // namespace internal

base::expected<FnCallDispatchTable> FnCallDispatchTable::Verify(
    Compiler *compiler, ast::OverloadSet const &os,
    core::FnArgs<VerifyResult> const &args) {
  DEBUG_LOG("dispatch-verify")
  ("Verifying overload set with ", os.members().size(), " members.");

  // Keep a collection of failed matches around so we can give better
  // diagnostics.
  absl::flat_hash_map<ast::Expression const *, FailedMatch> failures;

  FnCallDispatchTable table;
  for (ast::Expression const *overload : os.members()) {
    // TODO the type of the specific overload could *correctly* be null and we
    // need to handle that case.
    DEBUG_LOG("dispatch-verify")
    ("Verifying ", overload, ": ", overload->DebugString());
    auto result = MatchArgsToParams(ExtractParams(compiler, overload), args);
    if (not result) {
      failures.emplace(overload, result.error());
    } else {
      // TODO you also call compiler->type_of inside ExtractParams, so it's
      // probably worth reducing the number of lookups.
      table.table_.emplace(
          overload, internal::ExprData{compiler->type_of(overload), *result});
    }
  }

  if (not ParamsCoverArgs(args, table.table_)) {
    // TODO Return a failuere-match-reason.
    return base::unexpected("Match failure");
  }

  table.result_type_ = ComputeResultType(table.table_);
  return table;
}

type::Type const *FnCallDispatchTable::ComputeResultType(
    absl::flat_hash_map<ast::Expression const *, internal::ExprData> const
        &table) {
  std::vector<std::vector<type::Type const *>> results;
  for (auto const & [ overload, expr_data ] : table) {
    auto const & [ type, fn_params ] = expr_data;
    DEBUG_LOG("dispatch-verify")
    ("Extracting return type for ", overload->DebugString(), " of type ",
     type->to_string());
    if (auto *fn_type = type->if_as<type::Function>()) {
      auto const &out_vec = fn_type->output;
      results.push_back(out_vec);
    } else if (type == type::Generic) {
      NOT_YET("log error");
    } else {
      NOT_YET();
    }
  }

  return type::MultiVar(results);
}

ir::Results FnCallDispatchTable::EmitCall(
    Compiler *compiler,
    core::FnArgs<type::Typed<ir::Results>> const &args) const {
  if (table_.size() == 1) {
    auto const & [ overload, expr_data ] = *table_.begin();
    return internal::EmitCallOneOverload(compiler, overload, expr_data, args);
  } else {
    NOT_YET();
  }
  return ir::Results{};
}

}  // namespace compiler

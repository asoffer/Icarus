#include "compiler/dispatch/table.h"

#include "ast/expression.h"
#include "ast/methods/dump.h"
#include "base/debug.h"
#include "compiler/compiler.h"
#include "compiler/dispatch/extract_params.h"
#include "core/fn_params.h"
#include "ir/builder.h"
#include "type/cast.h"
#include "type/type.h"
#include "type/variant.h"

namespace compiler::internal {
namespace {
template <typename IndexT>
void AddType(IndexT &&index, type::Type const *t,
             std::vector<core::FnArgs<type::Type const *>> *args) {
  if (auto *vt = t->if_as<type::Variant>()) {
    std::vector<core::FnArgs<type::Type const *>> new_args;
    for (auto *v : vt->variants_) {
      for (auto fnargs : *args) {
        if constexpr (std::is_same_v<std::decay_t<IndexT>, size_t>) {
          fnargs.pos_emplace(v);
        } else {
          fnargs.named_emplace(index, v);
        }
        new_args.push_back(std::move(fnargs));
      }
    }
    *args = std::move(new_args);
  } else {
    std::for_each(
        args->begin(), args->end(),
        [&](core::FnArgs<type::Type const *> &fnargs) {
          if constexpr (std::is_same_v<std::decay_t<IndexT>, size_t>) {
            fnargs.pos_emplace(t);
          } else {
            fnargs.named_emplace(index, t);
          }
        });
  }
}

// TODO: Ideally we wouldn't create these all at once but rather iterate through
// the possibilities. Doing this the right way involves having sum and product
// iterators.
std::vector<core::FnArgs<type::Type const *>> ExpandedFnArgs(
    core::FnArgs<VerifyResult> const &fnargs) {
  std::vector<core::FnArgs<type::Type const *>> all_expanded_options(1);
  fnargs.ApplyWithIndex([&](auto &&index, compiler::VerifyResult r) {
    // TODO also maybe need the expression this came from to see if it needs
    // to be expanded.
    AddType(index, r.type(), &all_expanded_options);
  });

  return all_expanded_options;
}

std::pair<ir::Results, ir::OutParams> SetReturns(
    TableImpl::ExprData const &expr_data,
    absl::Span<type::Type const *> final_out_types) {
  auto const &[type, params] = expr_data;
  auto const &ret_types      = type->as<type::Function>().output;
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
    Compiler *compiler, ast::Expression const *fn,
    TableImpl::ExprData const &data,
    core::FnArgs<type::Typed<ir::Results>> const &args) {
  auto const &[type, params] = data;
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
  auto [out_results, out_params] = SetReturns(data, {});
  compiler->builder().Call(
      compiler->Visit(fn, EmitValueTag{}).get<ir::AnyFunc>(0),
      &compiler->type_of(fn)->as<type::Function>(), arg_results, out_params);
  return std::move(out_results);
}

}  // namespace

base::expected<TableImpl> TableImpl::Verify(
    Compiler *compiler, ast::OverloadSet const &os,
    core::FnArgs<VerifyResult> const &args) {
  DEBUG_LOG("dispatch-verify")
  ("Verifying overload set with ", os.members().size(), " members.");

  // Keep a collection of failed matches around so we can give better
  // diagnostics.
  absl::flat_hash_map<ast::Expression const *, FailedMatch> failures;
  TableImpl table;
  for (ast::Expression const *overload : os.members()) {
    // TODO the type of the specific overload could *correctly* be null and we
    // need to handle that case.
    DEBUG_LOG("dispatch-verify")
    ("Verifying ", overload, ": ", ast::Dump::ToString(overload));
    auto result = MatchArgsToParams(ExtractParams(compiler, overload), args);
    if (not result) {
      failures.emplace(overload, result.error());
    } else {
      // TODO you also call compiler->type_of inside ExtractParams, so it's
      // probably worth reducing the number of lookups.
      table.table_.emplace(overload,
                           ExprData{compiler->type_of(overload), *result});
    }
  }

  auto expanded_fnargs = ExpandedFnArgs(args);

  expanded_fnargs.erase(
      std::remove_if(
          expanded_fnargs.begin(), expanded_fnargs.end(),
          [&](core::FnArgs<type::Type const *> const &fnargs) {
            return absl::c_any_of(
                table.table_, [&fnargs](auto const &expr_data) {
                  return core::IsCallable(
                      expr_data.second.params, fnargs,
                      [](type::Type const *arg,
                         type::Typed<ast::Declaration const *> param) {
                        return type::CanCast(arg, param.type());
                      });
                });
          }),
      expanded_fnargs.end());

  if (not expanded_fnargs.empty()) { NOT_YET("log an error"); }
  return table;
}
}  // namespace compiler::internal

namespace compiler {
type::Type const *FnCallDispatchTable::ComputeResultType(
    internal::TableImpl const &impl) {
  std::vector<std::vector<type::Type const *>> results;
  for (auto const &[overload, expr_data] : impl.table_) {
    auto const &[type, fn_params] = expr_data;
    DEBUG_LOG("dispatch-verify")
    ("Extracting return type for ", ast::Dump::ToString(overload), " of type ",
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
  if (impl_.table_.size() == 1) {
    auto const &[overload, expr_data] = *impl_.table_.begin();
    return internal::EmitCallOneOverload(compiler, overload, expr_data, args);
  } else {
    NOT_YET();
  }
  return ir::Results{};
}

}  // namespace compiler

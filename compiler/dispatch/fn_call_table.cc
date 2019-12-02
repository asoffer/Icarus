#include "compiler/dispatch/fn_call_table.h"

#include "compiler/compiler.h"
#include "compiler/dispatch/match.h"
#include "compiler/dispatch/parameters_and_arguments.h"
#include "ir/components.h"
#include "ir/out_params.h"
#include "ir/results.h"
#include "type/cast.h"
#include "type/function.h"
#include "type/type.h"

namespace compiler {
namespace {

std::pair<ir::Results, ir::OutParams> SetReturns(
    internal::ExprData const &expr_data,
    absl::Span<type::Type const *> final_out_types) {
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
    Compiler *compiler, ast::Expression const *fn,
    internal::ExprData const &data,
    core::FnArgs<type::Typed<ir::Results>> const &args) {
  DEBUG_LOG("EmitCallOneOverload")
  (args.Transform([](auto const &x) { return x.type()->to_string(); })
       .to_string());

  auto const & [ type, params ] = data;
  auto arg_results = PrepareCallArguments(compiler, params, args);

  auto[out_results, out_params] = SetReturns(data, {});
  compiler->builder().Call(
      compiler->Visit(fn, EmitValueTag{}).get<ir::AnyFunc>(0),
      &compiler->type_of(fn)->as<type::Function>(), arg_results, out_params);
  return std::move(out_results);
}

// Emits code which determines if a function with parameters `params` should be
// called with arguments `args`. It does this by looking for variants in `args`
// and testing the actually held type to see if it matches the corresponding
// parameter type. Note that the parameter type need not be identical. Rather,
// there must be a cast from the actual argument type to the parameter type
// (usually due to a cast such as `int64` casting to `int64 | bool`).
ir::RegOr<bool> EmitRuntimeDispatchOneComparison(
    ir::Builder &bldr,
    core::FnParams<type::Typed<ast::Declaration const *>> const &params,
    core::FnArgs<type::Typed<ir::Results>> const &args) {
  size_t i = 0;
  for (; i < args.pos().size(); ++i) {
    auto &arg     = args.pos()[i];
    auto *arg_var = arg.type()->if_as<type::Variant>();
    if (not arg_var) { continue; }
    auto runtime_type =
        ir::Load<type::Type const *>(bldr.VariantType(arg->get<ir::Addr>(0)));
    // TODO Equality isn't the right thing to check
    return bldr.Eq(runtime_type, params.at(i).value.type());
  }
  for (; i < params.size(); ++i) {
    auto const &param = params.at(i);
    auto *arg         = args.at_or_null(param.name);
    if (not arg) { continue; }  // Default arguments
    auto *arg_var = arg->type()->if_as<type::Variant>();
    if (not arg_var) { continue; }
    NOT_YET();
  }
  return ir::RegOr<bool>(false);
}

// Emits code which jumps to the appropriate argument-prep-and-function-call
// after testing variants for the right type.
void EmitRuntimeDispatch(
    ir::Builder &bldr,
    absl::flat_hash_map<ast::Expression const *, internal::ExprData> const
        &table,
    absl::flat_hash_map<ast::Expression const *, ir::BasicBlock *> const
        &callee_to_block,
    core::FnArgs<type::Typed<ir::Results>> const &args) {
  // TODO This is a simple linear search through the table which is certainly a
  // bad idea. We can optimize it later. Likely the right way to do this is to
  // find a perfect hash of the function variants that produces an index into a
  // block table so we pay for a hash and a single indirect jump. This may be
  // harder if you remove variant and implement `overlay`.

  auto iter = table.begin();

  while (true) {
    auto const & [ overload, expr_data ] = *iter;
    ++iter;

    if (iter == table.end()) {
      bldr.UncondJump(callee_to_block.at(overload));
      break;
    }

    ir::RegOr<bool> match =
        EmitRuntimeDispatchOneComparison(bldr, expr_data.params, args);
    bldr.CurrentBlock() =
        ir::EarlyExitOn<true>(callee_to_block.at(overload), match);
  }
}

}  // namespace

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
  DEBUG_LOG("FnCallDispatchTable")
  ("Emitting a table with ", table_.size(), " entries.");
  if (table_.size() == 1) {
    // If there's just one entry in the table we can avoid doing all the work to
    // generate runtime dispatch code. It will amount to only a few
    // unconditional jumps between blocks which will be optimized out, but
    // there's no sense in generating them in the first place..
    auto const & [ overload, expr_data ] = *table_.begin();
    return EmitCallOneOverload(compiler, overload, expr_data, args);
  } else {
    auto &bldr           = compiler->builder();
    auto *land_block     = bldr.AddBlock();
    auto callee_to_block = bldr.AddBlocks(table_);

    EmitRuntimeDispatch(bldr, table_, callee_to_block, args);

    for (auto const & [ overload, expr_data ] : table_) {
      bldr.CurrentBlock() = callee_to_block[overload];
      // Argument preparation is done inside EmitCallOneOverload
      EmitCallOneOverload(compiler, overload, expr_data, args);
      // TODO phi-node to coalesce return values.
      bldr.UncondJump(land_block);
    }
    bldr.CurrentBlock() = land_block;
    return ir::Results{};
  }
}

}  // namespace compiler

#include "compiler/dispatch/fn_call_table.h"

#include "compiler/compiler.h"
#include "compiler/dispatch/match.h"
#include "compiler/dispatch/parameters_and_arguments.h"
#include "compiler/dispatch/runtime.h"
#include "core/params_ref.h"
#include "ir/components.h"
#include "ir/out_params.h"
#include "ir/results.h"
#include "type/cast.h"
#include "type/function.h"
#include "type/type.h"

namespace compiler {
namespace {

std::pair<ir::Results, ir::OutParams> SetReturns(
    ir::Builder &bldr, internal::ExprData const &expr_data,
    absl::Span<type::Type const *> final_out_types) {
  auto ret_types = expr_data.type()->as<type::Function>().output();
  ir::Results results;
  ir::OutParams out_params = bldr.OutParams(ret_types);
  // TODO find a better way to extract these. Figure out why you even need to.
  for (size_t i = 0; i < ret_types.size(); ++i) {
    results.append(out_params[i]);
  }
  return std::pair<ir::Results, ir::OutParams>(std::move(results),
                                               std::move(out_params));
}

ir::Results EmitCallOneOverload(Compiler *compiler, ast::Expression const *fn,
                                internal::ExprData const &data,
                                core::FnArgs<type::Typed<ir::Results>> args) {
  DEBUG_LOG("EmitCallOneOverload")
  (args.Transform([](auto const &x) { return x.type()->to_string(); })
       .to_string());

  core::FillMissingArgs(
      core::ParamsRef(data.params()), &args, [compiler](auto const &p) {
        return type::Typed(
            ir::Results{compiler->Visit(ASSERT_NOT_NULL(p.get()->init_val()),
                                        EmitValueTag{})},
            p.type());
      });

  auto arg_results = PrepareCallArguments(
      compiler, nullptr,
      data.params().Transform([](auto const &p) { return p.type(); }), args);

  auto[out_results, out_params] = SetReturns(compiler->builder(), data, {});

  auto callee_qual_type         = *ASSERT_NOT_NULL(compiler->qual_type_of(fn));
  auto callee                   = [&]() -> ir::RegOr<ir::AnyFunc> {
    if (callee_qual_type.constant()) {
      return compiler->Visit(fn, EmitValueTag{}).get<ir::AnyFunc>(0);
    } else {
      // NOTE: If the overload is a declaration, it's not because a declaration
      // is syntactically the callee. Rather, it's because the callee is an
      // identifier (or module_name.identifier, etc.) and this is one possible
      // resolution of that identifier. We cannot directly ask to emit IR for
      // the declaration because that will emit the initialization for the
      // declaration. Instead, we need load the address.
      if (auto *fn_decl = fn->if_as<ast::Declaration>()) {
        return ir::Load<ir::AnyFunc>(compiler->addr(fn_decl));
      } else {
        return ir::Load<ir::AnyFunc>(
            compiler->Visit(fn, EmitValueTag{}).get<ir::Addr>(0));
      }
    }
  }();

  compiler->builder().Call(callee,
                           &callee_qual_type.type()->as<type::Function>(),
                           arg_results, out_params);
  return std::move(out_results);
}

}  // namespace

base::expected<FnCallDispatchTable> FnCallDispatchTable::Verify(
    Compiler *compiler, ast::OverloadSet const &os,
    core::FnArgs<type::QualType> const &args) {
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

  if (not ParamsCoverArgs(args, table.table_,
                          [](auto const &, internal::ExprData const &data) {
                            return data.params();
                          })) {
    // TODO Return a failuere-match-reason.
    return base::unexpected("Match failure");
  }

  table.result_type_ = ComputeResultQualType(table.table_);
  return table;
}

type::QualType FnCallDispatchTable::ComputeResultQualType(
    absl::flat_hash_map<ast::Expression const *, internal::ExprData> const
        &table) {
  std::vector<absl::Span<type::Type const *const>> results;
  for (auto const &[overload, expr_data] : table) {
    DEBUG_LOG("dispatch-verify")
    ("Extracting return type for ", overload->DebugString(), " of type ",
     expr_data.type()->to_string());
    if (auto *fn_type = expr_data.type()->if_as<type::Function>()) {
      auto out_span = fn_type->output();
      results.push_back(out_span);
    } else if (expr_data.type() == type::Generic) {
      results.emplace_back(); // NOT_YET figuring out the real answer.
    } else {
      NOT_YET(expr_data.type()->to_string());
    }
  }

  return type::QualType(type::MultiVar(results), type::Quals::Unqualified());
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
    auto const &[overload, expr_data] = *table_.begin();
    return EmitCallOneOverload(compiler, overload, expr_data, args);
  } else {
    auto &bldr           = compiler->builder();
    auto *land_block     = bldr.AddBlock();
    auto callee_to_block = bldr.AddBlocks(table_);

    EmitRuntimeDispatch(bldr, table_, callee_to_block, args);

    for (auto const &[overload, expr_data] : table_) {
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

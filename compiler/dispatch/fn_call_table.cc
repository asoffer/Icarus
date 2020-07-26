#include "compiler/dispatch/fn_call_table.h"

#include "compiler/compiler.h"
#include "compiler/dispatch/match.h"
#include "compiler/dispatch/parameters_and_arguments.h"
#include "compiler/dispatch/runtime.h"
#include "core/params_ref.h"
#include "diagnostic/errors.h"
#include "ir/out_params.h"
#include "ir/value/generic_fn.h"
#include "ir/value/value.h"
#include "type/cast.h"
#include "type/function.h"
#include "type/generic_function.h"
#include "type/type.h"

namespace compiler {
namespace {

ir::OutParams SetReturnsCopyInit(
    ir::Builder &bldr, type::Type const *type,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  if (auto *fn_type = type->if_as<type::Function>()) {
    return bldr.OutParamsCopyInit(fn_type->output(), to);
  } else if (type->is<type::GenericFunction>()) {
    NOT_YET(type->to_string());
  } else {
    NOT_YET(type->to_string());
  }
}

ir::OutParams SetReturnsMoveInit(
    ir::Builder &bldr, type::Type const *type,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  if (auto *fn_type = type->if_as<type::Function>()) {
    return bldr.OutParamsMoveInit(fn_type->output(), to);
  } else if (type->is<type::GenericFunction>()) {
    NOT_YET(type->to_string());
  } else {
    NOT_YET(type->to_string());
  }
}

ir::OutParams SetReturnsAssign(
    ir::Builder &bldr, type::Type const *type,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  if (auto *fn_type = type->if_as<type::Function>()) {
    return bldr.OutParamsAssign(fn_type->output(), to);
  } else if (type->is<type::GenericFunction>()) {
    NOT_YET(type->to_string());
  } else {
    NOT_YET(type->to_string());
  }
}

ir::RegOr<ir::Fn> ComputeConcreteFn(Compiler *compiler,
                                    ast::Expression const *fn,
                                    type::Function const *f_type,
                                    type::Quals quals) {
  if (type::Quals::Const() <= quals) {
    return compiler->EmitValue(fn).get<ir::RegOr<ir::Fn>>();
  } else {
    // NOTE: If the overload is a declaration, it's not because a
    // declaration is syntactically the callee. Rather, it's because the
    // callee is an identifier (or module_name.identifier, etc.) and this
    // is one possible resolution of that identifier. We cannot directly
    // ask to emit IR for the declaration because that will emit the
    // initialization for the declaration. Instead, we need load the
    // address.
    if (auto *fn_decl = fn->if_as<ast::Declaration>()) {
      return compiler->builder().Load<ir::Fn>(compiler->data().addr(fn_decl));
    } else {
      return compiler->builder().Load<ir::Fn>(
          compiler->EmitValue(fn).get<ir::RegOr<ir::Addr>>());
    }
  }
}

std::tuple<ir::RegOr<ir::Fn>, type::Function const *, DependentComputedData *>
EmitCallee(Compiler &compiler, ast::Expression const *fn, type::QualType qt,
           internal::ExprData const &data,
           const core::FnArgs<type::Typed<ir::Value>> &args) {
  if (auto const *gf_type = qt.type()->if_as<type::GenericFunction>()) {
    ir::GenericFn gen_fn =
        compiler.EmitValue(fn).get<ir::RegOr<ir::GenericFn>>().value();

    // TODO declarations aren't callable so we shouldn't have to check this
    // here.

    if (auto const *decl = fn->if_as<ast::Declaration>()) {
      // TODO make this more robust.
      fn = decl->init_val();
    }

    auto *parameterized_expr = &fn->as<ast::ParameterizedExpression>();

    DependentComputedData temp_data(&compiler.data().module());
    Compiler c({
        .builder             = compiler.builder(),
        .data                = temp_data,
        .diagnostic_consumer = compiler.diag(),
    });
    temp_data.parent_ = &compiler.data();

    auto params = c.ComputeParamsFromArgs(
        parameterized_expr, OrderedDependencyNodes(parameterized_expr), args);

    auto find_dependent_result = compiler.data().FindDependent(
        &fn->as<ast::ParameterizedExpression>(), params);
    return std::make_tuple(ir::Fn(gen_fn.concrete(args)),
                           find_dependent_result.fn_type,
                           &find_dependent_result.data);
  } else if (auto const *f_type =
                 qt.type()->if_as<type::Function>()) {
    return std::make_tuple(ComputeConcreteFn(&compiler, fn, f_type, qt.quals()),
                           f_type, nullptr);
  } else {
    UNREACHABLE();
  }
}

void EmitCallOneOverloadCopyInit(
    Compiler *compiler, ast::Expression const *fn,
    internal::ExprData const &data, core::FnArgs<type::Typed<ir::Value>> args,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  auto callee_qual_type = compiler->qual_type_of(fn);
  ASSERT(callee_qual_type.has_value() == true);

  auto [callee, fn_type, dependent_data] =
      EmitCallee(*compiler, fn, *callee_qual_type, data, args);

  Compiler c({
      .builder = ir::GetBuilder(),
      .data    = dependent_data ? *dependent_data : compiler->data(),
      .diagnostic_consumer = compiler->diag(),
  });

  if (not callee.is_reg()) {
    switch (callee.value().kind()) {
      case ir::Fn::Kind::Native: {
        core::FillMissingArgs(
            core::ParamsRef(callee.value().native()->params()), &args,
            [&c](auto const &p) {
              return type::Typed(
                  c.EmitValue(ASSERT_NOT_NULL(p.get()->init_val())), p.type());
            });
      } break;
      default: break;
    }
  }

  auto out_params = SetReturnsCopyInit(c.builder(), data.type(), to);
  c.builder().Call(callee, fn_type,
                   PrepareCallArguments(&c, nullptr, data.params(), args),
                   out_params);
  int i = -1;
  for (auto const *t : fn_type->output()) {
    ++i;
    if (t->is_big()) continue;
    c.Visit(t, *to[i], type::Typed{ir::Value(out_params[i]), t},
            EmitCopyAssignTag{});
  }
}

void EmitCallOneOverloadMoveInit(
    Compiler *compiler, ast::Expression const *fn,
    internal::ExprData const &data, core::FnArgs<type::Typed<ir::Value>> args,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  auto callee_qual_type = compiler->qual_type_of(fn);
  ASSERT(callee_qual_type.has_value() == true);

  auto [callee, fn_type, dependent_data] =
      EmitCallee(*compiler, fn, *callee_qual_type, data, args);

  Compiler c({
      .builder = ir::GetBuilder(),
      .data    = dependent_data ? *dependent_data : compiler->data(),
      .diagnostic_consumer = compiler->diag(),
  });

  if (not callee.is_reg()) {
    switch (callee.value().kind()) {
      case ir::Fn::Kind::Native: {
        core::FillMissingArgs(
            core::ParamsRef(callee.value().native()->params()), &args,
            [&c](auto const &p) {
              return type::Typed(
                  c.EmitValue(ASSERT_NOT_NULL(p.get()->init_val())), p.type());
            });
      } break;
      default: break;
    }
  }

  auto out_params = SetReturnsMoveInit(c.builder(), data.type(), to);
  c.builder().Call(callee, fn_type,
                   PrepareCallArguments(&c, nullptr, data.params(), args),
                   out_params);
  int i = -1;
  for (auto const *t : fn_type->output()) {
    ++i;
    if (t->is_big()) continue;
    c.Visit(t, *to[i], type::Typed{ir::Value(out_params[i]), t},
            EmitCopyAssignTag{});
  }
}

void EmitCallOneOverloadAssign(
    Compiler *compiler, ast::Expression const *fn,
    internal::ExprData const &data, core::FnArgs<type::Typed<ir::Value>> args,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  auto callee_qual_type = compiler->qual_type_of(fn);
  ASSERT(callee_qual_type.has_value() == true);

  auto [callee, fn_type, dependent_data] =
      EmitCallee(*compiler, fn, *callee_qual_type, data, args);

  Compiler c({
      .builder = ir::GetBuilder(),
      .data    = dependent_data ? *dependent_data : compiler->data(),
      .diagnostic_consumer = compiler->diag(),
  });

  if (not callee.is_reg()) {
    switch (callee.value().kind()) {
      case ir::Fn::Kind::Native: {
        core::FillMissingArgs(
            core::ParamsRef(callee.value().native()->params()), &args,
            [&c](auto const &p) {
              return type::Typed(
                  c.EmitValue(ASSERT_NOT_NULL(p.get()->init_val())), p.type());
            });
      } break;
      default: break;
    }
  }

  auto out_params = SetReturnsAssign(c.builder(), data.type(), to);
  c.builder().Call(callee, fn_type,
                   PrepareCallArguments(&c, nullptr, data.params(), args),
                   out_params);
  int i = -1;
  for (auto const *t : fn_type->output()) {
    ++i;
    if (t->is_big()) continue;
    c.Visit(t, *to[i], type::Typed{ir::Value(out_params[i]), t},
            EmitCopyAssignTag{});
  }
}

void EmitCallCopyInit(Compiler *compiler,
                      absl::flat_hash_map<ast::Expression const *,
                                          internal::ExprData> const &table,
                      core::FnArgs<type::Typed<ir::Value>> const &args,
                      absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  DEBUG_LOG("FnCallDispatchTable")
  ("Emitting a table with ", table.size(), " entries.");

  if (table.size() == 1) {
    // If there's just one entry in the table we can avoid doing all the work to
    // generate runtime dispatch code. It will amount to only a few
    // unconditional jumps between blocks which will be optimized out, but
    // there's no sense in generating them in the first place..
    auto const &[overload, expr_data] = *table.begin();
    EmitCallOneOverloadCopyInit(compiler, overload, expr_data, args, to);
  } else {
    auto &bldr           = compiler->builder();
    auto *land_block     = bldr.AddBlock();
    auto callee_to_block = bldr.AddBlocks(table);

    EmitRuntimeDispatch(bldr, table, callee_to_block, args);

    for (auto const &[overload, expr_data] : table) {
      bldr.CurrentBlock() = callee_to_block[overload];
      // Argument preparation is done inside EmitCallOneOverload
      EmitCallOneOverloadCopyInit(compiler, overload, expr_data, args, to);
      // TODO phi-node to coalesce return values.
      bldr.UncondJump(land_block);
    }
    bldr.CurrentBlock() = land_block;
  }
}

void EmitCallMoveInit(Compiler *compiler,
                      absl::flat_hash_map<ast::Expression const *,
                                          internal::ExprData> const &table,
                      core::FnArgs<type::Typed<ir::Value>> const &args,
                      absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  DEBUG_LOG("FnCallDispatchTable")
  ("Emitting a table with ", table.size(), " entries.");

  if (table.size() == 1) {
    // If there's just one entry in the table we can avoid doing all the work to
    // generate runtime dispatch code. It will amount to only a few
    // unconditional jumps between blocks which will be optimized out, but
    // there's no sense in generating them in the first place..
    auto const &[overload, expr_data] = *table.begin();
    EmitCallOneOverloadMoveInit(compiler, overload, expr_data, args, to);
  } else {
    auto &bldr           = compiler->builder();
    auto *land_block     = bldr.AddBlock();
    auto callee_to_block = bldr.AddBlocks(table);

    EmitRuntimeDispatch(bldr, table, callee_to_block, args);

    for (auto const &[overload, expr_data] : table) {
      bldr.CurrentBlock() = callee_to_block[overload];
      // Argument preparation is done inside EmitCallOneOverload
      EmitCallOneOverloadMoveInit(compiler, overload, expr_data, args, to);
      // TODO phi-node to coalesce return values.
      bldr.UncondJump(land_block);
    }
    bldr.CurrentBlock() = land_block;
  }
}

void EmitCallAssign(Compiler *compiler,
                    absl::flat_hash_map<ast::Expression const *,
                                        internal::ExprData> const &table,
                    core::FnArgs<type::Typed<ir::Value>> const &args,
                    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  DEBUG_LOG("FnCallDispatchTable")
  ("Emitting a table with ", table.size(), " entries.");

  if (table.size() == 1) {
    // If there's just one entry in the table we can avoid doing all the work to
    // generate runtime dispatch code. It will amount to only a few
    // unconditional jumps between blocks which will be optimized out, but
    // there's no sense in generating them in the first place..
    auto const &[overload, expr_data] = *table.begin();
    EmitCallOneOverloadAssign(compiler, overload, expr_data, args, to);
  } else {
    auto &bldr           = compiler->builder();
    auto *land_block     = bldr.AddBlock();
    auto callee_to_block = bldr.AddBlocks(table);

    EmitRuntimeDispatch(bldr, table, callee_to_block, args);

    for (auto const &[overload, expr_data] : table) {
      bldr.CurrentBlock() = callee_to_block[overload];
      // Argument preparation is done inside EmitCallOneOverload
      EmitCallOneOverloadAssign(compiler, overload, expr_data, args, to);
      // TODO phi-node to coalesce return values.
      bldr.UncondJump(land_block);
    }
    bldr.CurrentBlock() = land_block;
  }
}

base::expected<absl::flat_hash_map<ast::Expression const *, internal::ExprData>>
Verify(Compiler *compiler, ast::OverloadSet const &os,
       core::FnArgs<type::Typed<ir::Value>> const &args) {
  DEBUG_LOG("dispatch-verify")
  ("Verifying overload set with ", os.members().size(), " members.");

  // Keep a collection of failed matches around so we can give better
  // diagnostics.
  absl::flat_hash_map<ast::Expression const *, FailedMatch> failures;

  auto args_qt = args.Transform(
      [](auto const &t) { return type::QualType::NonConstant(t.type()); });

  absl::flat_hash_map<ast::Expression const *, internal::ExprData> table;
  for (ast::Expression const *overload : os.members()) {
    // TODO the type of the specific overload could *correctly* be null and we
    // need to handle that case.
    DEBUG_LOG("dispatch-verify")
    ("Verifying ", overload, ": ", overload->DebugString());
    if (auto *gen =
            compiler->type_of(overload)->if_as<type::GenericFunction>()) {
      type::Function const *concrete = gen->concrete(args);
      table.emplace(overload, internal::ExprData{concrete, concrete->params(),
                                                 concrete->return_types(args)});
    } else {
      type::Type const *overload_type = compiler->type_of(overload);
      if (auto result = MatchArgsToParams(
              overload_type->as<type::Function>().params(), args_qt)) {
        table.emplace(overload, internal::ExprData{overload_type, *result});
      } else {
        DEBUG_LOG("dispatch-verify")(result.error());
        failures.emplace(overload, result.error());
      }
    }
  }

  if (not ParamsCoverArgs(args_qt, table,
                          [](auto const &, internal::ExprData const &data) {
                            return data.params();
                          })) {
    // Note: If the overload set is empty, ParamsCoverArgs will emit no
    // diagnostics!
    compiler->diag().Consume(diagnostic::Todo{});
    // TODO Return a failuere-match-reason.
    return base::unexpected("Match failure");
  }

  return table;
}

}  // namespace

void FnCallDispatchTable::EmitCopyInit(
    Compiler *c, ast::OverloadSet const &os,
    core::FnArgs<type::Typed<ir::Value>> const &args,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  // TODO: How do we know if there was an error? Can one actually happen?
  // Shouldn't we have verified earlier.
  ASSIGN_OR(return, auto table, Verify(c, os, args));
  return EmitCallCopyInit(c, table, args, to);
}

void FnCallDispatchTable::EmitMoveInit(
    Compiler *c, ast::OverloadSet const &os,
    core::FnArgs<type::Typed<ir::Value>> const &args,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  // TODO: How do we know if there was an error? Can one actually happen?
  // Shouldn't we have verified earlier.
  ASSIGN_OR(return, auto table, Verify(c, os, args));
  return EmitCallMoveInit(c, table, args, to);
}

void FnCallDispatchTable::EmitAssign(
    Compiler *c, ast::OverloadSet const &os,
    core::FnArgs<type::Typed<ir::Value>> const &args,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  // TODO: How do we know if there was an error? Can one actually happen?
  // Shouldn't we have verified earlier.
  ASSIGN_OR(return, auto table, Verify(c, os, args));
  return EmitCallAssign(c, table, args, to);
}

type::QualType FnCallDispatchTable::ComputeResultQualType(
    absl::Span<type::Function const *const> &fn_types) {
  std::vector<absl::Span<type::Type const *const>> results;
  for (auto const *fn_type : fn_types) { results.push_back(fn_type->output()); }
  return type::QualType(type::MultiVar(results), type::Quals::Unqualified());
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
    } else if (expr_data.type()->is<type::GenericFunction>()) {
      results.emplace_back();  // NOT_YET figuring out the real answer.
    } else {
      NOT_YET(expr_data.type()->to_string());
    }
  }

  return type::QualType(type::MultiVar(results), type::Quals::Unqualified());
}

}  // namespace compiler

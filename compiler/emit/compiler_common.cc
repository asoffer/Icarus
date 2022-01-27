#include "compiler/emit/compiler_common.h"

#include <vector>

#include "compiler/compiler.h"
#include "compiler/emit/common.h"
#include "compiler/emit/copy_move_assignment.h"
#include "compiler/instructions.h"
#include "compiler/module.h"
#include "core/arguments.h"
#include "core/params.h"
#include "type/qual_type.h"
#include "type/type.h"
#include "type/typed_value.h"

namespace compiler {

// TODO: Remove forward declaration.
absl::Span<type::QualType const> VerifyType(CompilationDataReference data,
                                            ast::Node const *node);

namespace {
ir::OutParams SetReturns(
    Compiler &c, type::Type type,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  if (auto *fn_type = type.if_as<type::Function>()) {
    return c.OutParams(fn_type->return_types(), to);
  } else if (type.is<type::Generic<type::Function>>()) {
    NOT_YET(type.to_string());
  } else {
    NOT_YET(type.to_string());
  }
}

core::Params<ast::Expression const *> DefaultsFor(ast::Expression const *expr,
                                                  Context const &context) {
  if (auto const *id = expr->if_as<ast::Identifier>()) {
    auto decl_id_span = context.decls(id);
    switch (decl_id_span.size()) {
      case 0: UNREACHABLE();
      case 1:
        // TODO: This initial value is for the entire declaration, even if it
        // has multiple identifiers.
        return DefaultsFor(decl_id_span[0]->declaration().init_val(), context);
      default: UNREACHABLE();
    }
  } else if (auto const *id = expr->if_as<ast::Declaration::Id>()) {
    return DefaultsFor(id->declaration().init_val(), context);
  } else if (auto const *p = expr->if_as<ast::ParameterizedExpression>()) {
    return p->params().Transform(
        [](auto const &decl) -> ast::Expression const * {
          return decl->init_val();
        });
  } else {
    return {};
  }
}

struct CalleeResult {
  ir::RegOr<ir::Fn> callee;
  type::Function const *type;
  core::Params<ast::Expression const *> defaults;
  Context *context;
};

CalleeResult EmitCallee(
    Compiler &c, CallMetadata::callee_locator_t callable,
    core::Arguments<type::Typed<ir::CompleteResultRef>> const &constants) {
  if (auto const *callable_expr = callable.get_if<ast::Expression>()) {
    type::QualType qt = c.context().qual_types(callable_expr)[0];
    if (auto const *gf_type =
            qt.type().if_as<type::Generic<type::Function>>()) {
      ir::GenericFn gen_fn = c.EmitAs<ir::GenericFn>(callable_expr).value();

      // TODO: declarations aren't callable_expr so we shouldn't have to check
      // this here.
      if (auto const *id = callable_expr->if_as<ast::Declaration::Id>()) {
        // TODO: make this more robust.
        // TODO: support multiple declarations
        callable_expr = id->declaration().init_val();
      }

      auto *parameterized_expr =
          &callable_expr->as<ast::ParameterizedExpression>();
      auto find_subcontext_result =
          FindInstantiation(c, parameterized_expr, constants);
      return {.callee = ir::Fn(gen_fn.concrete(c.work_resources(), constants)),
              .type   = find_subcontext_result.fn_type,
              .defaults =
                  DefaultsFor(callable_expr, find_subcontext_result.context),
              .context = &find_subcontext_result.context};
    } else if (auto const *gs_type =
                   qt.type().if_as<type::Generic<type::Struct>>()) {
      ir::Fn fn = c.EmitAs<ir::Fn>(callable_expr).value();

      // TODO: declarations aren't callable_expr so we shouldn't have to check
      // this here.
      if (auto const *id = callable_expr->if_as<ast::Declaration::Id>()) {
        // TODO: make this more robust.
        // TODO: support multiple declarations
        callable_expr = id->declaration().init_val();
      }

      auto *parameterized_expr =
          &callable_expr->as<ast::ParameterizedExpression>();
      auto find_subcontext_result =
          FindInstantiation(c, parameterized_expr, constants);
      return {.callee = fn,
              .type   = find_subcontext_result.fn_type,
              .defaults =
                  DefaultsFor(callable_expr, find_subcontext_result.context),
              .context = &find_subcontext_result.context};
    } else if (auto const *f_type = qt.type().if_as<type::Function>()) {
      if (type::Quals::Const() <= qt.quals()) {
        auto &context =
            ModuleFor(callable_expr)->as<CompiledModule>().context();
        return {.callee   = c.EmitAs<ir::Fn>(callable_expr),
                .type     = f_type,
                .defaults = DefaultsFor(callable_expr, context),
                .context  = nullptr};
      } else {
        // NOTE: If the overload is a declaration, it's not because a
        // declaration is syntactically the callee. Rather, it's because the
        // callee is an identifier (or module_name.identifier, etc.) and this
        // is one possible resolution of that identifier. We cannot directly
        // ask to emit IR for the declaration because that will emit the
        // initialization for the declaration. Instead, we need load the
        // address.
        if (auto *fn_decl = callable_expr->if_as<ast::Declaration>()) {
          return {.callee  = c.current_block()->Append(ir::LoadInstruction{
                      .type   = f_type,
                      .addr   = c.state().addr(&fn_decl->ids()[0]),
                      .result = c.current().subroutine->Reserve()}),
                  .type    = f_type,
                  .context = nullptr};
        } else if (auto *fn_decl_id =
                       callable_expr->if_as<ast::Declaration::Id>()) {
          return {.callee  = c.current_block()->Append(ir::LoadInstruction{
                      .type   = f_type,
                      .addr   = c.state().addr(fn_decl_id),
                      .result = c.current().subroutine->Reserve()}),
                  .type    = f_type,
                  .context = nullptr};
        } else {
          return {.callee  = c.current_block()->Append(ir::LoadInstruction{
                      .type   = f_type,
                      .addr   = c.EmitAs<ir::addr_t>(callable_expr),
                      .result = c.current().subroutine->Reserve()}),
                  .type    = f_type,
                  .context = nullptr};
        }
      }
    } else {
      UNREACHABLE(callable_expr->DebugString(), "\n", qt.type().to_string());
    }

  } else {
    auto const &symbol_info =
        *callable.get<module::Module::SymbolInformation>();
    type::QualType qt       = symbol_info.qualified_type;
    if (auto const *gf_type =
            qt.type().if_as<type::Generic<type::Function>>()) {
      ir::GenericFn gen_fn = symbol_info.value[0].get<ir::GenericFn>();
      auto *parameterized_expr = &symbol_info.id->declaration()
                                      .init_val()
                                      ->as<ast::ParameterizedExpression>();
      auto find_subcontext_result =
          FindInstantiation(c, parameterized_expr, constants);
      return {.callee = ir::Fn(gen_fn.concrete(c.work_resources(), constants)),
              .type   = find_subcontext_result.fn_type,
              .defaults = DefaultsFor(parameterized_expr,
                                      find_subcontext_result.context),
              .context  = &find_subcontext_result.context};
    } else if (auto const *gs_type =
                   qt.type().if_as<type::Generic<type::Struct>>()) {
      NOT_YET();
    } else if (auto const *f_type = qt.type().if_as<type::Function>()) {
      if (type::Quals::Const() <= qt.quals()) {
        return {
            .callee = symbol_info.value[0].get<ir::Fn>(),
            .type   = f_type,
            // TODO: Defaults .defaults = DefaultsFor(callable_expr, context),
            .context = nullptr};
      } else {
        NOT_YET();
      }
    } else {
      UNREACHABLE(callable_expr->DebugString(), "\n", qt.type().to_string());
    }
  }
}

void EmitAndCast(Compiler &c, ast::Expression const &expr, type::QualType to,
                 ir::PartialResultBuffer &buffer) {
  type::QualType arg_qt = c.context().qual_types(&expr)[0];
  if (auto const *ptr_to_type = to.type().if_as<type::Pointer>()) {
    if (ptr_to_type->pointee() == arg_qt.type()) {
      if (arg_qt.quals() >= type::Quals::Ref()) {
        buffer.append(c.EmitRef(&expr));
      } else {
        auto reg = c.state().TmpAlloca(arg_qt.type());
        c.EmitMoveInit(&expr,
                       {type::Typed<ir::RegOr<ir::addr_t>>(reg, to.type())});
        buffer.append(reg);
      }
      return;
    }
  }

  c.EmitToBuffer(&expr, buffer);
  ApplyImplicitCasts(c, arg_qt.type(), to, buffer);
}

}  // namespace

void EmitArguments(
    Compiler &c, core::Params<type::QualType> const &param_qts,
    core::Params<ast::Expression const *> const &defaults,
    absl::Span<ast::Call::Argument const> arg_exprs,
    core::Arguments<type::Typed<ir::CompleteResultRef>> const &constants,
    ir::PartialResultBuffer &buffer) {
  size_t i = 0;
  while (i < arg_exprs.size() and not arg_exprs[i].named()) {
    absl::Cleanup cleanup = [&] { ++i; };
    auto const &param     = param_qts[i];
    if (param.value.constant()) {
      buffer.append(constants[i]);
    } else {
      EmitAndCast(c, arg_exprs[i].expr(), param.value, buffer);
    }
  }

  size_t named_start = i;
  for (; i < param_qts.size(); ++i) {
    auto const &param     = param_qts[i];
    std::string_view name = param_qts[i].name;
    if (param.value.constant()) {
      if (auto const *value = constants.at_or_null(name)) {
        buffer.append(*value);
      } else {
        // TODO: It'd be better to simply extract the bound value.
        VerifyType(c, defaults[i].value);
        EmitAndCast(c, *defaults[i].value, param.value, buffer);
      }
    } else {
      // TODO: Encapsulate the argument name finding.
      size_t j = named_start;
      for (; j < arg_exprs.size(); ++j) {
        if (arg_exprs[j].name() == name) { break; }
      }
      ast::Expression const *expr;
      if (j == arg_exprs.size()) {
        size_t default_index = *ASSERT_NOT_NULL(defaults.at_or_null(name));
        expr                 = defaults[default_index].value;
      } else {
        expr = &arg_exprs[j].expr();
      }
      EmitAndCast(c, *expr, param.value, buffer);
    }
  }
}

void EmitIrForStatements(Compiler &c, ast::Scope const *scope,
                         base::PtrSpan<ast::Node const> stmts) {
  std::vector<type::Typed<ir::Reg>> temporaries =
      std::exchange(c.state().temporaries_to_destroy, {});
  absl::Cleanup cleanup = [&] {
    c.state().temporaries_to_destroy = std::move(temporaries);
  };

  ir::PartialResultBuffer buffer;
  for (auto *stmt : stmts) {
    buffer.clear();
    c.EmitToBuffer(stmt, buffer);
    c.DestroyTemporaries();
  }

  c.current_block() = c.EmitDestructionPath(scope, scope);
}

void AppendToPartialResultBuffer(Compiler &c, type::QualType qt,
                                 ast::Expression const &expr,
                                 ir::PartialResultBuffer &buffer) {
  if (not qt.constant()) {
    buffer.append();
    return;
  }

  ASSIGN_OR(NOT_YET(),  //
            auto result,
            c.EvaluateToBufferOrDiagnose(
                type::Typed<ast::Expression const *>(&expr, qt.type())));
  buffer.append(result);
}

void EmitCall(Compiler &c, CallMetadata::callee_locator_t callee,
              core::Arguments<type::Typed<ir::CompleteResultRef>> const
                  &constant_arguments,
              absl::Span<ast::Call::Argument const> arg_exprs,
              absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  auto [callee_fn, overload_type, defaults, context] =
      EmitCallee(c, callee, constant_arguments);
  // Arguments provided to a function call need to be "prepared" in the sense
  // that they need to be
  // * Ordered according to the parameters of the function (because named
  //   arguments may be out of order)
  // * Have any implicit conversions applied.
  //
  // Implicit conversions are tricky because we cannot first compute the values
  // and then apply conversions to them. This may work for conversions that take
  // a buffer-pointer and convert it to just a pointer, but some conversions
  // take values and convert them to pointers/references. If we first compute
  // the value, we may end up loading the value from memory and no longer having
  // access to its address. Or worse, we may have a temporary and never have an
  // allocated address for it.

  auto const &param_qts = overload_type->params();
  ir::PartialResultBuffer prepared_arguments;
  EmitArguments(c, param_qts, defaults, arg_exprs, constant_arguments,
                prepared_arguments);

  // TODO: With expansions, this might be wrong.
  auto out_params = SetReturns(c, overload_type, to);
  c.current_block()->Append(ir::CallInstruction(
      overload_type, callee_fn, std::move(prepared_arguments), out_params));
  int i = -1;
  for (type::Type t : overload_type->return_types()) {
    ++i;
    if (t.get()->is_big()) { continue; }

    ir::PartialResultBuffer buffer;
    buffer.append(out_params[i]);
    CopyAssignmentEmitter emitter(c);
    emitter(to[i], type::Typed(buffer[0], t));
  }
}

core::Arguments<type::Typed<ir::CompleteResultRef>> EmitConstantArguments(
    Compiler &c, absl::Span<ast::Call::Argument const> args,
    ir::CompleteResultBuffer &buffer) {
  core::Arguments<type::Typed<size_t>> indices;

  size_t i = 0;
  for (auto const &arg : args) {
    absl::Cleanup cleanup = [&] { ++i; };

    auto qt = c.context().qual_types(&arg.expr())[0];
    if (qt.constant()) {
      ASSIGN_OR(
          NOT_YET(),  //
          auto result,
          c.EvaluateToBufferOrDiagnose(
              type::Typed<ast::Expression const *>(&arg.expr(), qt.type())));
      buffer.append(result);
      if (not arg.named()) {
        indices.pos_emplace(
            type::Typed<size_t>(buffer.num_entries() - 1, qt.type()));
      } else {
        indices.named_emplace(
            arg.name(),
            type::Typed<size_t>(buffer.num_entries() - 1, qt.type()));
      }
    } else {
      buffer.append();
      if (not arg.named()) {
        indices.pos_emplace(
            type::Typed<size_t>(buffer.num_entries() - 1, qt.type()));
      } else {
        indices.named_emplace(
            arg.name(),
            type::Typed<size_t>(buffer.num_entries() - 1, qt.type()));
      }
    }
  }

  return indices.Transform([&](auto const &i) {
    return type::Typed<ir::CompleteResultRef>(buffer[*i], i.type());
  });
}

void EmitCast(Compiler &c, type::Typed<ast::Expression const *> node,
              type::Type to, ir::PartialResultBuffer &buffer) {
  c.EmitToBuffer(*node, buffer);
  EmitCast(c.current(), node.type(), to, buffer);
}

ir::PartialResultBuffer EmitCast(Compiler &c,
                                 type::Typed<ast::Expression const *> node,
                                 type::Type to) {
  ir::PartialResultBuffer buffer;
  EmitCast(c, node, to, buffer);
  return buffer;
}

}  // namespace compiler

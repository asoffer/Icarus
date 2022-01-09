#include "compiler/instantiate.h"

#include <string_view>
#include <utility>
#include <vector>

#include "base/debug.h"
#include "compiler/bound_parameters.h"
#include "compiler/common.h"
#include "compiler/common_diagnostics.h"
#include "compiler/compiler.h"
#include "compiler/module.h"
#include "core/params.h"
#include "ir/value/result_buffer.h"
#include "type/cast.h"
#include "type/type.h"

namespace compiler {
// TODO: Remove forward declaration.
absl::Span<type::QualType const> VerifyType(CompilationDataReference data,
                                            ast::Node const *node);

namespace {

type::Typed<ir::CompleteResultRef> const *ArgumentFromIndex(
    core::Arguments<type::Typed<ir::CompleteResultRef>> const &arguments,
    int index, std::string_view id) {
  if (index < arguments.pos().size()) { return &arguments[index]; }
  return arguments.at_or_null(id);
}

std::optional<type::Type> ComputeParameterTypeOrDiagnose(
    Compiler &c, ast::Declaration const *decl) {
  if (auto const *type_expr = decl->type_expr()) {
    auto type_expr_type = VerifyType(c, type_expr)[0].type();
    if (type_expr_type != type::Type_) {
      c.diag().Consume(
          NotAType{.view = SourceViewFor(type_expr), .type = type_expr_type});
      return std::nullopt;
    }

    return c.EvaluateOrDiagnoseAs<type::Type>(type_expr);
  } else {
    return VerifyType(c, decl->init_val())[0].type();
  }
}

std::optional<BoundParameters> ComputeParamsFromArgs(
    Compiler &c, ast::ParameterizedExpression const *node,
    core::Arguments<type::Typed<ir::CompleteResultRef>> const &args) {
  LOG("ComputeParamsFromArgs",
      "Creating a concrete implementation with arguments %s", args);

  std::vector<type::Type> argument_types(node->params().size());

  BoundParameters bound_parameters;

  for (auto [index, dep_node] : node->ordered_dependency_nodes()) {
    ASSERT(dep_node.node()->ids().size() == 1u);
    std::string_view id = dep_node.node()->ids()[0].name();
    LOG("ComputeParamsFromArgs", "Handling %s`%s` (index = %u)", dep_node, id,
        index);
    switch (dep_node.kind()) {
      case core::DependencyNodeKind::ArgumentType: {
        if (auto const *argument = ArgumentFromIndex(args, index, id)) {
          argument_types[index] = argument->type();
          c.context().set_arg_type(id, argument->type());
        } else if (auto const *default_value = dep_node.node()->init_val()) {
          type::Type t = VerifyType(c, default_value)[0].type();
          argument_types[index] = t;
          c.context().set_arg_type(id, t);
        } else {
          return std::nullopt;
        }
        LOG("ComputeParamsFromArgs", "... %s",
            argument_types[index].to_string());
      } break;
      case core::DependencyNodeKind::ArgumentValue: {
        if (not ArgumentFromIndex(args, index, id) and
            not dep_node.node()->init_val()) {
          return std::nullopt;
        }
      } break;
      case core::DependencyNodeKind::ParameterType: {
        ASSIGN_OR(return std::nullopt,  //
                         type::Type t,
                         ComputeParameterTypeOrDiagnose(c, dep_node.node()));

        auto qt = (dep_node.node()->flags() & ast::Declaration::f_IsConst)
                      ? type::QualType::Constant(t)
                      : type::QualType::NonConstant(t);

        LOG("ComputeParamsFromArgs", "... %s", qt.to_string());

        if (not type::CanCastImplicitly(argument_types[index], t)) {
          LOG("ComputeParamsFromArgs", "No cast %s -> %s",
              argument_types[index], t);
          NOT_YET("Log an error and bail out of this computation");
        }

        bound_parameters.bind_type(&dep_node.node()->ids()[0], qt);
      } break;
      case core::DependencyNodeKind::ParameterValue: {
        type::Typed<ir::CompleteResultBuffer> buffer;
        if (auto const *a = ArgumentFromIndex(args, index, id)) {
          buffer->append(**a);
          buffer.set_type(a->type());
        } else {
          auto const *default_value =
              ASSERT_NOT_NULL(dep_node.node()->init_val());
          type::Type default_value_type =
              c.context().qual_types(default_value)[0].type();
          ASSIGN_OR(NOT_YET("bail out of this computation"),  //
                    *buffer,
                    c.EvaluateToBufferOrDiagnose(
                        type::Typed(default_value, default_value_type)));
          buffer.set_type(default_value_type);
        }

        auto qt = bound_parameters.binding(&dep_node.node()->ids()[0]).qual_type();
        c.builder().ApplyImplicitCasts(buffer.type(), qt, *buffer);

        // TODO: Support multiple declarations
        if (not c.context().Constant(&dep_node.node()->ids()[0])) {
          c.context().SetConstant(&dep_node.node()->ids()[0], *buffer);
        }

        LOG("ComputeParamsFromArgs", "... %s",
            qt.type().Representation((*buffer)[0]));

        // TODO: We end up stashing arguments both in BoundParameters and in
        // the context's Constant map. We should probably only use the latter.
        bound_parameters.bind_value(&dep_node.node()->ids()[0], (*buffer)[0]);
      } break;
    }
  }
  return bound_parameters;
}

}  // namespace

std::optional<Context::InsertSubcontextResult> Instantiate(
    Compiler &c, ast::ParameterizedExpression const *node,
    core::Arguments<type::Typed<ir::CompleteResultRef>> const &arguments) {
  auto &ctx = ModuleFor(node)->as<CompiledModule>().context();
  LOG("Instantiate", "Instantiating %s: %s", node->DebugString(),
      ctx.DebugString());
  Context scratchpad            = ctx.ScratchpadSubcontext();
  CompilationData data{.context        = &scratchpad,
                       .work_resources = c.work_resources(),
                       .resources      = c.resources()};
  Compiler child(&data);

  ASSIGN_OR(return std::nullopt,  //
                   auto bound_params, ComputeParamsFromArgs(child, node, arguments));
  return ctx.InsertSubcontext(node, bound_params, std::move(scratchpad));
}

std::optional<Context::InsertSubcontextResult> Instantiate(
    Compiler &c, ast::ScopeLiteral const *node,
    ir::ScopeContext const &scope_context,
    core::Arguments<type::Typed<ir::CompleteResultRef>> const &arguments) {
  auto &ctx = ModuleFor(node)->as<CompiledModule>().context();
  LOG("Instantiate", "Instantiating %s: %s", node->DebugString(),
      ctx.DebugString());
  Context scratchpad            = ctx.ScratchpadSubcontext();
  CompilationData data{.context        = &scratchpad,
                       .work_resources = c.work_resources(),
                       .resources      = c.resources()};
  Compiler child(&data);

  ir::CompleteResultBuffer buffer;
  buffer.append(scope_context);

  ASSIGN_OR(return std::nullopt,  //
                   auto bound_parameters,
                   ComputeParamsFromArgs(child, node, arguments));
  bound_parameters.bind_type(&node->context().ids()[0],
                             type::QualType::Constant(type::ScopeContext));
  bound_parameters.bind_value(&node->context().ids()[0], buffer[0]);
  return ctx.InsertSubcontext(node, bound_parameters, std::move(scratchpad));
}

Context::FindSubcontextResult FindInstantiation(
    Compiler &c, ast::ParameterizedExpression const *node,
    core::Arguments<type::Typed<ir::CompleteResultRef>> const &arguments) {
  auto &ctx = ModuleFor(node)->as<CompiledModule>().context();
  LOG("FindInstantiation", "Finding %s: %s", node->DebugString(),
      ctx.DebugString());
  Context scratchpad            = ctx.ScratchpadSubcontext();
  CompilationData data{.context        = &scratchpad,
                       .work_resources = c.work_resources(),
                       .resources      = c.resources()};
  Compiler child(&data);
  return ctx.FindSubcontext(node,
                            *ComputeParamsFromArgs(child, node, arguments));
}

Context::FindSubcontextResult FindInstantiation(
    Compiler &c, ast::ScopeLiteral const *node,
    ir::ScopeContext const &scope_context,
    core::Arguments<type::Typed<ir::CompleteResultRef>> const &arguments) {
  auto &ctx = ModuleFor(node)->as<CompiledModule>().context();
  LOG("FindInstantiation", "Finding %s: %s", node->DebugString(),
      ctx.DebugString());
  Context scratchpad = ctx.ScratchpadSubcontext();
  CompilationData data{.context        = &scratchpad,
                       .work_resources = c.work_resources(),
                       .resources      = c.resources()};
  Compiler child(&data);

  ir::CompleteResultBuffer buffer;
  buffer.append(scope_context);

  BoundParameters bound_parameters =
      *ComputeParamsFromArgs(child, node, arguments);
  bound_parameters.bind_type(&node->context().ids()[0],
                             type::QualType::Constant(type::ScopeContext));
  bound_parameters.bind_value(&node->context().ids()[0], buffer[0]);
  return ctx.FindSubcontext(node, bound_parameters);
}

}  // namespace compiler

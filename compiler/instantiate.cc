#include "compiler/instantiate.h"

#include <string_view>
#include <utility>
#include <vector>

#include "base/debug.h"
#include "compiler/bound_parameters.h"
#include "compiler/compiler.h"
#include "compiler/module.h"
#include "core/params.h"
#include "ir/value/result_buffer.h"
#include "type/type.h"

namespace compiler {
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
    auto type_expr_type = c.VerifyType(type_expr)[0].type();
    if (type_expr_type != type::Type_) {
      c.diag().Consume(
          NotAType{.range = type_expr->range(), .type = type_expr_type});
      NOT_YET("Exit out of this computation.");
    }

    return c.EvaluateOrDiagnoseAs<type::Type>(type_expr);
  } else {
    return c.VerifyType(decl->init_val())[0].type();
  }
}

BoundParameters ComputeParamsFromArgs(
    Compiler &c, ast::ParameterizedExpression const *node,
    core::Arguments<type::Typed<ir::CompleteResultRef>> const &args) {
  LOG("ComputeParamsFromArgs", "Creating a concrete implementation with %s",
      args.Transform([](auto const &a) { return a.type().to_string(); }));

  std::vector<ir::CompleteResultBuffer> buffers(node->params().size());
  std::vector<core::Param<type::QualType>> qts(node->params().size());
  std::vector<type::Type> argument_types(node->params().size());

  for (auto [index, dep_node] : node->ordered_dependency_nodes()) {
    ASSERT(dep_node.node()->ids().size() == 1u);
    std::string_view id = dep_node.node()->ids()[0].name();
    LOG("ComputeParamsFromArgs", "Handling dep-node %s`%s` (index = %u)",
        ToString(dep_node.kind()), id, index);
    switch (dep_node.kind()) {
      case core::DependencyNodeKind::ArgValue: {
        ir::CompleteResultBuffer buffer;
        ir::CompleteResultRef value;
        if (auto const *argument = ArgumentFromIndex(args, index, id)) {
          value = **argument;
        } else {
          auto const *init_val = ASSERT_NOT_NULL(dep_node.node()->init_val());
          type::Type t         = c.context().arg_type(id);
          ASSIGN_OR(NOT_YET("bail out of this computation)"),  //
                    buffer,
                    c.EvaluateToBufferOrDiagnose(type::Typed(init_val, t)));
          value = buffer[0];
        }

        LOG("ComputeParamsFromArgs", "... %s",
            c.context().arg_type(id).Representation(value));
      } break;
      case core::DependencyNodeKind::ArgType: {
        auto const *argument      = ArgumentFromIndex(args, index, id);
        auto const *initial_value = dep_node.node()->init_val();
        type::Type arg_type =
            argument ? argument->type()
                     : c.VerifyType(ASSERT_NOT_NULL(initial_value))[0].type();
        argument_types[index] = arg_type;
        c.context().set_arg_type(id, arg_type);
        LOG("ComputeParamsFromArgs", "... %s", arg_type.to_string());
      } break;
      case core::DependencyNodeKind::ParamType: {
        ASSIGN_OR(NOT_YET("bail out of this computation"),  //
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

        qts[index] =
            core::Param<type::QualType>(id, qt, node->params()[index].flags);
      } break;
      case core::DependencyNodeKind::ParamValue: {
        // Find the argument associated with this parameter.
        // TODO, if the type is wrong but there is an implicit cast, deal with
        // that.
        type::Typed<ir::CompleteResultRef> argument;
        if (auto const *a = ArgumentFromIndex(args, index, id)) {
          argument = *a;
        } else {
          auto t = c.context().qual_types(dep_node.node())[0].type();
          ASSIGN_OR(NOT_YET("bail out of this computation"),  //
                    auto result,
                    c.EvaluateToBufferOrDiagnose(type::Typed(
                        ASSERT_NOT_NULL(dep_node.node()->init_val()), t)));
          argument = type::Typed(result[0], t);
          LOG("ComputeParamsFromArgs", "%s", dep_node.node()->DebugString());
        }

        // TODO: Support multiple declarations
        if (not c.context().Constant(&dep_node.node()->ids()[0])) {
          // TODO complete?
          // TODO: Support multiple declarations
          c.context().SetConstant(&dep_node.node()->ids()[0], *argument);
        }

        LOG("ComputeParamsFromArgs", "... %s", *argument);

        // TODO: We end up stashing arguments both in BoundParameters and in
        // the context's Constant map. We should probably only use the latter.
        buffers[index].append(*argument);
      } break;
    }
  }
  return BoundParameters(std::move(qts), buffers);
}

}  // namespace

Context::InsertSubcontextResult Instantiate(
    Compiler &c, ast::ParameterizedExpression const *node,
    core::Arguments<type::Typed<ir::CompleteResultRef>> const &args) {
  auto &ctx = node->scope()
                  ->Containing<ast::ModuleScope>()
                  ->module()
                  ->as<CompiledModule>()
                  .context();
  LOG("Instantiate", "Instantiating %s: %s", node->DebugString(),
      ctx.DebugString());
  Context scratchpad            = ctx.ScratchpadSubcontext();
  PersistentResources resources = c.resources();
  Compiler child(&scratchpad, resources);

  return ctx.InsertSubcontext(node, ComputeParamsFromArgs(child, node, args),
                              std::move(scratchpad));
}

Context::FindSubcontextResult FindInstantiation(
    Compiler &c, ast::ParameterizedExpression const *node,
    core::Arguments<type::Typed<ir::CompleteResultRef>> const &args) {
  auto &ctx = node->scope()
                  ->Containing<ast::ModuleScope>()
                  ->module()
                  ->as<CompiledModule>()
                  .context();
  LOG("FindInstantiation", "Finding %s: %s", node->DebugString(),
      ctx.DebugString());
  Context scratchpad = ctx.ScratchpadSubcontext();
  PersistentResources resources = c.resources();
  Compiler child(&scratchpad, resources);

  return ctx.FindSubcontext(node, ComputeParamsFromArgs(child, node, args));
}

}  // namespace compiler

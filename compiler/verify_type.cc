#include "absl/algorithm/container.h"

#include <iostream>
#include <optional>
#include <string_view>

#include "ast/ast.h"
#include "base/defer.h"
#include "compiler/compiler.h"
#include "compiler/dispatch/parameters_and_arguments.h"
#include "compiler/extract_jumps.h"
#include "compiler/library_module.h"
#include "core/call.h"
#include "diagnostic/errors.h"
#include "frontend/lex/operators.h"
#include "ir/interpretter/evaluate.h"
#include "ir/compiled_fn.h"
#include "ir/value/generic_fn.h"
#include "type/cast.h"
#include "type/generic_function.h"
#include "type/generic_struct.h"
#include "type/jump.h"
#include "type/overload_set.h"
#include "type/parameter_pack.h"
#include "type/qual_type.h"
#include "type/type.h"
#include "type/typed_value.h"
#include "type/util.h"

namespace compiler {

// TODO there's not that much shared between the inferred and uninferred
// cases, so probably break them out.
type::QualType VerifyBody(Compiler *c, ast::FunctionLiteral const *node,
                          type::Type const *t = nullptr) {
  if (not t) { t = ASSERT_NOT_NULL(c->type_of(node)); }
  auto const &fn_type = t->as<type::Function>();
  for (auto const *stmt : node->stmts()) { c->VerifyType(stmt); }

  // TODO we can have yields and returns, or yields and jumps, but not jumps
  // and returns. Check this.
  absl::flat_hash_set<type::Type const *> types;
  absl::flat_hash_map<ast::ReturnStmt const *, type::Type const *>
      saved_ret_types;
  for (auto const *n : c->data().extraction_map_[node]) {
    if (auto const *ret_node = n->if_as<ast::ReturnStmt>()) {
      std::vector<type::Type const *> ret_types;
      for (auto const *expr : ret_node->exprs()) {
        ret_types.push_back(ASSERT_NOT_NULL(c->type_of(expr)));
      }
      auto *t = type::Tup(std::move(ret_types));
      types.emplace(t);

      saved_ret_types.emplace(ret_node, t);
    } else {
      UNREACHABLE();  // TODO
    }
  }

  auto type_params = node->params().Transform([&](auto const &param) {
    auto maybe_qt = c->qual_type_of(param.get());
    ASSERT(maybe_qt != std::nullopt);
    return *maybe_qt;
  });

  if (not node->outputs()) {
    std::vector<type::Type const *> output_type_vec(
        std::make_move_iterator(types.begin()),
        std::make_move_iterator(types.end()));

    if (types.size() > 1) { NOT_YET("log an error"); }
    auto f = type::Func(std::move(type_params), std::move(output_type_vec));
    return c->data().set_qual_type(node, type::QualType::Constant(f));

  } else {
    auto *node_type = t;
    auto outs       = ASSERT_NOT_NULL(node_type)->as<type::Function>().output();
    switch (outs.size()) {
      case 0: {
        bool err = false;
        for (auto *n : c->data().extraction_map_[node]) {
          if (auto *ret_node = n->if_as<ast::ReturnStmt>()) {
            if (not ret_node->exprs().empty()) {
              c->diag().Consume(diagnostic::NoReturnTypes{
                  .range = ret_node->range(),
              });
              err = true;
            }
          } else {
            UNREACHABLE();  // TODO
          }
        }
        return err ? type::QualType::Error()
                   : type::QualType::Constant(node_type);
      } break;
      case 1: {
        bool err = false;
        for (auto *n : c->data().extraction_map_[node]) {
          if (auto *ret_node = n->if_as<ast::ReturnStmt>()) {
            auto *t = ASSERT_NOT_NULL(saved_ret_types[ret_node]);
            if (t == outs[0]) { continue; }
            c->diag().Consume(diagnostic::ReturnTypeMismatch{
                .actual   = t,
                .expected = outs[0],
                .range    = ret_node->range(),
            });
            err = true;
          } else {
            UNREACHABLE();  // TODO
          }
        }
        return err ? type::QualType::Error()
                   : type::QualType::Constant(node_type);
      } break;
      default: {
        for (auto *n : c->data().extraction_map_[node]) {
          if (auto *ret_node = n->if_as<ast::ReturnStmt>()) {
            auto *expr_type = ASSERT_NOT_NULL(saved_ret_types[ret_node]);
            if (expr_type->is<type::Tuple>()) {
              auto const &tup_entries = expr_type->as<type::Tuple>().entries_;
              if (tup_entries.size() != outs.size()) {
                c->diag().Consume(diagnostic::ReturningWrongNumber{
                    .actual   = (expr_type->is<type::Tuple>()
                                   ? expr_type->as<type::Tuple>().size()
                                   : 1),
                    .expected = outs.size(),
                    .range    = ret_node->range(),
                });
                return type::QualType::Error();
              } else {
                bool err = false;
                for (size_t i = 0; i < tup_entries.size(); ++i) {
                  if (tup_entries[i] != outs[i]) {
                    // TODO if this is a commalist we can point to it more
                    // carefully but if we're just passing on multiple return
                    // values it's harder.
                    //
                    // TODO point the span to the correct entry which may be
                    // hard if it's splatted.
                    c->diag().Consume(diagnostic::IndexedReturnTypeMismatch{
                        .index    = i,
                        .actual   = outs[i],
                        .expected = tup_entries[i],
                        .range    = ret_node->range(),
                    });
                    err = true;
                  }
                }
                if (err) { return type::QualType::Error(); }
              }
            } else {
              c->diag().Consume(diagnostic::ReturningWrongNumber{
                  .actual   = (expr_type->is<type::Tuple>()
                                 ? expr_type->as<type::Tuple>().size()
                                 : 1),
                  .expected = outs.size(),
                  .range    = ret_node->range(),
              });
              return type::QualType::Error();
            }
          } else {
            UNREACHABLE();  // TODO
          }
        }
        return type::QualType::Constant(node_type);
      } break;
    }
  }
}

bool Compiler::VerifyBody(ast::FunctionLiteral const *node) {
  DEBUG_LOG("function")
  ("function-literal body verification: ", node->DebugString(), " ", &data());
  auto const &fn_type =
      ASSERT_NOT_NULL(data().qual_type(node))->type()->as<type::Function>();
  for (auto const &param : fn_type.params()) {
    if (not true) {
      DEBUG_LOG("function")("rescheduled");
      data().ClearVerifyBody(node);

      DEBUG_LOG("compile-work-queue")
      ("Request work: ",
       static_cast<int>(TransientFunctionState::WorkType::VerifyBody), ": ",
       node);
      state_.work_queue.emplace(node,
                                TransientFunctionState::WorkType::VerifyBody);
      return true;
    }
  }

  return ::compiler::VerifyBody(this, node, data().qual_type(node)->type())
      .ok();
}

std::optional<core::Params<type::QualType>> VerifyParams(
    Compiler *c,
    core::Params<std::unique_ptr<ast::Declaration>> const &params) {
  // Parameter types cannot be dependent in concrete implementations so it is
  // safe to verify each of them separately (to generate more errors that are
  // likely correct).

  DEBUG_LOG("function")("got here");
  core::Params<type::QualType> type_params;
  type_params.reserve(params.size());
  bool err = false;
  for (auto &d : params) {
    auto qt = c->VerifyType(d.value.get());
    if (qt.ok()) {
      type_params.append(d.name, qt, d.flags);
    } else {
      err = true;
    }
  }
  DEBUG_LOG("function")("got here", err);
  if (err) { return std::nullopt; }
  return type_params;
}

type::QualType Compiler::VerifyConcreteFnLit(ast::FunctionLiteral const *node) {
  DEBUG_LOG("function")("Starting function-literal verification: ", node);

  ASSIGN_OR(
      {
        DEBUG_LOG("function")("Bailing due to errors");
        return type::QualType::Error();
      },  //
      auto params, VerifyParams(this, node->params()));

  std::vector<type::Type const *> output_type_vec;
  bool error   = false;
  auto outputs = node->outputs();
  if (outputs) {
    output_type_vec.reserve(outputs->size());
    for (auto *output : *outputs) {
      auto result = VerifyType(output);
      output_type_vec.push_back(result.type());
      if (result.type() != nullptr and not result.constant()) {
        // TODO this feels wrong because output could be a decl. And that decl
        // being a const decl isn't what I care about.
        NOT_YET("log an error");
        error = true;
      }
    }
  }

  if (error or absl::c_any_of(output_type_vec, [](type::Type const *t) {
        return t == nullptr;
      })) {
    DEBUG_LOG("function")("Bailing due to errors");
    return type::QualType::Error();
  }

  // TODO need a better way to say if there was an error recorded in a
  // particular section of compilation. Right now we just have the grad total
  // count.
  if (diag().num_consumed() > 0) {
    DEBUG_LOG("function")("Bailing due to errors");
    return type::QualType::Error();
  }

  if (outputs) {
    for (size_t i = 0; i < output_type_vec.size(); ++i) {
      if (auto *decl = (*outputs)[i]->if_as<ast::Declaration>()) {
        output_type_vec[i] = type_of(decl);
      } else {
        ASSERT(output_type_vec[i] == type::Type_);
        auto maybe_type = EvaluateAs<type::Type const *>((*outputs)[i]);
        if (not maybe_type) { NOT_YET(); }
        output_type_vec[i] = *maybe_type;
      }
    }

    DEBUG_LOG("compile-work-queue")
    ("Request work: ",
     static_cast<int>(TransientFunctionState::WorkType::VerifyBody), ": ",
     node);
    state_.work_queue.emplace(node,
                              TransientFunctionState::WorkType::VerifyBody);
    auto qt = type::QualType::Constant(
        type::Func(std::move(params), std::move(output_type_vec)));
    DEBUG_LOG("function")
    ("Setting function-literal type: ", node->DebugString(), " ", qt, " ",
     &data());
    return data().set_qual_type(node, qt);
  } else {
    DEBUG_LOG("function")("Setting function-literal type");
    return data().set_qual_type(node, ::compiler::VerifyBody(this, node));
  }
}

static std::optional<type::Quals> VerifyAndGetQuals(
    Compiler *v, base::PtrSpan<ast::Expression const> exprs) {
  bool err          = false;
  type::Quals quals = type::Quals::All();
  for (auto *expr : exprs) {
    auto r = v->VerifyType(expr);
    err |= not r.ok();
    if (not err) { quals &= r.quals(); }
  }
  if (err) { return std::nullopt; }
  return quals;
}

type::QualType Compiler::VerifyType(ast::ArgumentType const *node) {
  return data().set_qual_type(node, type::QualType::Constant(type::Type_));
}

bool Compiler::VerifyBody(ast::BlockLiteral const *node) {
  bool success = true;
  // TODO consider not verifying the types of the bodies. They almost certainly
  // contain circular references in the jump statements, and if the functions
  // require verifying the body upfront, things can maybe go wrong?
  for (auto *b : node->before()) { success &= VerifyType(b).ok(); }
  for (auto *a : node->after()) { success &= VerifyType(a).ok(); }
  return success;
}

type::QualType Compiler::VerifyType(ast::BlockLiteral const *node) {
  DEBUG_LOG("compile-work-queue")
  ("Request work: ",
   static_cast<int>(TransientFunctionState::WorkType::VerifyBody), ": ", node);
  state_.work_queue.emplace(node, TransientFunctionState::WorkType::VerifyBody);
  return data().set_qual_type(node, type::QualType::Constant(type::Block));
}

type::QualType Compiler::VerifyType(ast::BlockNode const *node) {
  for (auto &param : node->params()) { VerifyType(param.value.get()); }
  for (auto *stmt : node->stmts()) { VerifyType(stmt); }
  return data().set_qual_type(node, type::QualType::Constant(type::Block));
}

type::QualType Compiler::VerifyType(ast::BuiltinFn const *node) {
  return data().set_qual_type(node,
                              type::QualType::Constant(node->value().type()));
}

type::QualType Compiler::VerifyType(ast::Cast const *node) {
  auto expr_qual_type = VerifyType(node->expr());
  auto type_result    = VerifyType(node->type());
  if (not expr_qual_type.ok() or not type_result.ok()) {
    return type::QualType::Error();
  }

  if (type_result.type() != type::Type_) {
    diag().Consume(diagnostic::CastToNonType{
        .range = node->range(),
    });
    return type::QualType::Error();
  }
  if (not type_result.constant()) {
    diag().Consume(diagnostic::CastToNonConstantType{
        .range = node->range(),
    });
    return type::QualType::Error();
  }

  auto maybe_type = EvaluateAs<type::Type const *>(node->type());
  if (not maybe_type) { NOT_YET(); }
  auto const *t = *maybe_type;
  if (t->is<type::Struct>()) {
    // TODO: do you ever want to support overlaods that accepts constants?
    return data().set_qual_type(
        node, VerifyUnaryOverload(
                  "as", node, type::Typed(ir::Value(), expr_qual_type.type())));
  } else {
    if (not type::CanCast(expr_qual_type.type(), t)) {
      diag().Consume(diagnostic::InvalidCast{
          .from  = expr_qual_type.type(),
          .to    = t,
          .range = node->range(),
      });
      NOT_YET("log an error", expr_qual_type.type(), t);
    }

    return data().set_qual_type(node,
                                type::QualType(t, expr_qual_type.quals()));
  }
}

bool Compiler::VerifyBody(ast::EnumLiteral const *node) {
  bool success = true;
  for (auto const &elem : node->elems()) {
    if (auto *decl = elem->if_as<ast::Declaration>()) {
      auto *t = VerifyType(decl->init_val()).type();
      ASSERT(type::IsIntegral(t) == true);
      success = type::IsIntegral(t);
      // TODO determine what is allowed here and how to generate errors.
    }
  }
  return success;
}

type::QualType Compiler::VerifyType(ast::EnumLiteral const *node) {
  DEBUG_LOG("compile-work-queue")
  ("Request work: ",
   static_cast<int>(TransientFunctionState::WorkType::VerifyBody), ": ", node);
  state_.work_queue.emplace(node, TransientFunctionState::WorkType::VerifyBody);
  return data().set_qual_type(node, type::QualType::Constant(type::Type_));
}

std::vector<std::pair<int, core::DependencyNode<ast::Declaration>>>
OrderedDependencyNodes(ast::ParameterizedExpression const *node, bool all) {
  absl::flat_hash_set<core::DependencyNode<ast::Declaration>> deps;
  for (auto const &p : node->params()) {
    deps.insert(
        core::DependencyNode<ast::Declaration>::MakeArgType(p.value.get()));
    deps.insert(
        core::DependencyNode<ast::Declaration>::MakeType(p.value.get()));
    if (all or (p.value->flags() & ast::Declaration::f_IsConst)) {
      deps.insert(
          core::DependencyNode<ast::Declaration>::MakeValue(p.value.get()));
      deps.insert(
          core::DependencyNode<ast::Declaration>::MakeArgValue(p.value.get()));
    }
  }

  std::vector<std::pair<int, core::DependencyNode<ast::Declaration>>>
      ordered_nodes;
  node->parameter_dependency_graph().topologically([&](auto dep_node) {
    if (not deps.contains(dep_node)) { return; }
    DEBUG_LOG("generic-fn")
    ("adding ", ToString(dep_node.kind()), "`", dep_node.node()->id(), "`");
    ordered_nodes.emplace_back(0, dep_node);
  });

  // Compute and set the index or `ordered_nodes` so that each node knows the
  // ordering in source code. This allows us to match parameters to arguments
  // efficiently.
  absl::flat_hash_map<ast::Declaration const *, int> param_index;
  int index = 0;
  for (auto const &param : node->params()) {
    param_index.emplace(param.value.get(), index++);
  }

  for (auto &[index, node] : ordered_nodes) {
    index = param_index.find(node.node())->second;
  }

  return ordered_nodes;
}

// TODO: There's something strange about this: We want to work on a temporary
// data/compiler, but using `this` makes it feel more permanent.
core::Params<std::pair<ir::Value, type::QualType>>
Compiler::ComputeParamsFromArgs(
    ast::ParameterizedExpression const *node,
    absl::Span<std::pair<int, core::DependencyNode<ast::Declaration>> const>
        ordered_nodes,
    core::FnArgs<type::Typed<ir::Value>> const &args) {
  ConstantBinding constants;
  DEBUG_LOG("generic-fn")
  ("Creating a concrete implementation with ",
   args.Transform([](auto const &a) { return a.type()->to_string(); }));

  core::Params<std::pair<ir::Value, type::QualType>> parameters(
      node->params().size());

  auto tostr = [](ir::Value v) {
    if (auto **t = v.get_if<type::Type const *>()) {
      return (*t)->to_string();
    } else {
      std::stringstream ss;
      ss << v;
      return ss.str();
    }
  };

  // TODO use the proper ordering.
  for (auto [index, dep_node] : ordered_nodes) {
    DEBUG_LOG("generic-fn")
    ("Handling dep-node ", ToString(dep_node.kind()), "`",
     dep_node.node()->id(), "`");
    switch (dep_node.kind()) {
      case core::DependencyNodeKind::ArgValue: {
        ir::Value val;
        if (index < args.pos().size()) {
          val = *args[index];
        } else if (auto const *a = args.at_or_null(dep_node.node()->id())) {
          val = **a;
        } else {
          auto const *init_val = ASSERT_NOT_NULL(dep_node.node()->init_val());
          type::Type const *t =
              ASSERT_NOT_NULL(data().arg_type(dep_node.node()->id()));
          auto maybe_val = Evaluate(type::Typed(init_val, t));
          if (not maybe_val) { NOT_YET(); }
          val = *maybe_val;
        }

        // Erase values not known at compile-time.
        if (val.get_if<ir::Reg>()) { val = ir::Value(); }

        DEBUG_LOG("generic-fn")("... ", tostr(val));
        data().set_arg_value(dep_node.node()->id(), val);
      } break;
      case core::DependencyNodeKind::ArgType: {
        type::Type const *arg_type = nullptr;
        if (index < args.pos().size()) {
          arg_type = args[index].type();
        } else if (auto const *a = args.at_or_null(dep_node.node()->id())) {
          arg_type = a->type();
        } else {
          // TODO: What if this is a bug and you don't have an initial value?
          auto *init_val = ASSERT_NOT_NULL(dep_node.node()->init_val());
          arg_type       = VerifyType(init_val).type();
        }
        DEBUG_LOG("generic-fn")("... ", *arg_type);
        data().set_arg_type(dep_node.node()->id(), arg_type);
      } break;
      case core::DependencyNodeKind::ParamType: {
        type::Type const *t = nullptr;
        if (auto const *type_expr = dep_node.node()->type_expr()) {
          auto type_expr_type = VerifyType(type_expr).type();
          if (type_expr_type != type::Type_) {
            NOT_YET("log an error: ", type_expr->DebugString(), ": ",
                    type_expr_type);
          }

          auto maybe_type = EvaluateAs<type::Type const *>(type_expr);
          if (not maybe_type) { NOT_YET(); }
          t = ASSERT_NOT_NULL(*maybe_type);
        } else {
          t = VerifyType(dep_node.node()->init_val()).type();
        }

        auto qt = (dep_node.node()->flags() & ast::Declaration::f_IsConst)
                      ? type::QualType::Constant(t)
                      : type::QualType::NonConstant(t);

        // TODO: Once a parameter type has been computed, we know it's
        // argument type has already been computed so we can verify that the
        // implicit casts are allowed.
        DEBUG_LOG("generic-fn")("... ", qt);
        size_t i =
            *ASSERT_NOT_NULL(node->params().at_or_null(dep_node.node()->id()));
        parameters.set(
            i, core::Param<std::pair<ir::Value, type::QualType>>(
                   dep_node.node()->id(), std::make_pair(ir::Value(), qt),
                   node->params()[i].flags));
      } break;
      case core::DependencyNodeKind::ParamValue: {
        // Find the argument associated with this parameter.
        // TODO, if the type is wrong but there is an implicit cast, deal with
        // that.
        type::Typed<ir::Value> arg;
        if (index < args.pos().size()) {
          arg = args[index];
          DEBUG_LOG("generic-fn")(tostr(*arg), " ", *arg.type());
        } else if (auto const *a = args.at_or_null(dep_node.node()->id())) {
          arg = *a;
        } else {
          auto const *t  = ASSERT_NOT_NULL(type_of(dep_node.node()));
          auto maybe_val = Evaluate(
              type::Typed(ASSERT_NOT_NULL(dep_node.node()->init_val()), t));
          if (not maybe_val) { NOT_YET(); }
          arg = type::Typed<ir::Value>(*maybe_val, t);
          DEBUG_LOG("generic-fn")(dep_node.node()->DebugString());
        }

        if (not data().Constant(dep_node.node())) {
          // TODO complete?
          data().SetConstant(dep_node.node(), *arg);
        }

        size_t i =
            *ASSERT_NOT_NULL(node->params().at_or_null(dep_node.node()->id()));
        parameters[i].value.first = *arg;
      } break;
    }
  }
  return parameters;
}

DependentComputedData::InsertDependentResult MakeConcrete(
    ast::ParameterizedExpression const *node, CompiledModule *mod,
    absl::Span<std::pair<int, core::DependencyNode<ast::Declaration>> const>
        ordered_nodes,
    core::FnArgs<type::Typed<ir::Value>> const &args,
    DependentComputedData &compiler_data,
    diagnostic::DiagnosticConsumer &diag) {
  DependentComputedData temp_data(mod);
  Compiler c({
      .builder             = ir::GetBuilder(),
      .data                = temp_data,
      .diagnostic_consumer = diag,
  });
  temp_data.parent_ = &compiler_data;

  auto parameters = c.ComputeParamsFromArgs(node, ordered_nodes, args);
  return compiler_data.InsertDependent(node, parameters);
}

type::QualType Compiler::VerifyType(ast::FunctionLiteral const *node) {
  if (not node->is_generic()) { return VerifyConcreteFnLit(node); }

  auto ordered_nodes = OrderedDependencyNodes(node);

  auto *diag_consumer = &diag();
  auto gen            = [node, compiler_data = &data(), diag_consumer,
              ordered_nodes(std::move(ordered_nodes))](
                 core::FnArgs<type::Typed<ir::Value>> const &args) mutable
      -> type::Function const * {
    auto [params, rets, data, inserted] =
        MakeConcrete(node, &compiler_data->module(), ordered_nodes, args,
                     *compiler_data, *diag_consumer);
    if (inserted) {
      Compiler c({
          .builder             = ir::GetBuilder(),
          .data                = data,
          .diagnostic_consumer = *diag_consumer,
      });

      if (auto outputs = node->outputs(); outputs and not outputs->empty()) {
        for (auto const *o : *outputs) {
          auto qt = c.VerifyType(o);
          ASSERT(qt == type::QualType::Constant(type::Type_));
          auto maybe_type = c.EvaluateAs<type::Type const *>(o);
          if (not maybe_type) { NOT_YET(); }
          rets.push_back(ASSERT_NOT_NULL(*maybe_type));
        }
      }
    }

    type::Function const *ft = type::Func(
        params.Transform([](auto const &p) { return p.second; }), rets);
    data.set_qual_type(node, type::QualType::Constant(ft));
    return ft;
  };

  return data().set_qual_type(
      node, type::QualType::Constant(new type::GenericFunction(
                node->params().Transform([](auto const &p) {
                  return type::GenericFunction::EmptyStruct{};
                }),
                std::move(gen))));
}

type::QualType Compiler::VerifyType(ast::ShortFunctionLiteral const *node) {
  if (not node->is_generic()) {
    ASSIGN_OR(return type::QualType::Error(),  //
                     auto params, VerifyParams(this, node->params()));
    ASSIGN_OR(return _, auto body_qt, VerifyType(node->body()));
    return data().set_qual_type(
        node, type::QualType::Constant(
                  type::Func(std::move(params), {body_qt.type()})));
  }

  auto ordered_nodes = OrderedDependencyNodes(node);

  auto *diag_consumer = &diag();
  auto gen            = [node, compiler_data = &data(), diag_consumer,
              ordered_nodes(std::move(ordered_nodes))](
                 core::FnArgs<type::Typed<ir::Value>> const &args) mutable
      -> type::Function const * {
    // TODO handle compilation failures.
    auto [params, rets, data, inserted] =
        MakeConcrete(node, &compiler_data->module(), ordered_nodes, args,
                     *compiler_data, *diag_consumer);

    auto body_qt = Compiler({.builder             = ir::GetBuilder(),
                             .data                = data,
                             .diagnostic_consumer = *diag_consumer})
                       .VerifyType(node->body());
    rets = {body_qt.type()};
    return type::Func(params.Transform([](auto const &p) { return p.second; }),
                      rets);
  };

  return data().set_qual_type(
      node, type::QualType::Constant(new type::GenericFunction(
                node->params().Transform([](auto const &p) {
                  return type::GenericFunction::EmptyStruct{};
                }),
                std::move(gen))));
}

type::QualType Compiler::VerifyType(ast::Import const *node) {
  DEBUG_LOG("Import")(node->DebugString());
  ASSIGN_OR(return _, auto result, VerifyType(node->operand()));
  bool err = false;
  if (result.type() != type::ByteView) {
    // TODO allow (import) overload
    diag().Consume(diagnostic::InvalidImport{
        .range = node->operand()->range(),
    });
    err = true;
  }

  if (not result.constant()) {
    diag().Consume(diagnostic::NonConstantImport{
        .range = node->operand()->range(),
    });
    err = true;
  }

  if (err) { return type::QualType::Error(); }

  auto maybe_src = EvaluateAs<ir::String>(node->operand());
  if (not maybe_src) { NOT_YET(); }

  auto canonical_file_name =
      frontend::CanonicalFileName::Make(frontend::FileName(maybe_src->get()));
  if (auto *mod = ImportLibraryModule(canonical_file_name)) {
    data().set_imported_module(node, mod);
    return data().set_qual_type(node, type::QualType::Constant(type::Module));
  } else {
    return type::QualType::Error();
  }
}

type::QualType Compiler::VerifyType(ast::ConditionalGoto const *node) {
  VerifyType(node->condition());
  for (auto const &option : node->true_options()) {
    for (auto const &expr : option.args()) { VerifyType(expr.get()); }
  }
  for (auto const &option : node->false_options()) {
    for (auto const &expr : option.args()) { VerifyType(expr.get()); }
  }
  return type::QualType::Constant(type::Void());
}

type::QualType Compiler::VerifyType(ast::UnconditionalGoto const *node) {
  for (auto const &option : node->options()) {
    for (auto const &expr : option.args()) { VerifyType(expr.get()); }
  }
  return type::QualType::Constant(type::Void());
}

type::QualType Compiler::VerifyType(ast::Label const *node) {
  return data().set_qual_type(node, type::QualType::Constant(type::Label));
}

bool Compiler::VerifyBody(ast::Jump const *node) {
  bool success = true;
  for (auto const *stmt : node->stmts()) { success &= VerifyType(stmt).ok(); }
  return success;
}

type::QualType Compiler::VerifyType(ast::Jump const *node) {
  DEBUG_LOG("Jump")(node->DebugString());

  bool err                = false;
  type::Type const *state = nullptr;

  if (node->state()) {
    auto state_qual_type = VerifyType(node->state());
    err                  = not state_qual_type.ok();
    if (not err) { state = state_qual_type.type(); }
  }

  core::Params<type::Type const *> param_types =
      node->params().Transform([&](auto const &param) {
        auto v = VerifyType(param.get());
        err |= not v.ok();
        return v.type();
      });

  DEBUG_LOG("compile-work-queue")
  ("Request work: ",
   static_cast<int>(TransientFunctionState::WorkType::VerifyBody), ": ", node);
  state_.work_queue.emplace(node, TransientFunctionState::WorkType::VerifyBody);
  return data().set_qual_type(
      node, err ? type::QualType::Error()
                : type::QualType::Constant(type::Jmp(state, param_types)));
}

type::QualType Compiler::VerifyType(ast::ReturnStmt const *node) {
  ASSIGN_OR(return type::QualType::Error(),  //
                   auto quals, VerifyAndGetQuals(this, node->exprs()));
  return type::QualType(type::Void(), quals);
}

type::QualType Compiler::VerifyType(ast::YieldStmt const *node) {
  ASSIGN_OR(return type::QualType::Error(),  //
                   auto quals, VerifyAndGetQuals(this, node->exprs()));
  return type::QualType(type::Void(), quals);
}

type::QualType Compiler::VerifyType(ast::ScopeNode const *node) {
  DEBUG_LOG("ScopeNode")(node->DebugString());
  ASSIGN_OR(return type::QualType::Error(),  //
                   std::ignore, VerifyFnArgs(node->args()));
  for (auto const &block : node->blocks()) { VerifyType(&block); }
  // TODO hack. Set this for real.
  return data().set_qual_type(node, type::QualType::NonConstant(type::Void()));
}

bool Compiler::VerifyBody(ast::ParameterizedStructLiteral const *node) {
  // NOT_YET();
  return false;
}

type::QualType Compiler::VerifyType(
    ast::ParameterizedStructLiteral const *node) {
  auto ordered_nodes  = OrderedDependencyNodes(node, true);
  auto *diag_consumer = &diag();
  auto gen            = [node, compiler_data = &data(), diag_consumer,
              ordered_nodes(std::move(ordered_nodes))](
                 core::FnArgs<type::Typed<ir::Value>> const &args) mutable
      -> type::Struct const * {
    // TODO: Need a version of MakeConcrete that doesn't generate return types
    // because those only make sense for functions.
    auto [params, rets, data, inserted] =
        MakeConcrete(node, &compiler_data->module(), ordered_nodes, args,
                     *compiler_data, *diag_consumer);
    if (inserted) {
      Compiler c({
          .builder             = ir::GetBuilder(),
          .data                = data,
          .diagnostic_consumer = *diag_consumer,
      });

      type::Struct *s = new type::Struct(
          &c.data().module(),
          {.is_copyable = not node->contains_hashtag(
               ast::Hashtag(ast::Hashtag::Builtin::Uncopyable)),
           .is_movable = not node->contains_hashtag(
               ast::Hashtag(ast::Hashtag::Builtin::Immovable))});

      DEBUG_LOG("struct")
      ("Allocating a new (parameterized) struct ", s, " for ", node);
      c.data().set_struct(node, s);
      for (auto const &field : node->fields()) { c.VerifyType(&field); }

      // TODO: This should actually be behind a must_complete work queue item.
      ir::CompiledFn fn(type::Func({}, {}),
                        core::Params<type::Typed<ast::Declaration const *>>{});
      ICARUS_SCOPE(ir::SetCurrent(&fn, &c.builder())) {
        // TODO: this is essentially a copy of the body of
        // FunctionLiteral::EmitValue Factor these out together.
        c.builder().CurrentBlock() = fn.entry();

        std::vector<ir::StructField> fields;
        fields.reserve(node->fields().size());

        for (auto const &field : node->fields()) {
          // TODO hashtags, special members.
          if (auto *init_val = field.init_val()) {
            // TODO init_val type may not be the same.
            auto *t = ASSERT_NOT_NULL(c.qual_type_of(init_val)->type());
            fields.emplace_back(field.id(), t, c.EmitValue(init_val));
          } else {
            fields.emplace_back(
                field.id(),
                c.EmitValue(field.type_expr()).get<type::Type const *>());
          }
        }
        c.builder().Struct(&c.data().module(), s, std::move(fields),
                           std::nullopt);
        c.builder().ReturnJump();
      }

      // TODO: What if execution fails.
      fn.WriteByteCode();
      interpretter::Execute(std::move(fn));
      DEBUG_LOG("struct")
      ("Completed ", node->DebugString(), " which is a (parameterized) struct ",
       *s, " with ", s->fields().size(), " field(s).");
      return s;
    } else {
      return data.get_struct(node);
    }
  };

  return data().set_qual_type(
      node, type::QualType::Constant(new type::GenericStruct(std::move(gen))));
}

}  // namespace compiler

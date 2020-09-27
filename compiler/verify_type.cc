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
#include "compiler/verify/common.h"
#include "core/call.h"
#include "diagnostic/errors.h"
#include "frontend/lex/operators.h"
#include "ir/compiled_fn.h"
#include "ir/interpretter/evaluate.h"
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
namespace {

std::optional<type::Quals> VerifyAndGetQuals(
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

}  // namespace

type::QualType Compiler::VerifyType(ast::ArgumentType const *node) {
  return data().set_qual_type(node, type::QualType::Constant(type::Type_));
}

WorkItem::Result Compiler::VerifyBody(ast::BlockLiteral const *node) {
  bool success = true;
  // TODO consider not verifying the types of the bodies. They almost certainly
  // contain circular references in the jump statements, and if the functions
  // require verifying the body upfront, things can maybe go wrong?
  for (auto *b : node->before()) { success &= VerifyType(b).ok(); }
  for (auto *a : node->after()) { success &= VerifyType(a).ok(); }
  return WorkItem::Result::Success;
}

type::QualType Compiler::VerifyType(ast::BlockLiteral const *node) {
  LOG("compile-work-queue", "Request work block: %p", node);
  state_.work_queue.Enqueue({
      .kind     = WorkItem::Kind::VerifyBlockBody,
      .node     = node,
      .context  = data(),
      .consumer = diag(),
  });
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

WorkItem::Result Compiler::VerifyBody(ast::EnumLiteral const *node) {
  bool success = true;
  for (auto const &elem : node->elems()) {
    if (auto *decl = elem->if_as<ast::Declaration>()) {
      auto const &t = *ASSERT_NOT_NULL(
          VerifyType(ASSERT_NOT_NULL(decl->init_val())).type());
      ASSERT(type::IsIntegral(&t) == true);
      success = type::IsIntegral(&t);
      // TODO determine what is allowed here and how to generate errors.
    }
  }
  return success ? WorkItem::Result::Success : WorkItem::Result::Failure;
}

type::QualType Compiler::VerifyType(ast::EnumLiteral const *node) {
  LOG("compile-work-queue", "Request work enum: %p", node);
  state_.work_queue.Enqueue({
      .kind     = WorkItem::Kind::VerifyEnumBody,
      .node     = node,
      .context  = data(),
      .consumer = diag(),
  });
  return data().set_qual_type(node, type::QualType::Constant(type::Type_));
}

type::QualType Compiler::VerifyType(ast::ShortFunctionLiteral const *node) {
  ast::OverloadSet os;
  os.insert(node);
  data().SetAllOverloads(node, std::move(os));

  if (not node->is_generic()) {
    ASSIGN_OR(return type::QualType::Error(),  //
                     auto params, VerifyParams(node->params()));
    ASSIGN_OR(return _, auto body_qt, VerifyType(node->body()));
    return data().set_qual_type(
        node, type::QualType::Constant(
                  type::Func(std::move(params), {body_qt.type()})));
  }

  auto ordered_nodes = OrderedDependencyNodes(node);

  auto *diag_consumer = &diag();
  auto gen            = [node, compiler_data = &data(), diag_consumer,
              ordered_nodes = std::move(ordered_nodes)](
                 core::FnArgs<type::Typed<ir::Value>> const &args) mutable
      -> type::Function const * {
    // TODO handle compilation failures.
    auto [params, rets, data, inserted] =
        MakeConcrete(node, &compiler_data->module(), ordered_nodes, args,
                     *compiler_data, *diag_consumer);

    module::FileImporter<LibraryModule> importer;
    auto body_qt = Compiler({.builder             = ir::GetBuilder(),
                             .data                = data,
                             .diagnostic_consumer = *diag_consumer,
                             .importer            = importer})
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

type::QualType Compiler::VerifyType(ast::Label const *node) {
  return data().set_qual_type(node, type::QualType::Constant(type::Label));
}

WorkItem::Result Compiler::VerifyBody(ast::Jump const *node) {
  bool success = true;
  for (auto const *stmt : node->stmts()) { success &= VerifyType(stmt).ok(); }
  return WorkItem::Result::Success;
}

type::QualType Compiler::VerifyType(ast::Jump const *node) {
  LOG("Jump", "%s", node->DebugString());

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

  LOG("compile-work-queue", "Request work jump: %p", node);
  state_.work_queue.Enqueue({
      .kind     = WorkItem::Kind::VerifyJumpBody,
      .node     = node,
      .context  = data(),
      .consumer = diag(),
  });
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
  LOG("ScopeNode", "%s", node->DebugString());
  ASSIGN_OR(return type::QualType::Error(),  //
                   std::ignore, VerifyFnArgs(node->args()));
  for (auto const &block : node->blocks()) { VerifyType(&block); }
  // TODO hack. Set this for real.
  return data().set_qual_type(node, type::QualType::NonConstant(type::Void()));
}

WorkItem::Result Compiler::VerifyBody(ast::ParameterizedStructLiteral const *node) {
  // NOT_YET();
  return WorkItem::Result::Success;
}

type::QualType Compiler::VerifyType(
    ast::ParameterizedStructLiteral const *node) {
  auto ordered_nodes  = OrderedDependencyNodes(node);
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
    module::FileImporter<LibraryModule> importer;
    if (inserted) {
      Compiler c({
          .builder             = ir::GetBuilder(),
          .data                = data,
          .diagnostic_consumer = *diag_consumer,
          .importer            = importer,
      });

      type::Struct *s = new type::Struct(
          &c.data().module(),
          {.is_copyable = not node->contains_hashtag(
               ast::Hashtag(ast::Hashtag::Builtin::Uncopyable)),
           .is_movable = not node->contains_hashtag(
               ast::Hashtag(ast::Hashtag::Builtin::Immovable))});

      LOG("struct", "Allocating a new (parameterized) struct %p for %p", s,
          node);
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
        // TODO destructors and assignment
        c.builder().Struct(s, std::move(fields), std::nullopt, std::nullopt);
        c.builder().ReturnJump();
      }

      // TODO: What if execution fails.
      fn.WriteByteCode<interpretter::instruction_set_t>();
      interpretter::Execute(std::move(fn));
      LOG("struct",
          "Completed %s which is a (parameterized) struct %s with %u field(s).",
          node->DebugString(), *s, s->fields().size());
      return s;
    } else {
      return data.get_struct(node);
    }
  };

  return data().set_qual_type(
      node, type::QualType::Constant(new type::GenericStruct(std::move(gen))));
}

}  // namespace compiler

#include "compiler/compiler.h"

#include "ast/ast.h"
#include "ast/scope/exec.h"
#include "base/defer.h"
#include "base/guarded.h"
#include "compiler/emit/common.h"
#include "compiler/emit_function_call_infrastructure.h"
#include "compiler/executable_module.h"
#include "diagnostic/consumer/trivial.h"
#include "frontend/parse.h"
#include "ir/builder.h"
#include "ir/compiled_jump.h"
#include "ir/instruction/type.h"
#include "ir/interpretter/evaluate.h"
#include "ir/interpretter/execution_context.h"
#include "ir/value/builtin_fn.h"
#include "ir/value/generic_fn.h"
#include "ir/value/reg.h"
#include "type/generic_struct.h"
#include "type/jump.h"
#include "type/type.h"
#include "type/typed_value.h"

// TODO: Value emission of any kind could fail even if there weren't type
// errors. Any value emission could be a constant-evaluated function that ends
// up dividing by zero. It's *possible* you that you can cover this stuff with
// property checking, but that's way off in the future.
namespace compiler {
namespace {

template <typename NodeType>
base::move_func<void()> *DeferBody(Compiler::PersistentResources resources,
                                   TransientState &state, NodeType const *node,
                                   type::Type t) {
  LOG("DeferBody", "Deferring body of %s", node->DebugString());
  state.deferred_work.push_back(std::make_unique<base::move_func<void()>>(
      [c = Compiler(resources), node, t]() mutable {
        if constexpr (base::meta<NodeType> ==
                          base::meta<ast::FunctionLiteral> or
                      base::meta<NodeType> ==
                          base::meta<ast::ShortFunctionLiteral>) {
          CompleteBody(&c, node, &t.as<type::Function>());
        } else {
          static_cast<void>(t);
          CompleteBody(&c, node);
        }
      }));
  return state.deferred_work.back().get();
}

}  // namespace

ir::Value Compiler::EmitValue(ast::Assignment const *node) {
  std::vector<type::Typed<ir::RegOr<ir::Addr>>> lhs_refs;
  lhs_refs.reserve(node->lhs().size());

  // TODO understand the precise semantics you care about here and document
  // them. Must references be computed first?
  for (auto const *l : node->lhs()) {
    lhs_refs.push_back(type::Typed<ir::RegOr<ir::Addr>>(
        EmitRef(l), ASSERT_NOT_NULL(context().qual_type(l))->type()));
  }

  auto ref_iter = lhs_refs.begin();
  for (auto const *r : node->rhs()) {
    auto from_qt          = *ASSERT_NOT_NULL(context().qual_type(r));
    size_t expansion_size = from_qt.expansion_size();
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> ref_span(&*ref_iter,
                                                                expansion_size);
    EmitAssign(r, ref_span);
    ref_iter += expansion_size;
  }
  return ir::Value();
}

ir::Value Compiler::EmitValue(ast::Declaration const *node) {
  // TODO: The entirety of constant-caching mechanism here is weird and broken
  // and needs a cleanup.
  LOG("EmitValueDeclaration", "%s", node->id());
  if (node->flags() & ast::Declaration::f_IsConst) {
    if (node->module() != &context().module()) {
      // Constant declarations from other modules should already be stored on
      // that module. They must be at the root of the binding tree map,
      // otherwise they would be local to some function/jump/etc. and not be
      // exported.
      return node->module()
          ->as<CompiledModule>()
          .context()
          .Constant(node)
          ->value;
    }

    if (node->flags() & ast::Declaration::f_IsFnParam) {
      auto val = context().LoadConstantParam(node);
      LOG("EmitValueDeclaration", "%s", val);
      return val;
    } else {
      if (auto *constant_value = context().Constant(node)) {
        // TODO: This feels quite hacky.
        if (node->init_val()->is<ast::StructLiteral>()) {
          if (not constant_value->complete and state_.must_complete) {
            LOG("compile-work-queue", "Request work complete-struct: %p", node);
            state_.work_queue.Enqueue({
                .kind     = WorkItem::Kind::CompleteStructMembers,
                .node     = node->init_val(),
                .context  = context(),
                .consumer = diag(),
            });
          }
        }
        return constant_value->value;
      }
      auto t = type_of(node);

      if (node->IsCustomInitialized()) {
        LOG("EmitValueDeclaration", "Computing slot with %s",
            node->init_val()->DebugString());
        auto maybe_val =
            Evaluate(type::Typed<ast::Expression const *>(node->init_val(), t),
                     state_.must_complete);
        if (not maybe_val) {
          // TODO we reserved a slot and haven't cleaned it up. Do we care?
          diag().Consume(maybe_val.error());
          return ir::Value();
        }

        LOG("EmitValueDeclaration", "Setting slot = %s", *maybe_val);
        context().SetConstant(node, *maybe_val);

        // TODO: This is a struct-speficic hack.
        if (type::Type *type_val = maybe_val->get_if<type::Type>()) {
          if (auto const *struct_type = type_val->if_as<type::Struct>()) {
            if (struct_type->completeness() != type::Completeness::Complete) {
              return *maybe_val;
            }
            context().CompleteConstant(node);
          }
        }

        return *maybe_val;

      } else if (node->IsDefaultInitialized()) {
        UNREACHABLE();
      } else {
        UNREACHABLE();
      }
      UNREACHABLE(node->DebugString());
    }
  } else {
    if (node->IsUninitialized()) { return ir::Value(); }
    auto t = context().qual_type(node)->type();
    auto a = context().addr(node);
    if (node->IsCustomInitialized()) {
      auto to = type::Typed<ir::RegOr<ir::Addr>>(a, t);
      EmitMoveInit(node->init_val(), absl::MakeConstSpan(&to, 1));
    } else {
      if (not(node->flags() & ast::Declaration::f_IsFnParam)) {
        EmitDefaultInit(type::Typed<ir::Reg>(a, t));
      }
    }
    return ir::Value(a);
  }
  UNREACHABLE();
}

ir::Value Compiler::EmitValue(ast::DesignatedInitializer const *node) {
  auto t     = type_of(node);
  auto alloc = builder().TmpAlloca(t);
  auto typed_alloc =
      type::Typed<ir::RegOr<ir::Addr>>(ir::RegOr<ir::Addr>(alloc), t);
  EmitMoveInit(node, absl::MakeConstSpan(&typed_alloc, 1));
  return ir::Value(alloc);
}

void Compiler::EmitAssign(
    ast::DesignatedInitializer const *node,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  // TODO: Improve this to avoid the temporary allocation.
  auto t = context().qual_type(node)->type();
  ASSERT(to.size() == 1u);
  EmitCopyAssign(to[0], type::Typed<ir::Value>(EmitValue(node), t));
}

void Compiler::EmitMoveInit(
    ast::DesignatedInitializer const *node,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  ASSERT(to.size() == 1u);
  // TODO actual initialization with these field members.
  auto t             = type_of(node);
  auto &struct_type  = t.as<type::Struct>();
  auto const &fields = struct_type.fields();
  for (size_t i = 0; i < fields.size(); ++i) {
    auto const &field = fields[i];

    for (auto const *assignment : node->assignments()) {
      for (auto const *expr : assignment->lhs()) {
        std::string_view field_name = expr->as<ast::Identifier>().name();
        // Skip default initialization if we're going to use the designated
        // initializer.
        if (field_name == field.name) { goto next_field; }
      }
    }

    {
      auto field_reg = builder().FieldRef(to[0]->reg(), &struct_type, i);
      if (field.initial_value.empty()) {
        EmitDefaultInit(field_reg);
      } else {
        EmitCopyAssign(field_reg,
                       type::Typed<ir::Value>(field.initial_value, field.type));
      }
    }
  next_field:;
  }

  for (auto const *assignment : node->assignments()) {
    if (assignment->lhs().size() != 1) { NOT_YET(assignment->lhs().size()); }
    if (assignment->rhs().size() != 1) { NOT_YET(assignment->rhs().size()); }

    auto const &id     = assignment->lhs()[0]->as<ast::Identifier>();
    auto const *f      = struct_type.field(id.name());
    size_t field_index = struct_type.index(f->name);
    type::Typed<ir::RegOr<ir::Addr>> field_reg =
        builder().FieldRef(to[0]->reg(), &struct_type, field_index);
    EmitMoveInit(assignment->rhs()[0], absl::MakeConstSpan(&field_reg, 1));
  }
}

void Compiler::EmitCopyInit(
    ast::DesignatedInitializer const *node,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  ASSERT(to.size() == 1u);
  // TODO actual initialization with these field members.
  auto t             = type_of(node);
  auto &struct_type  = t.as<type::Struct>();
  auto const &fields = struct_type.fields();
  for (size_t i = 0; i < fields.size(); ++i) {
    auto const &field = fields[i];

    for (auto const *assignment : node->assignments()) {
      for (auto const *expr : assignment->lhs()) {
        std::string_view field_name = expr->as<ast::Identifier>().name();
        // Skip default initialization if we're going to use the designated
        // initializer.
        if (field_name == field.name) { goto next_field; }
      }
    }

    {
      auto field_reg = builder().FieldRef(to[0]->reg(), &struct_type, i);
      if (field.initial_value.empty()) {
        EmitDefaultInit(field_reg);
      } else {
        EmitCopyAssign(field_reg,
                       type::Typed<ir::Value>(field.initial_value, field.type));
      }
    }
  next_field:;
  }

  for (auto const *assignment : node->assignments()) {
    if (assignment->lhs().size() != 1) { NOT_YET(assignment->lhs().size()); }
    if (assignment->rhs().size() != 1) { NOT_YET(assignment->rhs().size()); }

    auto const &id     = assignment->lhs()[0]->as<ast::Identifier>();
    auto const *f      = struct_type.field(id.name());
    size_t field_index = struct_type.index(f->name);
    auto field_reg =
        builder().FieldRef(to[0]->reg(), &struct_type, field_index);
    type::Typed<ir::RegOr<ir::Addr>> lhs(*field_reg, field_reg.type());
    EmitCopyInit(assignment->rhs()[0], absl::MakeConstSpan(&lhs, 1));
  }
}

ir::Value Compiler::EmitValue(ast::EnumLiteral const *node) {
  // TODO: Check the result of body verification.
  if (context().ShouldVerifyBody(node)) { VerifyBody(node); }

  using enum_t = ir::EnumVal::underlying_type;
  std::vector<std::string_view> names(node->enumerators().begin(),
                                      node->enumerators().end());
  absl::flat_hash_map<uint64_t, ir::RegOr<enum_t>> specified_values;

  uint64_t i = 0;
  for (uint64_t i = 0; i < names.size(); ++i) {
    if (auto iter = node->specified_values().find(names[i]);
        iter != node->specified_values().end()) {
      specified_values.emplace(
          i, EmitValue(iter->second.get()).get<ir::RegOr<enum_t>>());
    }
  }

  // TODO: allocate the type upfront so it can be used in incomplete contexts.
  switch (node->kind()) {
    case ast::EnumLiteral::Kind::Enum: {
      auto *e = type::Allocate<type::Enum>(&context().module());
      return ir::Value(builder().Enum(e, names, specified_values));
    } break;
    case ast::EnumLiteral::Kind::Flags: {
      auto *f = type::Allocate<type::Flags>(&context().module());
      return ir::Value(builder().Flags(f, names, specified_values));
    } break;
    default: UNREACHABLE();
  }
}

template <typename NodeType>
ir::NativeFn MakeConcreteFromGeneric(
    Compiler *compiler, NodeType const *node,
    core::Arguments<type::Typed<ir::Value>> const &args) {
  ASSERT(node->is_generic() == true);

  // Note: Cannot use structured bindings because the bindings need to be
  // captured in the lambda.
  auto find_subcontext_result = compiler->FindInstantiation(node, args);
  auto const *fn_type         = find_subcontext_result.fn_type;
  auto &context               = find_subcontext_result.context;

  auto [f, inserted] = context.add_func(node);
  if (inserted) {
    f->work_item = DeferBody({.data                = context,
                              .diagnostic_consumer = compiler->diag(),
                              .importer            = compiler->importer()},
                             compiler->state(), node, f.type());
  }
  return f;
}

ir::Value Compiler::EmitValue(ast::ShortFunctionLiteral const *node) {
  if (node->is_generic()) {
    auto gen_fn = ir::GenericFn(
        [c = this->WithPersistent(),
         node](core::Arguments<type::Typed<ir::Value>> const &args) mutable
        -> ir::NativeFn { return MakeConcreteFromGeneric(&c, node, args); });
    return ir::Value(gen_fn);
  }

  auto [f, inserted] = context().add_func(node);
  if (inserted) {
    f->work_item = DeferBody(resources_, state_, node, f.type());
  }
  return ir::Value(ir::Fn{f});
}

ir::Value Compiler::EmitValue(ast::FunctionLiteral const *node) {
  if (node->is_generic()) {
    auto gen_fn = ir::GenericFn(
        [c = this->WithPersistent(),
         node](core::Arguments<type::Typed<ir::Value>> const &args) mutable
        -> ir::NativeFn { return MakeConcreteFromGeneric(&c, node, args); });
    return ir::Value(gen_fn);
  }

  // TODO: Check the result of body verification.
  if (context().ShouldVerifyBody(node)) { VerifyBody(node); }

  // TODO Use correct constants
  auto [f, inserted] = context().add_func(node);
  if (inserted) {
    f->work_item = DeferBody(resources_, state_, node, f.type());
  }
  return ir::Value(ir::Fn{f});
}

ir::Value Compiler::EmitValue(ast::FunctionType const *node) {
  std::vector<ir::RegOr<type::Type>> param_vals, out_vals;
  param_vals.reserve(node->params().size());
  out_vals.reserve(node->outputs().size());
  for (auto const *p : node->params()) {
    param_vals.push_back(EmitValue(p).get<ir::RegOr<type::Type>>());
  }
  for (auto const *o : node->outputs()) {
    out_vals.push_back(EmitValue(o).get<ir::RegOr<type::Type>>());
  }

  return ir::Value(builder().Arrow(std::move(param_vals), std::move(out_vals)));
}

ir::Value Compiler::EmitValue(ast::Jump const *node) {
  LOG("Jump", "Emit %s", node->DebugString());
  // TODO: Check the result of body verification.
  if (context().ShouldVerifyBody(node)) { VerifyBody(node); }

  auto [jmp, inserted] = context().add_jump(node);
  if (inserted) {
    auto j       = ir::CompiledJump::From(jmp);
    j->work_item = DeferBody(resources_, state(), node, j->type());
  }
  return ir::Value(jmp);
}

static std::vector<std::pair<ast::Expression const *, ir::Value>>
EmitValueWithExpand(Compiler *c, base::PtrSpan<ast::Expression const> exprs) {
  // TODO expansion
  std::vector<std::pair<ast::Expression const *, ir::Value>> vals;
  for (auto *expr : exprs) { vals.emplace_back(expr, c->EmitValue(expr)); }
  return vals;
}

ir::Value Compiler::EmitValue(ast::ReturnStmt const *node) {
  auto const &fn_scope =
      *ASSERT_NOT_NULL(node->scope()->Containing<ast::FnScope>());
  auto const &fn_lit  = *ASSERT_NOT_NULL(fn_scope.fn_lit_);
  auto const &fn_type = type_of(&fn_lit).as<type::Function>();

  // TODO: It's tricky... on a single expression that gets expanded, we could
  // have both small and big types and we would need to handle both setting
  // registers for small types and writing through them for big ones.
  ASSERT(
      node->exprs().size() ==
      fn_type.output().size());  // TODO: For now, assume no actual expansion.
  for (size_t i = 0; i < node->exprs().size(); ++i) {
    auto const *expr    = node->exprs()[i];
    type::Type ret_type = fn_type.output()[i];
    if (ret_type.get()->is_big()) {
      type::Typed<ir::RegOr<ir::Addr>> typed_alloc(
          ir::RegOr<ir::Addr>(builder().GetRet(i, ret_type)), ret_type);
      EmitMoveInit(expr, absl::MakeConstSpan(&typed_alloc, 1));
    } else {
      builder().SetRet(i, type::Typed<ir::Value>(EmitValue(expr), ret_type));
    }
  }

  // Rather than doing this on each block it'd be better to have each
  // scope's destructors jump you to the correct next block for destruction.
  auto *scope = node->scope();
  while (auto *exec = scope->if_as<ast::ExecScope>()) {
    MakeAllDestructions(*this, exec);
    if (not exec->parent or exec->is<ast::FnScope>()) { break; }
    scope = exec->parent;
  }

  builder().ReturnJump();
  return ir::Value();
}

ir::Value Compiler::EmitValue(ast::YieldStmt const *node) {
  auto arg_vals = EmitValueWithExpand(this, node->exprs());
  // TODO store this as an exec_scope.
  MakeAllDestructions(*this, &node->scope()->as<ast::ExecScope>());
  // TODO pretty sure this is all wrong.

  // Can't return these because we need to pass them up at least through the
  // containing statements this and maybe further if we allow labelling
  // scopes to be yielded to.
  auto &yielded_args = state_.yields.back();

  // TODO one problem with this setup is that we look things up in a context
  // after returning, so the `after` method has access to a different
  // (smaller) collection of bound constants. This can change the meaning of
  // things or at least make them not compile if the `after` function takes
  // a compile-time constant argument.
  for (size_t i = 0; i < arg_vals.size(); ++i) {
    auto qt = qual_type_of(node->exprs()[i]);
    ASSERT(qt.has_value() == true);
    yielded_args.vals.pos_emplace(arg_vals[i].second, *qt);
  }
  yielded_args.label = node->label() ? node->label()->value() : ir::Label{};

  builder().block_termination_state() =
      node->label() ? ir::Builder::BlockTerminationState::kLabeledYield
                    : ir::Builder::BlockTerminationState::kYield;
  return ir::Value();
}

ir::Value Compiler::EmitValue(ast::ScopeLiteral const *node) {
  LOG("ScopeLiteral", "State type = %p", node->state_type());
  type::Type state_type = nullptr;
  if (node->state_type()) {
    ASSIGN_OR(return ir::Value(),  //
                     type::Type state_type,
                     EvaluateOrDiagnoseAs<type::Type>(node->state_type()));
  }

  absl::flat_hash_map<std::string_view, ir::Block> blocks;
  std::vector<ir::RegOr<ir::Jump>> inits;
  std::vector<ir::RegOr<ir::Fn>> dones;
  for (auto const &decl : node->decls()) {
    if (decl.id() == "enter") {
      inits.push_back(EmitValue(&decl).get<ir::RegOr<ir::Jump>>());
    } else if (decl.id() == "exit") {
      dones.push_back(EmitValue(&decl).get<ir::RegOr<ir::Fn>>());
    } else {
      blocks.emplace(decl.id(),
                     EmitValue(&decl).get<ir::RegOr<ir::Block>>().value());
    }
  }

  return ir::Value(builder().MakeScope(context().add_scope(state_type),
                                       std::move(inits), std::move(dones),
                                       std::move(blocks)));
}

WorkItem::Result Compiler::CompleteStruct(ast::StructLiteral const *node) {
  LOG("struct", "Completing struct-literal emission: %p must-complete = %s",
      node, state_.must_complete ? "true" : "false");

  type::Struct *s = context().get_struct(node);
  if (s->completeness() == type::Completeness::Complete) {
    LOG("struct", "Already complete, exiting: %p", node);
    return WorkItem::Result::Success;
  }

  ASSIGN_OR(return WorkItem::Result::Failure,  //
                   auto fn, StructCompletionFn(*this, s, node->fields()));
  // TODO: What if execution fails.
  interpretter::Execute(std::move(fn));
  s->complete();
  LOG("struct", "Completed %s which is a struct %s with %u field(s).",
      node->DebugString(), *s, s->fields().size());
  return WorkItem::Result::Success;
}

ir::Value Compiler::EmitValue(ast::StructLiteral const *node) {
  LOG("struct", "Starting struct-literal emission: %p%s", node,
      state_.must_complete ? " (must complete)" : " (need not complete)");

  if (type::Struct *s = context().get_struct(node)) {
    return ir::Value(static_cast<type::Type>(s));
  }

  type::Struct *s = type::Allocate<type::Struct>(
      &context().module(),
      type::Struct::Options{
          .is_copyable = not node->hashtags.contains(ir::Hashtag::Uncopyable),
          .is_movable  = not node->hashtags.contains(ir::Hashtag::Immovable),
      });

  LOG("struct", "Allocating a new struct %p for %p", s, node);
  context().set_struct(node, s);

  // Note: VerifyBody may end up triggering EmitValue calls for member types
  // that depend on this incomplete type. For this reason it is important that
  // we have already allocated the struct so we do not get a double-allocation.
  //
  // The process, as you can see above is to
  // 1. Check if it has already been allocated. Return if it has been.
  // 2. Allocate ourselves.
  // 3. Start body verification.
  // 4. Schedule completion.
  //
  // Notably, steps 1 and 2 must not be separated. Moreover, because body
  // verification could end up calling this function again, we must "set up
  // guards" (i.e., steps 1 and 2) before step 3 runs.
  //
  // TODO: Check the result of body verification.
  if (context().ShouldVerifyBody(node)) { VerifyBody(node); }

  if (state_.must_complete) {
    LOG("compile-work-queue", "Request work complete struct: %p", node);
    state_.work_queue.Enqueue({
        .kind     = WorkItem::Kind::CompleteStructMembers,
        .node     = node,
        .context  = context(),
        .consumer = diag(),
    });
  }
  return ir::Value(static_cast<type::Type>(s));
}

ir::Value Compiler::EmitValue(ast::ParameterizedStructLiteral const *node) {
  // TODO: Check the result of body verification.
  if (context().ShouldVerifyBody(node)) { VerifyBody(node); }
  return ir::Value(ir::GenericFn(
      [c = this->WithPersistent(),
       node](core::Arguments<type::Typed<ir::Value>> const &args) mutable
      -> ir::NativeFn { return MakeConcreteFromGeneric(&c, node, args); }));
}

}  // namespace compiler

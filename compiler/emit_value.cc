#include "ast/ast.h"
#include "ast/scope/exec.h"
#include "base/defer.h"
#include "base/guarded.h"
#include "compiler/compiler.h"
#include "compiler/emit/common.h"
#include "compiler/executable_module.h"
#include "compiler/resources.h"
#include "diagnostic/consumer/trivial.h"
#include "frontend/parse.h"
#include "ir/builder.h"
#include "ir/compiled_jump.h"
#include "ir/interpreter/evaluate.h"
#include "ir/interpreter/execution_context.h"
#include "ir/value/builtin_fn.h"
#include "ir/value/generic_fn.h"
#include "ir/value/reg.h"
#include "type/enum.h"
#include "type/flags.h"
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
base::move_func<void()> *DeferBody(PersistentResources resources,
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

void MakeAllStackAllocations(Compiler &compiler, ast::FnScope const *fn_scope) {
  for (auto *scope : fn_scope->descendants()) {
    if (scope != fn_scope and scope->is<ast::FnScope>()) { continue; }
    for (const auto &[key, val] : scope->decls_) {
      LOG("MakeAllStackAllocations", "%s", key);
      for (auto *decl : val) {
        if (decl->flags() &
            (ast::Declaration::f_IsConst | ast::Declaration::f_IsFnParam)) {
          LOG("MakeAllStackAllocations", "skipping constant/param decl %s",
              decl->id());
          continue;
        }

        LOG("MakeAllStackAllocations", "allocating %s", decl->id());

        compiler.context().set_addr(
            decl, compiler.builder().Alloca(
                      compiler.context().qual_type(decl)->type()));
      }
    }
  }
}

// TODO One problem with this setup is that we don't end up calling destructors
// if we exit early, so those need to be handled externally.
void EmitIrForStatements(Compiler &compiler,
                         base::PtrSpan<ast::Node const> span) {
  ICARUS_SCOPE(ir::SetTemporaries(compiler.builder())) {
    for (auto *stmt : span) {
      LOG("EmitIrForStatements", "%s", stmt->DebugString());
      compiler.EmitValue(stmt);
      compiler.builder().FinishTemporariesWith(
          [&compiler](type::Typed<ir::Reg> r) { compiler.EmitDestroy(r); });
      LOG("EmitIrForStatements", "%p %s", compiler.builder().CurrentBlock(),
          *compiler.builder().CurrentGroup());

      if (compiler.builder().block_termination_state() !=
          ir::Builder::BlockTerminationState::kMoreStatements) {
        break;
      }
    }
  }
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
                .kind      = WorkItem::Kind::CompleteStructMembers,
                .node      = node->init_val(),
                .resources = resources_,
            });
          }
        }
        return constant_value->value;
      }

      auto t = ASSERT_NOT_NULL(context().qual_type(node))->type();

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

ir::Value Compiler::EmitValue(ast::EnumLiteral const *node) {
  // TODO: Check the result of body verification.
  if (context().ShouldVerifyBody(node)) { VerifyBody(node); }

  std::vector<std::string_view> names(node->enumerators().begin(),
                                      node->enumerators().end());
  absl::flat_hash_map<uint64_t, ir::RegOr<type::Enum::underlying_type>>
      specified_values;

  uint64_t i = 0;
  for (uint64_t i = 0; i < names.size(); ++i) {
    if (auto iter = node->specified_values().find(names[i]);
        iter != node->specified_values().end()) {
      specified_values.emplace(
          i, EmitValue(iter->second.get())
                 .get<ir::RegOr<type::Enum::underlying_type>>());
    }
  }

  // TODO: allocate the type upfront so it can be used in incomplete contexts.
  switch (node->kind()) {
    case ast::EnumLiteral::Kind::Enum: {
      return ir::Value(current_block()->Append(type::EnumInstruction{
          .type              = type::Allocate<type::Enum>(&context().module()),
          .names_            = std::move(names),
          .specified_values_ = std::move(specified_values),
          .result            = builder().CurrentGroup()->Reserve()}));
    } break;
    case ast::EnumLiteral::Kind::Flags: {
      return ir::Value(current_block()->Append(type::FlagsInstruction{
          .type              = type::Allocate<type::Flags>(&context().module()),
          .names_            = std::move(names),
          .specified_values_ = std::move(specified_values),
          .result            = builder().CurrentGroup()->Reserve()}));
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
  // TODO: Check for whether or not we actually need to do the verification is
  // handled inside VerifyBody, at least for now.
  VerifyBody(node);

  // TODO Use correct constants
  auto [f, inserted] = context().add_func(node);
  if (inserted) {
    f->work_item = DeferBody(resources_, state_, node, f.type());
  }
  return ir::Value(ir::Fn{f});
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
  auto const &fn_lit = *ASSERT_NOT_NULL(fn_scope.fn_lit_);
  auto const &fn_type =
      context().qual_type(&fn_lit)->type().as<type::Function>();

  // TODO: Reduce code-size by sharing these sequences whenever they share a
  // suffix.
  for (auto iter = state().scope_landings.rbegin();
       iter != state().scope_landings.rend(); ++iter) {
    // TODO: Emit all destructions on this scope.
    // TODO: Call the quick-exit for this scope.
  }

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

  builder().FinishTemporariesWith(
      [this](type::Typed<ir::Reg> r) { EmitDestroy(r); });

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

  // TODO: store this as an exec_scope.
  MakeAllDestructions(*this, &node->scope()->as<ast::ExecScope>());

  if (ast::Label const *lbl = node->label()) {
    auto iter = state().scope_landings.rbegin();
    for (; iter->label == lbl->value(); ++iter) {
      // TODO: Emit all destructions on this scope.
      // TODO: Call the quick-exit for this scope.
    }
    // TODO: Call `before` with arguments.
    ir::CompiledScope *compiled_scope = ir::CompiledScope::From(iter->scope);

    core::Arguments<type::QualType> yield_arg_types;
    core::Arguments<type::Typed<ir::Value>> yield_arg_typed_values;
    for (size_t i = 0; i < arg_vals.size(); ++i) {
      type::QualType const *qt = context().qual_type(arg_vals[i].first);
      yield_arg_types.pos_emplace(*qt);
      yield_arg_typed_values.pos_emplace(arg_vals[i].second, qt->type());
      // TODO: Determine if you are going to support named yields.
    }

    ir::OverloadSet &exit = compiled_scope->exit();
    ir::Fn exit_fn        = exit.Lookup(yield_arg_types).value();

    type::Type t;
    if (iter->result_type.expansion_size() == 1) {
      t = iter->result_type.type();
    }
    absl::Span<type::Type const> result_types =
        iter->result_type.expansion_size() == 1
            ? absl::Span<type::Type const>(&t, 1)
            : iter->result_type.expanded();
    auto out_params = builder().OutParams(result_types);

    auto inst_iter = iter->block->instructions().begin();
    auto out_iter  = out_params.regs().begin();
    for (type::Type const &result_type : result_types) {
      // TODO: Support all types
      type::ApplyTypes<bool, int8_t, int16_t, int32_t, int64_t, uint8_t,
                       uint16_t, uint32_t, uint64_t, float, double>(
          result_type, [&]<typename T>() {
            ASSERT(inst_iter != iter->block->instructions().end());
            ASSERT(static_cast<bool>(*inst_iter) == true);
            ASSERT(out_iter != out_params.regs().end());
            auto &phi = inst_iter->template as<ir::PhiInstruction<T>>();
            phi.blocks.push_back(builder().CurrentBlock());
            phi.values.push_back(*out_iter);
            ++inst_iter;
            ++out_iter;
          });
    }

    builder().Call(
        exit_fn, exit_fn.type(),
        PrepareCallArguments(compiled_scope->state_type(),
                             exit_fn.type()->params(), yield_arg_typed_values),
        std::move(out_params));

    builder().UncondJump(iter->block);

    // TODO: Wire up phi node.
    builder().block_termination_state() =
        ir::Builder::BlockTerminationState::kLabeledYield;
  } else {
    builder().block_termination_state() =
        ir::Builder::BlockTerminationState::kYield;
  }

  return ir::Value();
}

WorkItem::Result Compiler::CompleteStruct(
    ast::ParameterizedStructLiteral const *node) {
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
  interpreter::Execute(std::move(fn));
  s->complete();
  LOG("struct", "Completed %s which is a struct %s with %u field(s).",
      node->DebugString(), *s, s->fields().size());
  return WorkItem::Result::Success;
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
  interpreter::Execute(std::move(fn));
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
        .kind      = WorkItem::Kind::CompleteStructMembers,
        .node      = node,
        .resources = resources_,
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

void CompleteBody(Compiler *compiler, ast::ShortFunctionLiteral const *node,
                  type::Function const *t) {
  // TODO have validate return a bool distinguishing if there are errors and
  // whether or not we can proceed.

  ir::NativeFn ir_func =
      *ASSERT_NOT_NULL(compiler->context().FindNativeFn(node));

  auto &bldr = compiler->builder();
  ICARUS_SCOPE(ir::SetCurrent(ir_func, bldr)) {
    bldr.CurrentBlock() = bldr.CurrentGroup()->entry();

    // TODO arguments should be renumbered to not waste space on const values
    size_t i = 0;
    for (auto const &param : node->params()) {
      compiler->context().set_addr(param.value.get(), ir::Reg::Arg(i++));
    }

    MakeAllStackAllocations(*compiler, node->body_scope());

    type::Type ret_type = t->output()[0];
    if (ret_type.get()->is_big()) {
      type::Typed<ir::RegOr<ir::Addr>> typed_alloc(
          ir::RegOr<ir::Addr>(compiler->builder().GetRet(0, ret_type)),
          type::Ptr(ret_type));
      compiler->EmitMoveInit(node->body(),
                             absl::MakeConstSpan(&typed_alloc, 1));
    } else {
      compiler->builder().SetRet(
          0,
          type::Typed<ir::Value>(compiler->EmitValue(node->body()), ret_type));
    }

    bldr.FinishTemporariesWith([compiler](type::Typed<ir::Reg> r) {
      if (r.type().get()->HasDestructor()) { compiler->EmitDestroy(r); }
    });

    MakeAllDestructions(*compiler, node->body_scope());
    bldr.ReturnJump();
  }

  ir_func->work_item = nullptr;
  ir_func->WriteByteCode<interpreter::instruction_set_t>();
}

void CompleteBody(Compiler *compiler, ast::FunctionLiteral const *node,
                  type::Function const *t) {
  LOG("CompleteBody", "%s", node->DebugString());
  // TODO have validate return a bool distinguishing if there are errors and
  // whether or not we can proceed.

  ir::NativeFn ir_func =
      *ASSERT_NOT_NULL(compiler->context().FindNativeFn(node));

  auto &bldr = compiler->builder();
  ICARUS_SCOPE(ir::SetCurrent(ir_func, bldr)) {
    bldr.CurrentBlock() = bldr.CurrentGroup()->entry();

    // TODO arguments should be renumbered to not waste space on const values
    size_t i = 0;
    for (auto const &param : node->params()) {
      compiler->context().set_addr(param.value.get(), ir::Reg::Arg(i++));
    }

    MakeAllStackAllocations(*compiler, node->body_scope());
    if (auto outputs = node->outputs()) {
      for (size_t i = 0; i < outputs->size(); ++i) {
        auto *out_decl = (*outputs)[i]->if_as<ast::Declaration>();
        if (not out_decl) { continue; }
        type::Type out_decl_type =
            compiler->context().qual_type(out_decl)->type();
        auto alloc = out_decl_type.get()->is_big()
                         ? bldr.GetRet(i, out_decl_type)
                         : bldr.Alloca(out_decl_type);

        compiler->context().set_addr(out_decl, alloc);
        if (out_decl->IsDefaultInitialized()) {
          compiler->EmitDefaultInit(type::Typed<ir::Reg>(alloc, out_decl_type));
        } else {
          compiler->EmitCopyAssign(
              type::Typed<ir::RegOr<ir::Addr>>(alloc, out_decl_type),
              type::Typed<ir::Value>(compiler->EmitValue(out_decl->init_val()),
                                     out_decl_type));
        }
      }
    }

    EmitIrForStatements(*compiler, node->stmts());
    MakeAllDestructions(*compiler, node->body_scope());
    bldr.ReturnJump();
  }

  ir_func->work_item = nullptr;
  ir_func->WriteByteCode<interpreter::instruction_set_t>();
}

void CompleteBody(Compiler *compiler,
                  ast::ParameterizedStructLiteral const *node) {
  NOT_YET();
}

void CompleteBody(Compiler *compiler, ast::Jump const *node) {
  LOG("CompleteBody", "Jump %s", node->DebugString());
  ir::CompiledJump &jmp = *ASSERT_NOT_NULL(compiler->context().jump(node));

  ICARUS_SCOPE(ir::SetCurrent(jmp, compiler->builder())) {
    ASSERT(compiler != nullptr);
    compiler->builder().CurrentBlock() = jmp.entry();
    // TODO arguments should be renumbered to not waste space on const
    // values
    int32_t i = 0;
    if (node->state()) {
      compiler->context().set_addr(node->state(), ir::Reg::Arg(i++));
    }
    for (auto const &param : node->params()) {
      compiler->context().set_addr(param.value.get(), ir::Reg::Arg(i++));
    }

    MakeAllStackAllocations(*compiler, node->body_scope());

    EmitIrForStatements(*compiler, node->stmts());

    // TODO: it seems like this will be appended after ChooseJump, which means
    // it'll never be executed.
    MakeAllDestructions(*compiler, node->body_scope());
  }
  jmp.WriteByteCode<interpreter::instruction_set_t>();
  jmp.work_item = nullptr;
}

ir::Value Compiler::EmitValue(ast::BlockNode const *node) {
  LOG("BlockNode", "EmitValue for block node named %s", node->name());
  EmitIrForStatements(*this, node->stmts());
  MakeAllDestructions(*this, node->body_scope());
  auto &termination = builder().block_termination_state();
  if (termination == ir::Builder::BlockTerminationState::kMoreStatements) {
    termination = ir::Builder::BlockTerminationState::kNoTerminator;
  }
  return ir::Value();
}

void Compiler::ProcessExecutableBody(base::PtrSpan<ast::Node const> nodes,
                                     ir::CompiledFn *main_fn) {
  if (nodes.empty()) {
    ICARUS_SCOPE(ir::SetCurrent(*main_fn, builder())) {
      EmitIrForStatements(*this, nodes);
      builder().ReturnJump();
    }
  } else {
    ICARUS_SCOPE(ir::SetCurrent(*main_fn, builder())) {
      ast::ModuleScope *mod_scope =
          &nodes.front()->scope()->as<ast::ModuleScope>();

      MakeAllStackAllocations(*this, mod_scope);
      EmitIrForStatements(*this, nodes);
      MakeAllDestructions(*this, mod_scope);
      // TODO determine under which scenarios destructors can be skipped.

      builder().ReturnJump();
    }
  }
  CompleteDeferredBodies();
}

}  // namespace compiler

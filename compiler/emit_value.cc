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

void CompleteBody(Compiler *compiler,
                  ast::ParameterizedStructLiteral const *node, type::Type t) {
  NOT_YET();
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
    f->work_item =
        compiler->state()
            .deferred_work
            .emplace_back(std::make_unique<base::move_func<void()>>(
                [c = Compiler({.data                = context,
                               .diagnostic_consumer = compiler->diag(),
                               .importer            = compiler->importer()}),
                 node, t = f.type()]() mutable { CompleteBody(&c, node, t); }))
            .get();
  }
  return f;
}

std::vector<std::pair<ast::Expression const *, ir::Value>> EmitValueWithExpand(
    Compiler *c, base::PtrSpan<ast::Expression const> exprs) {
  // TODO expansion
  std::vector<std::pair<ast::Expression const *, ir::Value>> vals;
  for (auto *expr : exprs) { vals.emplace_back(expr, c->EmitValue(expr)); }
  return vals;
}

}  // namespace

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
    f->work_item =
        state_.deferred_work
            .emplace_back(std::make_unique<base::move_func<void()>>(
                [c = Compiler(resources_), node, t = f.type()]() mutable {
                  CompleteBody(&c, node, t);
                }))
            .get();
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
    f->work_item =
        state_.deferred_work
            .emplace_back(std::make_unique<base::move_func<void()>>(
                [c = Compiler(resources_), node, t = f.type()]() mutable {
                  CompleteBody(&c, node, t);
                }))
            .get();
  }
  return ir::Value(ir::Fn{f});
}

ir::Value Compiler::EmitValue(ast::Jump const *node) {
  LOG("Jump", "Emit %s", node->DebugString());
  // TODO: Check the result of body verification.
  if (context().ShouldVerifyBody(node)) { VerifyBody(node); }

  auto [jmp, inserted] = context().add_jump(node);
  if (inserted) {
    LOG("compile-work-queue", "Request work complete struct: %p", node);
    Enqueue({
        .kind      = WorkItem::Kind::EmitJumpBody,
        .node      = node,
        .resources = resources_,
    });
  }
  return ir::Value(jmp);
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

ir::Value Compiler::EmitValue(ast::ParameterizedStructLiteral const *node) {
  // TODO: Check the result of body verification.
  if (context().ShouldVerifyBody(node)) { VerifyBody(node); }
  return ir::Value(ir::GenericFn(
      [c = this->WithPersistent(),
       node](core::Arguments<type::Typed<ir::Value>> const &args) mutable
      -> ir::NativeFn { return MakeConcreteFromGeneric(&c, node, args); }));
}

WorkItem::Result Compiler::EmitJumpBody(ast::Jump const *node) {
  LOG("EmitJumpBody", "Jump %s", node->DebugString());
  ir::CompiledJump &jmp = *ASSERT_NOT_NULL(context().jump(node));

  ICARUS_SCOPE(ir::SetCurrent(jmp, builder())) {
    ASSERT(this != nullptr);
    builder().CurrentBlock() = jmp.entry();
    // TODO arguments should be renumbered to not waste space on const
    // values
    int32_t i = 0;
    if (node->state()) { context().set_addr(node->state(), ir::Reg::Arg(i++)); }
    for (auto const &param : node->params()) {
      context().set_addr(param.value.get(), ir::Reg::Arg(i++));
    }

    MakeAllStackAllocations(*this, node->body_scope());
    EmitIrForStatements(*this, node->stmts());

    // TODO: it seems like this will be appended after ChooseJump, which means
    // it'll never be executed.
    MakeAllDestructions(*this, node->body_scope());
  }
  jmp.WriteByteCode<interpreter::instruction_set_t>();
  return WorkItem::Result ::Success;
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

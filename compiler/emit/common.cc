#include "compiler/emit/common.h"

#include <vector>

#include "compiler/compiler.h"
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

ir::Fn InsertGeneratedMoveInit(Compiler &c, type::Struct *s) {
  auto [fn, inserted] = c.context().ir().InsertMoveInit(s, s);
  if (inserted) {
    c.set_builder(&*fn);
    absl::Cleanup cleanup      = [&] { c.state().builders.pop_back(); };
    c.builder().CurrentBlock() = c.builder().CurrentGroup()->entry();

    auto from = ir::Reg::Arg(0);
    auto to   = ir::Reg::Out(0);

    size_t i = 0;
    for (auto const &field : s->fields()) {
      auto to_ref =
          c.builder().CurrentBlock()->Append(ir::StructIndexInstruction{
              .addr        = to,
              .index       = i,
              .struct_type = s,
              .result      = c.builder().CurrentGroup()->Reserve()});
      auto from_val =
          c.builder().CurrentBlock()->Append(ir::StructIndexInstruction{
              .addr        = from,
              .index       = i,
              .struct_type = s,
              .result      = c.builder().CurrentGroup()->Reserve()});

      ir::RegOr<ir::addr_t> r(c.builder().PtrFix(from_val, field.type));
      ir::PartialResultBuffer buffer;
      buffer.append(r);
      c.EmitMoveInit(type::Typed<ir::Reg>(to_ref, field.type), buffer);
      ++i;
    }
    c.builder().ReturnJump();
    c.context().ir().WriteByteCode<EmitByteCode>(fn);
  }
  return fn;
}

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

ir::Fn InsertGeneratedCopyInit(Compiler &c, type::Struct *s) {
  auto [fn, inserted] = c.context().ir().InsertCopyInit(s, s);
  if (inserted) {
    c.set_builder(&*fn);
    absl::Cleanup cleanup = [&] { c.state().builders.pop_back(); };
    c.builder().CurrentBlock() = c.builder().CurrentGroup()->entry();

    auto from = ir::Reg::Arg(0);
    auto to   = ir::Reg::Out(0);

    size_t i = 0;
    for (auto const &field : s->fields()) {
      auto to_ref =
          c.builder().CurrentBlock()->Append(ir::StructIndexInstruction{
              .addr        = to,
              .index       = i,
              .struct_type = s,
              .result      = c.builder().CurrentGroup()->Reserve()});
      auto from_val =
          c.builder().CurrentBlock()->Append(ir::StructIndexInstruction{
              .addr        = from,
              .index       = i,
              .struct_type = s,
              .result      = c.builder().CurrentGroup()->Reserve()});

      ir::PartialResultBuffer buffer;
      buffer.append(c.builder().PtrFix(from_val, field.type));
      c.EmitCopyInit(type::Typed<ir::Reg>(to_ref, field.type), buffer);
      ++i;
    }
    c.builder().ReturnJump();
    c.context().ir().WriteByteCode<EmitByteCode>(fn);
  }
  return fn;
}

ir::Fn InsertGeneratedMoveAssign(Compiler &c, type::Struct *s) {
  auto [fn, inserted] = c.context().ir().InsertMoveAssign(s, s);
  if (inserted) {
    c.set_builder(&*fn);
    absl::Cleanup cleanup = [&] { c.state().builders.pop_back(); };
    c.builder().CurrentBlock() = fn->entry();
    auto var                   = ir::Reg::Arg(0);
    auto val                   = ir::Reg::Arg(1);

    for (size_t i = 0; i < s->fields().size(); ++i) {
      ir::Reg to_ref   = c.current_block()->Append(ir::StructIndexInstruction{
          .addr        = var,
          .index       = i,
          .struct_type = s,
          .result      = c.builder().CurrentGroup()->Reserve()});
      ir::Reg from_ref = c.current_block()->Append(ir::StructIndexInstruction{
          .addr        = val,
          .index       = i,
          .struct_type = s,
          .result      = c.builder().CurrentGroup()->Reserve()});

      ir::PartialResultBuffer buffer;
      buffer.append(c.builder().PtrFix(from_ref, s->fields()[i].type));
      c.EmitCopyAssign(
          type::Typed<ir::RegOr<ir::addr_t>>(to_ref, s->fields()[i].type),
          type::Typed(buffer[0], s->fields()[i].type));
    }

    c.builder().ReturnJump();
    c.context().ir().WriteByteCode<EmitByteCode>(fn);
  }
  return fn;
}

ir::Fn InsertGeneratedCopyAssign(Compiler &c, type::Struct *s) {
  auto [fn, inserted] = c.context().ir().InsertCopyAssign(s, s);
  if (inserted) {
    c.set_builder(&*fn);
    absl::Cleanup cleanup = [&] { c.state().builders.pop_back(); };
    c.builder().CurrentBlock() = fn->entry();
    auto var                   = ir::Reg::Arg(0);
    auto val                   = ir::Reg::Arg(1);

    for (size_t i = 0; i < s->fields().size(); ++i) {
      ir::Reg to_ref   = c.current_block()->Append(ir::StructIndexInstruction{
          .addr        = var,
          .index       = i,
          .struct_type = s,
          .result      = c.builder().CurrentGroup()->Reserve()});
      ir::Reg from_ref = c.current_block()->Append(ir::StructIndexInstruction{
          .addr        = val,
          .index       = i,
          .struct_type = s,
          .result      = c.builder().CurrentGroup()->Reserve()});
      ir::PartialResultBuffer buffer;
      buffer.append(c.builder().PtrFix(from_ref, s->fields()[i].type));
      c.EmitCopyAssign(
          type::Typed<ir::RegOr<ir::addr_t>>(to_ref, s->fields()[i].type),
          type::Typed(buffer[0], s->fields()[i].type));
    }

    c.builder().ReturnJump();
    c.context().ir().WriteByteCode<EmitByteCode>(fn);
  }
  return fn;
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
    Compiler &c, ast::Expression const *callable,
    core::Arguments<type::Typed<ir::CompleteResultRef>> const &constants) {
  Context const &context_root  = c.context().root();
  Context const &callable_root = callable->scope()
                                     ->Containing<ast::ModuleScope>()
                                     ->module()
                                     ->as<CompiledModule>()
                                     .context();
  // TODO: This is fraught, because we still don't have access to
  // instantiated contexts if that's what's needed here.
  type::QualType qt = (&context_root == &callable_root)
                          ? c.context().qual_types(callable)[0]
                          : callable_root.qual_types(callable)[0];

  if (auto const *gf_type = qt.type().if_as<type::Generic<type::Function>>()) {
    ir::GenericFn gen_fn = c.EmitAs<ir::GenericFn>(callable).value();

    // TODO: declarations aren't callable so we shouldn't have to check this
    // here.
    if (auto const *id = callable->if_as<ast::Declaration::Id>()) {
      // TODO: make this more robust.
      // TODO: support multiple declarations
      callable = id->declaration().init_val();
    }

    auto *parameterized_expr = &callable->as<ast::ParameterizedExpression>();
    auto find_subcontext_result =
        FindInstantiation(c, parameterized_expr, constants);
    return {.callee   = ir::Fn(gen_fn.concrete(c.work_resources(), constants)),
            .type     = find_subcontext_result.fn_type,
            .defaults = DefaultsFor(callable, find_subcontext_result.context),
            .context  = &find_subcontext_result.context};
  } else if (auto const *f_type = qt.type().if_as<type::Function>()) {
    if (type::Quals::Const() <= qt.quals()) {
      return {.callee   = c.EmitAs<ir::Fn>(callable),
              .type     = f_type,
              .defaults = DefaultsFor(callable, c.context()),
              .context  = nullptr};
    } else {
      // NOTE: If the overload is a declaration, it's not because a
      // declaration is syntactically the callee. Rather, it's because the
      // callee is an identifier (or module_name.identifier, etc.) and this
      // is one possible resolution of that identifier. We cannot directly
      // ask to emit IR for the declaration because that will emit the
      // initialization for the declaration. Instead, we need load the
      // address.
      if (auto *fn_decl = callable->if_as<ast::Declaration>()) {
        return {.callee = c.builder().Load<ir::Fn>(
                    c.state().addr(&fn_decl->ids()[0]), f_type),
                .type    = f_type,
                .context = nullptr};
      } else if (auto *fn_decl_id = callable->if_as<ast::Declaration::Id>()) {
        return {.callee = c.builder().Load<ir::Fn>(c.state().addr(fn_decl_id),
                                                   f_type),
                .type   = f_type,
                .context = nullptr};
      } else {
        return {.callee = c.builder().Load<ir::Fn>(
                    c.EmitAs<ir::addr_t>(callable), f_type),
                .type    = f_type,
                .context = nullptr};
      }
    }
  } else {
    UNREACHABLE(callable->DebugString(), "\n", qt.type().to_string());
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
  c.ApplyImplicitCasts(arg_qt.type(), to, buffer);
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

std::optional<ir::CompiledFn> StructCompletionFn(
    CompilationDataReference data, type::Struct *s,
    absl::Span<ast::Declaration const> field_decls) {
  ASSERT(s->completeness() == type::Completeness::DataComplete);

  ir::CompiledFn fn(type::Func({}, {}));
  data.set_builder(&fn);
  absl::Cleanup cleanup = [&] { data.state().builders.pop_back(); };
  Compiler c(data);
  // TODO this is essentially a copy of the body of
  // FunctionLiteral::EmitToBuffer. Factor these out together.
  data.builder().CurrentBlock() = fn.entry();

  std::vector<type::StructInstruction::Field> constants;
  bool needs_dtor = false;
  for (auto const &field : s->fields()) {
    needs_dtor = needs_dtor or field.type.get()->HasDestructor();
  }

  std::optional<ir::Fn> user_dtor;
  std::vector<ir::Fn> move_inits, copy_inits, move_assignments,
      copy_assignments;
  for (auto const &field_decl : field_decls) {
    if (not(field_decl.flags() & ast::Declaration::f_IsConst)) { continue; }
    VerifyType(data, &field_decl);

    // TODO: Access to init_val is not correct here because that may
    // initialize multiple values.
    for (auto const &id : field_decl.ids()) {
      // TODO: handle potential errors on each of these.
      if (id.name() == "destroy") {
        user_dtor = c.EmitAs<ir::Fn>(id.declaration().init_val()).value();
      } else if (id.name() == "move") {
        auto f = c.EmitAs<ir::Fn>(id.declaration().init_val());
        switch (f.value().type()->params().size()) {
          case 1: move_inits.push_back(f.value()); break;
          case 2: move_assignments.push_back(f.value()); break;
          default: UNREACHABLE();
        }
      } else if (id.name() == "copy") {
        auto f = c.EmitAs<ir::Fn>(id.declaration().init_val());
        switch (f.value().type()->params().size()) {
          case 1: copy_inits.push_back(f.value()); break;
          case 2: copy_assignments.push_back(f.value()); break;
          default: UNREACHABLE();
        }
      } else {
        if (auto const *init_val = id.declaration().init_val()) {
          // TODO init_val type may not be the same.
          type::Type field_type = data.context().qual_types(init_val)[0].type();

          ASSIGN_OR(NOT_YET(),  //
                    auto result,
                    data.EvaluateToBufferOrDiagnose(
                        type::Typed(init_val, field_type)));

          constants.emplace_back(id.name(), field_type, std::move(result))
              .set_export(
                  id.declaration().hashtags.contains(ir::Hashtag::Export));
        } else {
          // TODO: Failed evaluation
          // TODO: Type expression actually refers potentially to multiple
          // declaration ids.
          type::Type field_type = data.EvaluateOrDiagnoseAs<type::Type>(
                                          id.declaration().type_expr())
                                      .value();
          constants.emplace_back(id.name(), field_type)
              .set_export(
                  id.declaration().hashtags.contains(ir::Hashtag::Export));
        }
      }
    }
  }

  std::optional<ir::Fn> dtor;
  if (needs_dtor) {
    auto [full_dtor, inserted] = data.context().ir().InsertDestroy(s);
    if (inserted) {
      data.set_builder(&*full_dtor);
      absl::Cleanup cleanup         = [&] { c.state().builders.pop_back(); };
      data.builder().CurrentBlock() = data.builder().CurrentGroup()->entry();
      auto var                      = ir::Reg::Arg(0);
      if (user_dtor) {
        // TODO: Should probably force-inline this.
        ir::PartialResultBuffer args;
        args.append(var);
        // TODO: Constants
        data.builder().Call(*user_dtor, full_dtor.type(), std::move(args),
                            ir::OutParams());
      }
      for (int i = s->fields().size() - 1; i >= 0; --i) {
        // TODO: Avoid emitting IR if the type doesn't need to be
        // destroyed.
        c.EmitDestroy(type::Typed<ir::Reg>(data.builder().FieldRef(var, s, i)));
      }

      data.builder().ReturnJump();
      data.context().ir().WriteByteCode<EmitByteCode>(full_dtor);

      dtor = full_dtor;
    }
  } else {
    if (user_dtor) { dtor = *user_dtor; }
  }

  if (move_inits.empty() and copy_inits.empty()) {
    move_inits.push_back(InsertGeneratedMoveInit(c, s));
    copy_inits.push_back(InsertGeneratedCopyInit(c, s));
  }

  if (move_assignments.empty() and copy_assignments.empty()) {
    move_assignments.push_back(InsertGeneratedMoveAssign(c, s));
    copy_assignments.push_back(InsertGeneratedCopyAssign(c, s));
  }

  data.current_block()->Append(
      type::StructInstruction{.struct_          = s,
                              .constants        = std::move(constants),
                              .move_inits       = std::move(move_inits),
                              .copy_inits       = std::move(copy_inits),
                              .move_assignments = std::move(move_assignments),
                              .copy_assignments = std::move(copy_assignments),
                              .dtor             = dtor});
  data.builder().ReturnJump();

  return fn;
}

void MakeAllStackAllocations(Compiler &compiler, ast::FnScope const *fn_scope) {
  for (auto *scope : fn_scope->descendants()) {
    if (not scope->executable()) { continue; }
    if (scope != fn_scope and scope->is<ast::FnScope>()) { continue; }
    for (const auto &[key, val] : scope->decls_) {
      LOG("MakeAllStackAllocations", "%s", key);
      // TODO: Support multiple declarations
      for (ast::Declaration::Id const *id : val) {
        if (id->declaration().flags() &
            (ast::Declaration::f_IsConst | ast::Declaration::f_IsFnParam)) {
          LOG("MakeAllStackAllocations", "skipping constant/param decl %s",
              id->name());
          continue;
        }

        LOG("MakeAllStackAllocations", "allocating %s", id->name());

        compiler.state().set_addr(
            id, compiler.builder().Alloca(
                    compiler.context().qual_types(id)[0].type()));
      }
    }
  }
}

void MakeAllDestructions(Compiler &c, ast::Scope const *scope) {
  // TODO store these in the appropriate order so we don't have to compute this?
  // Will this be faster?
  std::vector<std::pair<ast::Declaration::Id const *, type::Type>>
      ordered_decl_ids;
  LOG("MakeAllDestructions", "decls in this scope:");
  for (auto &[name, ids] : scope->decls_) {
    if (ids[0]->declaration().flags() & ast::Declaration::f_IsConst) {
      continue;
    }

    LOG("MakeAllDestructions", "... %s", name);
    for (ast::Declaration::Id const *id : ids) {
      type::QualType qt = c.context().qual_types(id)[0];
      if (qt.constant()) { continue; }
      if (not qt.type().get()->HasDestructor()) { continue; }
      ordered_decl_ids.emplace_back(id, qt.type());
    }
  }

  // TODO eek, don't use line number to determine destruction order!
  absl::c_sort(ordered_decl_ids, [](auto const &lhs, auto const &rhs) {
    return (lhs.first->range().begin() > rhs.first->range().begin());
  });

  for (auto const &[id, t] : ordered_decl_ids) {
    c.EmitDestroy(type::Typed<ir::Reg>(c.state().addr(id).reg(), t));
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
    LOG("EmitIrForStatements", "%s", stmt->DebugString());
    buffer.clear();
    c.EmitToBuffer(stmt, buffer);
    c.DestroyTemporaries();
    LOG("EmitIrForStatements", "%p %s", c.builder().CurrentBlock(),
        *c.builder().CurrentGroup());
  }

  c.builder().CurrentBlock() = c.builder().EmitDestructionPath(scope, scope);
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

void EmitCall(Compiler &c, ast::Expression const *callee,
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
  c.builder().Call(callee_fn, overload_type, std::move(prepared_arguments),
                   out_params);
  int i = -1;
  for (type::Type t : overload_type->return_types()) {
    ++i;
    if (t.get()->is_big()) { continue; }

    ir::PartialResultBuffer buffer;
    buffer.append(out_params[i]);
    c.EmitCopyAssign(to[i], type::Typed(buffer[0], t));
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

}  // namespace compiler

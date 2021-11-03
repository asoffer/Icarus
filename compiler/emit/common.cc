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
namespace {

struct IncompleteField {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "incomplete-field";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Struct field has incomplete type."),
        diagnostic::SourceQuote(src).Highlighted(
            range, diagnostic::Style::ErrorText()));
  }

  frontend::SourceRange range;
};

ir::Fn InsertGeneratedMoveInit(
    Compiler &c, type::Struct *s,
    absl::Span<type::StructInstruction::Field const> ir_fields) {
  auto [fn, inserted] = c.context().ir().InsertMoveInit(s, s);
  if (inserted) {
    ICARUS_SCOPE(ir::SetCurrent(fn, c.builder())) {
      c.builder().CurrentBlock() = c.builder().CurrentGroup()->entry();

      auto from = ir::Reg::Arg(0);
      auto to   = ir::Reg::Out(0);

      size_t i = 0;
      for (auto const &field : ir_fields) {
        type::Type t = field.type().value();
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

        ir::RegOr<ir::addr_t> r(c.builder().PtrFix(from_val, t));
        ir::PartialResultBuffer buffer;
        buffer.append(r);
        c.EmitMoveInit(type::Typed<ir::Reg>(to_ref, t), buffer);
        ++i;
      }
      c.builder().ReturnJump();
    }
    c.context().ir().WriteByteCode<EmitByteCode>(fn);
  }
  return fn;
}

ir::OutParams SetReturns(
    ir::Builder &bldr, type::Type type,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  if (auto *fn_type = type.if_as<type::Function>()) {
    return bldr.OutParams(fn_type->output(), to);
  } else if (type.is<type::GenericFunction>()) {
    NOT_YET(type.to_string());
  } else {
    NOT_YET(type.to_string());
  }
}

ir::Fn InsertGeneratedCopyInit(
    Compiler &c, type::Struct *s,
    absl::Span<type::StructInstruction::Field const> ir_fields) {
  auto [fn, inserted] = c.context().ir().InsertCopyInit(s, s);
  if (inserted) {
    ICARUS_SCOPE(ir::SetCurrent(fn, c.builder())) {
      c.builder().CurrentBlock() = c.builder().CurrentGroup()->entry();

      auto from = ir::Reg::Arg(0);
      auto to   = ir::Reg::Out(0);

      size_t i = 0;
      for (auto const &field : ir_fields) {
        type::Type t = field.type().value();
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
        buffer.append(c.builder().PtrFix(from_val, t));
        c.EmitCopyInit(type::Typed<ir::Reg>(to_ref, t), buffer);
        ++i;
      }
      c.builder().ReturnJump();
    }
    c.context().ir().WriteByteCode<EmitByteCode>(fn);
  }
  return fn;
}

ir::Fn InsertGeneratedMoveAssign(
    Compiler &c, type::Struct *s,
    absl::Span<type::StructInstruction::Field const> ir_fields) {
  auto [fn, inserted] = c.context().ir().InsertMoveAssign(s, s);
  if (inserted) {
    ICARUS_SCOPE(ir::SetCurrent(fn, c.builder())) {
      c.builder().CurrentBlock() = fn->entry();
      auto var                   = ir::Reg::Arg(0);
      auto val                   = ir::Reg::Arg(1);

      for (size_t i = 0; i < ir_fields.size(); ++i) {
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
        buffer.append(
            c.builder().PtrFix(from_ref, ir_fields[i].type().value()));
        c.EmitCopyAssign(type::Typed<ir::RegOr<ir::addr_t>>(
                             to_ref, ir_fields[i].type().value()),
                         type::Typed(buffer[0], ir_fields[i].type().value()));
      }

      c.builder().ReturnJump();
    }
    c.context().ir().WriteByteCode<EmitByteCode>(fn);
  }
  return fn;
}

ir::Fn InsertGeneratedCopyAssign(
    Compiler &c, type::Struct *s,
    absl::Span<type::StructInstruction::Field const> ir_fields) {
  auto [fn, inserted] = c.context().ir().InsertCopyAssign(s, s);
  if (inserted) {
    ICARUS_SCOPE(ir::SetCurrent(fn, c.builder())) {
      c.builder().CurrentBlock() = fn->entry();
      auto var                   = ir::Reg::Arg(0);
      auto val                   = ir::Reg::Arg(1);

      for (size_t i = 0; i < ir_fields.size(); ++i) {
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
        buffer.append(
            c.builder().PtrFix(from_ref, ir_fields[i].type().value()));
        c.EmitCopyAssign(type::Typed<ir::RegOr<ir::addr_t>>(
                             to_ref, ir_fields[i].type().value()),
                         type::Typed(buffer[0], ir_fields[i].type().value()));
      }

      c.builder().ReturnJump();
    }
    c.context().ir().WriteByteCode<EmitByteCode>(fn);
  }
  return fn;
}

core::Params<ast::Expression const *> DefaultsFor(ast::Expression const *expr,
                                                  Context const &context) {
  if (auto const *id = expr->if_as<ast::Identifier>()) {
    auto decl_span = context.decls(id);
    switch (decl_span.size()) {
      case 0: UNREACHABLE();
      case 1: return DefaultsFor(decl_span[0]->init_val(), context);
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

CalleeResult EmitCalleeImpl(
    Compiler &c, ast::Expression const *callable, type::QualType qt,
    core::Arguments<type::Typed<ir::CompleteResultRef>> const &constants) {
  if (auto const *gf_type = qt.type().if_as<type::GenericFunction>()) {
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
    return {.callee   = ir::Fn(gen_fn.concrete(constants)),
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
                    c.builder().addr(&fn_decl->ids()[0])),
                .type    = f_type,
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

CalleeResult EmitCallee(
    Compiler &c, ast::Expression const *callee,
    core::Arguments<type::Typed<ir::CompleteResultRef>> const &constants) {
  CompiledModule *callee_mod = &callee->scope()
                                    ->Containing<ast::ModuleScope>()
                                    ->module()
                                    ->as<CompiledModule>();
  // Note: We only need to wait on the module if it's not this one, so even
  // though `callee_mod->context()` would be sufficient, we want to ensure that
  // we call the non-const overload if `callee_mod == &module()`.

  if (callee_mod == c.resources().module) {
    return EmitCalleeImpl(c, callee, c.context().qual_types(callee)[0],
                          constants);
  } else {
    type::QualType callee_qual_type =
        callee_mod->context().qual_types(callee)[0];

    Compiler callee_compiler(&callee_mod->context(), c.resources());

    return EmitCalleeImpl(callee_compiler, callee, callee_qual_type, constants);
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
        auto reg = c.builder().TmpAlloca(arg_qt.type());
        c.EmitMoveInit(&expr,
                       {type::Typed<ir::RegOr<ir::addr_t>>(reg, to.type())});
        buffer.append(reg);
      }
      return;
    }
  }

  c.EmitToBuffer(&expr, buffer);
  c.builder().ApplyImplicitCasts(arg_qt.type(), to, buffer);
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
      buffer.append(constants[name]);
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
    Compiler &c, type::Struct *s,
    absl::Span<ast::Declaration const> field_decls) {
  ASSERT(s->completeness() != type::Completeness::Complete);
  bool field_error = false;
  for (auto const &field_decl : field_decls) {
    for (auto const &id : field_decl.ids()) {
      type::QualType qt = c.context().qual_types(&id)[0];
      if (not qt or
          qt.type().get()->completeness() != type::Completeness::Complete) {
        c.diag().Consume(IncompleteField{.range = id.range()});
        field_error = true;
      }
    }
  }

  if (field_error) { return std::nullopt; }

  ir::CompiledFn fn(type::Func({}, {}));
  ICARUS_SCOPE(ir::SetCurrent(fn, c.builder())) {
    // TODO this is essentially a copy of the body of
    // FunctionLiteral::EmitToBuffer. Factor these out together.
    c.builder().CurrentBlock() = fn.entry();

    std::vector<type::StructInstruction::Field> ir_fields, constants;

    bool has_field_needing_destruction = false;
    std::optional<ir::Fn> user_dtor;
    std::vector<ir::Fn> move_inits, copy_inits, move_assignments,
        copy_assignments;
    for (auto const &field_decl : field_decls) {
      // TODO: Access to init_val is not correct here because that may
      // initialize multiple values.
      for (auto const &id : field_decl.ids()) {
        // TODO: Decide whether to support all hashtags. For now just covering
        // export.
        if (id.name() == "destroy") {
          // TODO: handle potential errors here.
          user_dtor = c.EmitAs<ir::Fn>(id.declaration().init_val()).value();
        } else if (id.name() == "move") {
          // TODO handle potential errors here.
          auto f = c.EmitAs<ir::Fn>(id.declaration().init_val());
          switch (f.value().type()->params().size()) {
            case 1: move_inits.push_back(f.value()); break;
            case 2: move_assignments.push_back(f.value()); break;
            default: UNREACHABLE();
          }
        } else if (id.name() == "copy") {
          // TODO handle potential errors here.
          auto f = c.EmitAs<ir::Fn>(id.declaration().init_val());
          switch (f.value().type()->params().size()) {
            case 1: copy_inits.push_back(f.value()); break;
            case 2: copy_assignments.push_back(f.value()); break;
            default: UNREACHABLE();
          }
        } else {
          type::Type field_type;
          auto &fields =
              (id.declaration().flags() & ast::Declaration::f_IsConst)
                  ? constants
                  : ir_fields;
          if (auto const *init_val = id.declaration().init_val()) {
            // TODO init_val type may not be the same.
            field_type = c.context().qual_types(init_val)[0].type();

            ASSIGN_OR(NOT_YET(),  //
                      auto result,
                      c.EvaluateToBufferOrDiagnose(
                          type::Typed(init_val, field_type)));

            fields.emplace_back(id.name(), field_type, std::move(result));
            fields.back().set_export(
                id.declaration().hashtags.contains(ir::Hashtag::Export));
          } else {
            // TODO: Failed evaluation
            // TODO: Type expression actually refers potentially to multiple
            // declaration ids.
            field_type =
                c.EvaluateOrDiagnoseAs<type::Type>(id.declaration().type_expr())
                    .value();
            fields.emplace_back(id.name(), field_type);
            fields.back().set_export(
                id.declaration().hashtags.contains(ir::Hashtag::Export));
          }
          has_field_needing_destruction = has_field_needing_destruction or
                                          field_type.get()->HasDestructor();
        }
      }
    }

    std::optional<ir::Fn> dtor;
    if (has_field_needing_destruction) {
      auto [full_dtor, inserted] = c.context().ir().InsertDestroy(s);
      if (inserted) {
        ICARUS_SCOPE(ir::SetCurrent(full_dtor, c.builder())) {
          c.builder().CurrentBlock() = c.builder().CurrentGroup()->entry();
          auto var                   = ir::Reg::Arg(0);
          if (user_dtor) {
            // TODO: Should probably force-inline this.
            ir::PartialResultBuffer args;
            args.append(var);
            // TODO: Constants
            c.builder().Call(*user_dtor, full_dtor.type(), std::move(args),
                             ir::OutParams());
          }
          for (int i = ir_fields.size() - 1; i >= 0; --i) {
            // TODO: Avoid emitting IR if the type doesn't need to be
            // destroyed.
            c.EmitDestroy(
                type::Typed<ir::Reg>(c.builder().FieldRef(var, s, i)));
          }

          c.builder().ReturnJump();
          c.context().ir().WriteByteCode<EmitByteCode>(full_dtor);

          dtor = full_dtor;
        }
      }
    } else {
      if (user_dtor) { dtor = *user_dtor; }
    }

    if (move_inits.empty() and copy_inits.empty()) {
      move_inits.push_back(InsertGeneratedMoveInit(c, s, ir_fields));
      copy_inits.push_back(InsertGeneratedCopyInit(c, s, ir_fields));
    }

    if (move_assignments.empty() and copy_assignments.empty()) {
      move_assignments.push_back(InsertGeneratedMoveAssign(c, s, ir_fields));
      copy_assignments.push_back(InsertGeneratedCopyAssign(c, s, ir_fields));
    }

    c.current_block()->Append(
        type::StructInstruction{.struct_          = s,
                                .constants        = std::move(constants),
                                .fields           = std::move(ir_fields),
                                .move_inits       = std::move(move_inits),
                                .copy_inits       = std::move(copy_inits),
                                .move_assignments = std::move(move_assignments),
                                .copy_assignments = std::move(copy_assignments),
                                .dtor             = dtor});
    c.builder().ReturnJump();
  }

  return fn;
}

bool Compiler::EnsureDataCompleteness(type::Struct *s) {
  if (s->completeness() >= type::Completeness::DataComplete) { return true; }

  ast::Expression const &expr = *ASSERT_NOT_NULL(context().ast_struct(s));
  // TODO: Deal with repetition between ast::StructLiteral and
  // ast::ParameterizedStructLiteral
  if (auto const *node = expr.if_as<ast::StructLiteral>()) {
    if (not VerifyBody(node)) { return false; }
    EmitVoid(node);

    LOG("struct", "Completing struct-literal emission: %p must-complete = %s",
        node, state_.must_complete ? "true" : "false");

    ASSIGN_OR(return false,  //
                     auto fn, StructCompletionFn(*this, s, node->fields()));
    // TODO: What if execution fails.
    InterpretAtCompileTime(fn);
    s->complete();
    LOG("struct", "Completed %s which is a struct %s with %u field(s).",
        node->DebugString(), *s, s->fields().size());
    return true;
  } else if (auto const *node = expr.if_as<ast::ParameterizedStructLiteral>()) {
    if (not VerifyBody(node)) { return false; }
    EmitVoid(node);

    LOG("struct", "Completing struct-literal emission: %p must-complete = %s",
        node, state_.must_complete ? "true" : "false");

    ASSIGN_OR(return false,  //
                     auto fn, StructCompletionFn(*this, s, node->fields()));
    // TODO: What if execution fails.
    InterpretAtCompileTime(fn);
    s->complete();
    LOG("struct", "Completed %s which is a struct %s with %u field(s).",
        node->DebugString(), *s, s->fields().size());
    return true;
  } else {
    // TODO Should we encode that it's one of these two in the type?
    NOT_YET();
  }
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

        compiler.builder().set_addr(
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
    c.EmitDestroy(type::Typed<ir::Reg>(c.builder().addr(id), t));
  }
}

// TODO One problem with this setup is that we don't end up calling destructors
// if we exit early, so those need to be handled externally.
void EmitIrForStatements(Compiler &c, base::PtrSpan<ast::Node const> stmts) {
  ICARUS_SCOPE(ir::SetTemporaries(c.builder())) {
    ir::PartialResultBuffer buffer;
    for (auto *stmt : stmts) {
      LOG("EmitIrForStatements", "%s", stmt->DebugString());
      buffer.clear();
      c.EmitToBuffer(stmt, buffer);
      c.builder().FinishTemporariesWith(
          [&c](type::Typed<ir::Reg> r) { c.EmitDestroy(r); });
      LOG("EmitIrForStatements", "%p %s", c.builder().CurrentBlock(),
          *c.builder().CurrentGroup());

      if (c.builder().block_termination_state() !=
          ir::Builder::BlockTerminationState::kMoreStatements) {
        break;
      }
    }
  }
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
  PersistentResources resources = c.resources();
  Compiler child = c.MakeChild(context ? context : &c.context(), resources);

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
  auto out_params = SetReturns(child.builder(), overload_type, to);
  c.builder().Call(callee_fn, overload_type, std::move(prepared_arguments),
                   out_params);
  int i = -1;
  for (type::Type t : overload_type->output()) {
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

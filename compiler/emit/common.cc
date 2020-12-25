#include <vector>

#include "compiler/compiler.h"
#include "core/arguments.h"
#include "core/params.h"
#include "ir/value/value.h"
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

ir::Fn InsertGeneratedMoveAssign(
    Compiler &c, type::Struct *s,
    absl::Span<type::StructInstruction::Field const> ir_fields) {
  auto [fn, inserted] = c.context().root().InsertMoveAssign(s, s);
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
        c.EmitMoveAssign(
            type::Typed<ir::RegOr<ir::Addr>>(to_ref,
                                             ir_fields[i].type().value()),
            type::Typed<ir::Value>(ir::Value(c.builder().PtrFix(
                                       from_ref, ir_fields[i].type().value())),
                                   ir_fields[i].type().value()));
      }

      c.builder().ReturnJump();
    }
    fn->WriteByteCode<interpreter::instruction_set_t>();
  }
  return fn;
}

ir::Fn InsertGeneratedCopyAssign(
    Compiler &c, type::Struct *s,
    absl::Span<type::StructInstruction::Field const> ir_fields) {
  auto [fn, inserted] = c.context().root().InsertCopyAssign(s, s);
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
        c.EmitCopyAssign(
            type::Typed<ir::RegOr<ir::Addr>>(to_ref,
                                             ir_fields[i].type().value()),
            type::Typed<ir::Value>(ir::Value(c.builder().PtrFix(
                                       from_ref, ir_fields[i].type().value())),
                                   ir_fields[i].type().value()));
      }

      c.builder().ReturnJump();
    }
    fn->WriteByteCode<interpreter::instruction_set_t>();
  }
  return fn;
}

}  // namespace

std::optional<ir::CompiledFn> StructCompletionFn(
    Compiler &c, type::Struct *s, absl::Span<ast::Declaration const> fields) {
  ASSERT(s->completeness() != type::Completeness::Complete);
  bool field_error = false;
  for (auto const &field : fields) {
    auto const *qt = c.context().qual_type(&field);
    if (not qt or
        qt->type().get()->completeness() != type::Completeness::Complete) {
      c.diag().Consume(IncompleteField{.range = field.range()});
      field_error = true;
    }
  }

  if (field_error) { return std::nullopt; }

  ir::CompiledFn fn(type::Func({}, {}),
                    core::Params<type::Typed<ast::Declaration const *>>{});
  ICARUS_SCOPE(ir::SetCurrent(fn, c.builder())) {
    // TODO this is essentially a copy of the body of FunctionLiteral::EmitValue
    // Factor these out together.
    c.builder().CurrentBlock() = fn.entry();

    std::vector<type::StructInstruction::Field> ir_fields;

    bool has_field_needing_destruction = false;
    std::optional<ir::Fn> user_dtor;
    std::vector<ir::Fn> move_assignments, copy_assignments;
    for (auto const &field : fields) {
      // TODO: Decide whether to support all hashtags. For now just covering
      // export.
      if (field.id() == "destroy") {
        // TODO: handle potential errors here.
        user_dtor = c.EmitValue(field.init_val()).get<ir::Fn>();
      } else if (field.id() == "move") {
        // TODO handle potential errors here.
        move_assignments.push_back(c.EmitValue(field.init_val()).get<ir::Fn>());
      } else if (field.id() == "copy") {
        // TODO handle potential errors here.
        copy_assignments.push_back(c.EmitValue(field.init_val()).get<ir::Fn>());
      } else {
        type::Type field_type;
        if (auto *init_val = field.init_val()) {
          // TODO init_val type may not be the same.
          field_type = c.context().qual_type(init_val)->type();
          ir_fields.emplace_back(field.id(), field_type, c.EmitValue(init_val));
          ir_fields.back().set_export(
              field.hashtags.contains(ir::Hashtag::Export));
        } else {
          // TODO: Failed evaluation
          field_type =
              c.EvaluateOrDiagnoseAs<type::Type>(field.type_expr()).value();
          ir_fields.emplace_back(field.id(), field_type);
          ir_fields.back().set_export(
              field.hashtags.contains(ir::Hashtag::Export));
        }
        has_field_needing_destruction =
            has_field_needing_destruction or field_type.get()->HasDestructor();
      }
    }

    std::optional<ir::Fn> dtor;
    if (has_field_needing_destruction) {
      auto [full_dtor, inserted] = c.context().InsertDestroy(s);
      if (inserted) {
        ICARUS_SCOPE(ir::SetCurrent(full_dtor, c.builder())) {
          c.builder().CurrentBlock() = c.builder().CurrentGroup()->entry();
          auto var                   = ir::Reg::Arg(0);
          if (user_dtor) {
            // TODO: Should probably force-inline this.
            c.builder().Call(*user_dtor, full_dtor.type(), {ir::Value(var)},
                             ir::OutParams());
          }
          for (int i = fields.size() - 1; i >= 0; --i) {
            // TODO: Avoid emitting IR if the type doesn't need to be
            // destroyed.
            c.EmitDestroy(
                type::Typed<ir::Reg>(c.builder().FieldRef(var, s, i)));
          }

          c.builder().ReturnJump();
          full_dtor->WriteByteCode<interpreter::instruction_set_t>();

          dtor = full_dtor;
        }
      }
    } else {
      if (user_dtor) { dtor = *user_dtor; }
    }

    if (move_assignments.empty() and copy_assignments.empty()) {
      move_assignments.push_back(InsertGeneratedMoveAssign(c, s, ir_fields));
      copy_assignments.push_back(InsertGeneratedCopyAssign(c, s, ir_fields));
    }

    c.current_block()->Append(
        type::StructInstruction{.struct_     = s,
                                .fields      = std::move(ir_fields),
                                .move_assignments = std::move(move_assignments),
                                .copy_assignments = std::move(copy_assignments),
                                .dtor        = dtor});
    c.builder().ReturnJump();
  }

  fn.WriteByteCode<interpreter::instruction_set_t>();

  return fn;
}

WorkItem::Result Compiler::EnsureDataCompleteness(type::Struct *s) {
  if (s->completeness() >= type::Completeness::DataComplete) {
    return WorkItem::Result::Success;
  }

  ast::Expression const &expr = *ASSERT_NOT_NULL(context().ast_struct(s));
  // TODO: Deal with repetition between ast::StructLiteral and
  // ast::ParameterizedStructLiteral
  if (auto const *node = expr.if_as<ast::StructLiteral>()) {
    if (auto result = VerifyBody(node); result != WorkItem::Result::Success) {
      return result;
    }
    EmitValue(node);

    LOG("struct", "Completing struct-literal emission: %p must-complete = %s",
        node, state_.must_complete ? "true" : "false");

    ASSIGN_OR(return WorkItem::Result::Failure,  //
                     auto fn, StructCompletionFn(*this, s, node->fields()));
    // TODO: What if execution fails.
    interpreter::Execute(std::move(fn));
    s->complete();
    LOG("struct", "Completed %s which is a struct %s with %u field(s).",
        node->DebugString(), *s, s->fields().size());
    return WorkItem::Result::Success;
  } else if (auto const *node = expr.if_as<ast::ParameterizedStructLiteral>()) {
    if (auto result = VerifyBody(node); result != WorkItem::Result::Success) {
      return result;
    }
    EmitValue(node);

    LOG("struct", "Completing struct-literal emission: %p must-complete = %s",
        node, state_.must_complete ? "true" : "false");

    ASSIGN_OR(return WorkItem::Result::Failure,  //
                     auto fn, StructCompletionFn(*this, s, node->fields()));
    // TODO: What if execution fails.
    interpreter::Execute(std::move(fn));
    s->complete();
    LOG("struct", "Completed %s which is a struct %s with %u field(s).",
        node->DebugString(), *s, s->fields().size());
    return WorkItem::Result::Success;
  } else {
    // TODO Should we encode that it's one of these two in the type?
    NOT_YET();
  }
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

void MakeAllDestructions(Compiler &compiler, ast::ExecScope const *exec_scope) {
  // TODO store these in the appropriate order so we don't have to compute this?
  // Will this be faster?
  std::vector<ast::Declaration *> ordered_decls;
  LOG("MakeAllDestructions", "decls in this scope:");
  for (auto &[name, decls] : exec_scope->decls_) {
    LOG("MakeAllDestructions", "... %s", name);
    ordered_decls.insert(ordered_decls.end(), decls.begin(), decls.end());
  }

  // TODO eek, don't use line number to determine destruction order!
  absl::c_sort(ordered_decls, [](ast::Declaration *lhs, ast::Declaration *rhs) {
    return (lhs->range().begin() > rhs->range().begin());
  });

  for (auto *decl : ordered_decls) {
    type::Type t = compiler.context().qual_type(decl)->type();
    if (not t.get()->HasDestructor()) { continue; }
    compiler.EmitDestroy(
        type::Typed<ir::Reg>(compiler.context().addr(decl), t));
  }
}

// TODO One problem with this setup is that we don't end up calling destructors
// if we exit early, so those need to be handled externally.
void EmitIrForStatements(Compiler &compiler,
                         base::PtrSpan<ast::Node const> stmts) {
  ICARUS_SCOPE(ir::SetTemporaries(compiler.builder())) {
    for (auto *stmt : stmts) {
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

ir::Value PrepareArgument(Compiler &compiler, ir::Value constant,
                          ast::Expression const *expr,
                          type::QualType param_qt) {
  type::QualType arg_qt = *compiler.context().qual_type(expr);
  type::Type arg_type   = arg_qt.type();
  type::Type param_type = param_qt.type();

  if (constant.empty()) {
    if (arg_type == param_type) {
      return compiler.EmitValue(expr);
    } else if (auto [bufptr_arg_type, ptr_param_type] =
                   std::make_pair(arg_type.if_as<type::BufferPointer>(),
                                  param_type.if_as<type::Pointer>());
               bufptr_arg_type and ptr_param_type and
               ptr_param_type->pointee() == bufptr_arg_type->pointee()) {
      return ir::Value(compiler.EmitValue(expr));
    } else if (auto const *ptr_param_type = param_type.if_as<type::Pointer>()) {
      if (ptr_param_type->pointee() == arg_type) {
        if (arg_qt.quals() >= type::Quals::Ref()) {
          return ir::Value(compiler.EmitRef(expr));
        } else {
          auto reg = compiler.builder().TmpAlloca(arg_type);
          compiler.EmitMoveInit(
              expr, {type::Typed<ir::RegOr<ir::Addr>>(reg, arg_type)});
          return ir::Value(reg);
        }
      } else {
        NOT_YET(arg_qt, " vs ", param_qt);
      }
    } else {
      NOT_YET(arg_qt, " vs ", param_qt);
    }
  } else {
    if (arg_type == param_type) {
      return constant;
    } else if (auto [bufptr_arg_type, ptr_param_type] =
                   std::make_pair(arg_type.if_as<type::BufferPointer>(),
                                  param_type.if_as<type::Pointer>());
               bufptr_arg_type and ptr_param_type and
               ptr_param_type->pointee() == bufptr_arg_type->pointee()) {
      return constant;
    } else if (auto const *ptr_param_type = param_type.if_as<type::Pointer>()) {
      if (ptr_param_type->pointee() == arg_type) {
        auto reg = compiler.builder().TmpAlloca(arg_type);
        compiler.EmitMoveInit(type::Typed<ir::Value>(constant, arg_type),
                              type::Typed<ir::Reg>(reg, arg_type));
        return ir::Value(reg);
      } else {
        NOT_YET(arg_qt, " vs ", param_qt);
      }
    } else {
      NOT_YET(arg_qt, " vs ", param_qt);
    }
  }
}

// TODO: A good amount of this could probably be reused from the above
// PrepareArgument overload.
ir::Value PrepareArgument(Compiler &compiler, ir::Value arg_value,
                          type::QualType arg_qt, type::QualType param_qt) {
  type::Type arg_type   = arg_qt.type();
  type::Type param_type = param_qt.type();

  if (arg_type == param_type) { return arg_value; }

  if (auto [bufptr_arg_type, ptr_param_type] =
          std::make_pair(arg_type.if_as<type::BufferPointer>(),
                         param_type.if_as<type::Pointer>());
      bufptr_arg_type and ptr_param_type and
      ptr_param_type->pointee() == bufptr_arg_type->pointee()) {
    return arg_value;
  }

  if (auto const *ptr_param_type = param_type.if_as<type::Pointer>()) {
    if (ptr_param_type->pointee() == arg_type) {
      auto reg = compiler.builder().TmpAlloca(arg_type);
      // TODO: Once EmitMoveInit is no longer a method on Compiler, we can
      // reduce the dependency here from being on Compiler to on Builder.
      compiler.EmitMoveInit(type::Typed<ir::Value>(arg_value, arg_type),
                            type::Typed<ir::Reg>(reg, arg_type));
      return ir::Value(reg);
    } else {
      NOT_YET(arg_qt, " vs ", param_qt);
    }
  } else {
    NOT_YET(arg_qt, " vs ", param_qt);
  }
}

core::Arguments<type::Typed<ir::Value>> EmitConstantArguments(
    Compiler &c, core::Arguments<ast::Expression const *> const &args) {
  return args.Transform([&](ast::Expression const *expr) {
    auto qt = *ASSERT_NOT_NULL(c.context().qual_type(expr));
    if (qt.constant()) {
      ir::Value result = c.EvaluateOrDiagnose(
          type::Typed<ast::Expression const *>(expr, qt.type()));
      if (result.empty()) { NOT_YET(); }
      return type::Typed<ir::Value>(result, qt.type());
    } else {
      return type::Typed<ir::Value>(ir::Value(), qt.type());
    }
  });
}

}  // namespace compiler

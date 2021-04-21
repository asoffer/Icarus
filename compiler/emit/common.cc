#include <vector>

#include "compiler/compiler.h"
#include "compiler/instructions.h"
#include "core/arguments.h"
#include "core/params.h"
#include "ir/byte_code_writer.h"
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

ir::Fn InsertGeneratedMoveInit(
    Compiler &c, type::Struct *s,
    absl::Span<type::StructInstruction::Field const> ir_fields) {
  auto [fn, inserted] = c.context().root().InsertMoveInit(s, s);
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

        c.EmitMoveInit(type::Typed<ir::Reg>(to_ref, t),
                       type::Typed<ir::Value>(
                           ir::Value(c.builder().PtrFix(from_val, t)), t));
        ++i;
      }
      c.builder().ReturnJump();
    }
    WriteByteCode(*fn.get());
  }
  return fn;
}

ir::Fn InsertGeneratedCopyInit(
    Compiler &c, type::Struct *s,
    absl::Span<type::StructInstruction::Field const> ir_fields) {
  auto [fn, inserted] = c.context().root().InsertCopyInit(s, s);
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

       c.EmitCopyInit(type::Typed<ir::Reg>(to_ref, t),
                      type::Typed<ir::Value>(
                          ir::Value(c.builder().PtrFix(from_val, t)), t));
       ++i;
      }
      c.builder().ReturnJump();
    }
    WriteByteCode(*fn.get());
  }
  return fn;
}

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
    WriteByteCode(*fn.get());
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
    WriteByteCode(*fn.get());
  }
  return fn;
}

}  // namespace

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

  ir::CompiledFn fn(type::Func({}, {}),
                    core::Params<type::Typed<ast::Declaration const *>>{});
  ICARUS_SCOPE(ir::SetCurrent(fn, c.builder())) {
    // TODO this is essentially a copy of the body of FunctionLiteral::EmitValue
    // Factor these out together.
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
          user_dtor = c.EmitValue(id.declaration().init_val()).get<ir::Fn>();
        } else if (id.name() == "move") {
          // TODO handle potential errors here.
          auto f = c.EmitValue(id.declaration().init_val()).get<ir::Fn>();
          switch (f.type()->params().size()) {
            case 1: move_inits.push_back(f); break;
            case 2: move_assignments.push_back(f); break;
            default: UNREACHABLE();
          }
        } else if (id.name() == "copy") {
          // TODO handle potential errors here.
          auto f = c.EmitValue(id.declaration().init_val()).get<ir::Fn>();
          switch (f.type()->params().size()) {
            case 1: copy_inits.push_back(f); break;
            case 2: copy_assignments.push_back(f); break;
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
            fields.emplace_back(id.name(), field_type, c.EmitValue(init_val));
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
          for (int i = ir_fields.size() - 1; i >= 0; --i) {
            // TODO: Avoid emitting IR if the type doesn't need to be
            // destroyed.
            c.EmitDestroy(
                type::Typed<ir::Reg>(c.builder().FieldRef(var, s, i)));
          }

          c.builder().ReturnJump();
          WriteByteCode(*full_dtor.get());

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

  WriteByteCode(fn);

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
    InterpretAtCompileTime(ir::NativeFn(&fn));
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
    InterpretAtCompileTime(ir::NativeFn(&fn));
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

        compiler.context().set_addr(
            id,
            compiler.builder().Alloca(compiler.context().qual_types(id)[0].type()));
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
    c.EmitDestroy(type::Typed<ir::Reg>(c.context().addr(id), t));
  }
}

// TODO One problem with this setup is that we don't end up calling destructors
// if we exit early, so those need to be handled externally.
void EmitIrForStatements(Compiler &c, base::PtrSpan<ast::Node const> stmts) {
  ICARUS_SCOPE(ir::SetTemporaries(c.builder())) {
    for (auto *stmt : stmts) {
      LOG("EmitIrForStatements", "%s", stmt->DebugString());
      c.EmitValue(stmt);
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

ir::Value PrepareArgument(Compiler &compiler, ir::Value constant,
                          ast::Expression const *expr,
                          type::QualType param_qt) {
  type::QualType arg_qt = *compiler.context().qual_types(expr)[0];
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
        compiler.EmitMoveInit(type::Typed<ir::Reg>(reg, arg_type),
                              type::Typed<ir::Value>(constant, arg_type));
        return ir::Value(reg);
      } else if (arg_type == type::NullPtr) {
        return ir::Value(ir::Addr::Null());
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

  if (arg_type == param_type) {
    if (auto const *r = arg_value.get_if<ir::Reg>()) {
      return ir::Value(compiler.builder().PtrFix(*r, param_type));
    } else {
      return arg_value;
    }
  }

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
      compiler.EmitMoveInit(type::Typed<ir::Reg>(reg, arg_type),
                            type::Typed<ir::Value>(arg_value, arg_type));
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
    auto qt = c.context().qual_types(expr)[0];
    if (qt.constant()) {
      if (qt.type().get()->is_big()) {
        // TODO: Implement constant computation here.
        return type::Typed<ir::Value>(ir::Value(), qt.type());
      } else {
        ir::Value result = c.EvaluateOrDiagnose(
            type::Typed<ast::Expression const *>(expr, qt.type()));
        if (result.empty()) { NOT_YET(); }
        return type::Typed<ir::Value>(result, qt.type());
      }
    } else {
      return type::Typed<ir::Value>(ir::Value(), qt.type());
    }
  });
}

core::Arguments<type::Typed<ir::Value>> EmitConstantArguments(
    Compiler &c, absl::Span<ast::Call::Argument const> args) {
  core::Arguments<type::Typed<ir::Value>> arg_vals;
  type::Typed<ir::Value> result;
  for (auto const &arg : args) {
    auto qt = c.context().qual_types(&arg.expr())[0];
    if (qt.constant()) {
      if (qt.type().get()->is_big()) {
        // TODO: Implement constant computation here.
        result = type::Typed<ir::Value>(ir::Value(), qt.type());
      } else {
        ir::Value value = c.EvaluateOrDiagnose(
            type::Typed<ast::Expression const *>(&arg.expr(), qt.type()));
        if (value.empty()) { NOT_YET(); }
        result = type::Typed<ir::Value>(value, qt.type());
      }
    } else {
      result = type::Typed<ir::Value>(ir::Value(), qt.type());
    }

    if (not arg.named()) {
      arg_vals.pos_emplace(result);
    } else {
      arg_vals.named_emplace(arg.name(), result);
    }
  }
  return arg_vals;
}

}  // namespace compiler

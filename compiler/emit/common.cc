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
  Context const &context_root = c.context().root();
  Context const &callable_root =
      ModuleFor(callable)->as<CompiledModule>().context();
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
        return {.callee  = c.current_block()->Append(ir::LoadInstruction{
                    .type   = f_type,
                    .addr   = c.state().addr(&fn_decl->ids()[0]),
                    .result = c.current().subroutine->Reserve()}),
                .type    = f_type,
                .context = nullptr};
      } else if (auto *fn_decl_id = callable->if_as<ast::Declaration::Id>()) {
        return {.callee  = c.current_block()->Append(ir::LoadInstruction{
                    .type   = f_type,
                    .addr   = c.state().addr(fn_decl_id),
                    .result = c.current().subroutine->Reserve()}),
                .type    = f_type,
                .context = nullptr};
      } else {
        return {.callee  = c.current_block()->Append(ir::LoadInstruction{
                    .type   = f_type,
                    .addr   = c.EmitAs<ir::addr_t>(callable),
                    .result = c.current().subroutine->Reserve()}),
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
    LOG("EmitIrForStatements", "%p %s", c.current_block(), *c.current().subroutine);
  }

  c.current_block() = c.EmitDestructionPath(scope, scope);
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
  c.current_block()->Append(ir::CallInstruction(
      overload_type, callee_fn, std::move(prepared_arguments), out_params));
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

void EmitCast(SubroutineBlockReference &ref, type::Type from, type::Type to,
              ir::PartialResultBuffer &buffer) {
#if defined(ICARUS_DEBUG)
  ASSERT(buffer.size() != 0u);
  absl::Cleanup c = [size = buffer.size(), &buffer] {
    ASSERT(size == buffer.size());
  };
#endif  // defined(ICARUS_DEBUG)

  // Ignore no-op conversions.
  if (to == from or type::CanCastInPlace(from, to)) { return; }

  // Allow any conversion to raw byte buffer pointers.
  if (from == type::Type(type::BufPtr(type::Byte)) or
      to == type::Type(type::BufPtr(type::Byte))) {
    return;
  }

  // TODO: We don't actually want to support casts to/from char. This should be
  // done explicitly with named builtin functions like `ascii_encode :: char ->
  // u8` and `ascii_decode :: u8 -> char`.
  if (to == type::Char) {
    if (from == type::U8) {
      EmitCast<uint8_t, ir::Char>(ref, buffer);
    } else if (from == type::I8) {
      EmitCast<int8_t, ir::Char>(ref, buffer);
    } else {
      UNREACHABLE(from);
    }
    return;
  } else if (from == type::Char) {
    ASSERT(type::IsIntegral(to) == true);
    ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t, uint32_t,
               uint64_t>(
        to, [&]<typename T>() { EmitCast<ir::Char, T>(ref, buffer); });
    return;
  }

  if (type::IsNumeric(from)) {
    if (auto const *enum_type = to.if_as<type::Enum>()) {
      ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t, uint32_t,
                 uint64_t>(from, [&]<typename T>() {
        EmitCast<T, type::Enum::underlying_type>(ref, buffer);
      });
    } else if (auto const *flags_type = to.if_as<type::Flags>()) {
      return ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t,
                        uint32_t, uint64_t>(from, [&]<typename T>() {
        EmitCast<T, type::Flags::underlying_type>(ref, buffer);
      });
    } else {
      ApplyTypes<ir::Integer, int8_t, int16_t, int32_t, int64_t, uint8_t,
                 uint16_t, uint32_t, uint64_t, float, double>(
          to, [&]<typename To>() {
            ApplyTypes<ir::Integer, int8_t, int16_t, int32_t, int64_t, uint8_t,
                       uint16_t, uint32_t, uint64_t, float, double>(
                from,
                [&]<typename From>() { EmitCast<From, To>(ref, buffer); });
          });
    }
    return;
  }

  if (from == type::NullPtr) {
    buffer.pop_back();
    buffer.append(ir::Null());
    return;
  }

  if (auto const *enum_type = from.if_as<type::Enum>()) {
    ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t, uint32_t,
               uint64_t>(to, [&]<typename T>() {
      EmitCast<type::Enum::underlying_type, T>(ref, buffer);
    });
    return;
  }

  if (auto const *flags_type = from.if_as<type::Flags>()) {
    return ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t,
                      uint32_t, uint64_t>(to, [&]<typename T>() {
      EmitCast<type::Flags::underlying_type, T>(ref, buffer);
    });
    return;
  }

  UNREACHABLE(from, " to ", to);
}

void EmitCast(Compiler &c, type::Typed<ast::Expression const *> node,
              type::Type to, ir::PartialResultBuffer &buffer) {
  c.EmitToBuffer(*node, buffer);
  EmitCast(c.current(), node.type(), to, buffer);
}

ir::PartialResultBuffer EmitCast(Compiler &c,
                                 type::Typed<ast::Expression const *> node,
                                 type::Type to) {
  ir::PartialResultBuffer buffer;
  EmitCast(c, node, to, buffer);
  return buffer;
}

ir::Reg RegisterReferencing(SubroutineBlockReference current, type::Type t,
                            ir::PartialResultRef const &value) {
  if (t.is_big() or t.is<type::Pointer>()) {
    return current.block->Append(ir::RegisterInstruction<ir::addr_t>{
        .operand = value.get<ir::addr_t>(),
        .result  = current.subroutine->Reserve(),
    });
  } else {
    if (auto const *p = t.if_as<type::Primitive>()) {
      return p->Apply([&]<typename T>() {
        return current.block->Append(ir::RegisterInstruction<T>{
            .operand = value.get<T>(),
            .result  = current.subroutine->Reserve(),
        });
      });
    } else if (auto const *e = t.if_as<type::Enum>()) {
      return current.block->Append(
          ir::RegisterInstruction<type::Enum::underlying_type>{
              .operand = value.get<type::Enum::underlying_type>(),
              .result  = current.subroutine->Reserve(),
          });
    } else if (auto const *e = t.if_as<type::Flags>()) {
      return current.block->Append(
          ir::RegisterInstruction<type::Flags::underlying_type>{
              .operand = value.get<type::Flags::underlying_type>(),
              .result  = current.subroutine->Reserve(),
          });
    } else {
      NOT_YET(t);
    }
  }
}

ir::Reg PtrFix(SubroutineBlockReference current, ir::RegOr<ir::addr_t> addr,
               type::Type desired_type) {
  // TODO must this be a register if it's loaded?
  if (desired_type.get()->is_big()) { return addr.reg(); }
  return current.block->Append(ir::LoadInstruction{
      .type   = desired_type,
      .addr   = addr,
      .result = current.subroutine->Reserve(),
  });
}

}  // namespace compiler

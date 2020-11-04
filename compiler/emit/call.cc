#include "ast/ast.h"
#include "base/meta.h"
#include "compiler/compiler.h"

namespace compiler {
namespace {

struct MoveInitTag {};
struct CopyInitTag {};
struct AssignTag {};

template <typename Tag>
ir::OutParams SetReturns(
    Tag, ir::Builder &bldr, type::Type type,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  if (auto *fn_type = type->if_as<type::Function>()) {
    if constexpr (base::meta<Tag> == base::meta<MoveInitTag>) {
      return bldr.OutParamsMoveInit(fn_type->output(), to);
    } else if constexpr (base::meta<Tag> == base::meta<CopyInitTag>) {
      return bldr.OutParamsCopyInit(fn_type->output(), to);
    } else if constexpr (base::meta<Tag> == base::meta<AssignTag>) {
      return bldr.OutParamsAssign(fn_type->output(), to);
    } else {
      static_assert(base::always_false<Tag>());
    }
  } else if (type->is<type::GenericFunction>()) {
    NOT_YET(type->to_string());
  } else {
    NOT_YET(type->to_string());
  }
}

ir::RegOr<ir::Fn> ComputeConcreteFn(Compiler *compiler,
                                    ast::Expression const *fn,
                                    type::Function const *f_type,
                                    type::Quals quals) {
  if (type::Quals::Const() <= quals) {
    return compiler->EmitValue(fn).get<ir::RegOr<ir::Fn>>();
  } else {
    // NOTE: If the overload is a declaration, it's not because a
    // declaration is syntactically the callee. Rather, it's because the
    // callee is an identifier (or module_name.identifier, etc.) and this
    // is one possible resolution of that identifier. We cannot directly
    // ask to emit IR for the declaration because that will emit the
    // initialization for the declaration. Instead, we need load the
    // address.
    if (auto *fn_decl = fn->if_as<ast::Declaration>()) {
      return compiler->builder().Load<ir::Fn>(
          compiler->context().addr(fn_decl));
    } else {
      return compiler->builder().Load<ir::Fn>(
          compiler->EmitValue(fn).get<ir::RegOr<ir::Addr>>());
    }
  }
}

std::tuple<ir::RegOr<ir::Fn>, type::Function const *, Context *> EmitCallee(
    Compiler &compiler, ast::Expression const *fn, type::QualType qt,
    const core::Arguments<type::Typed<ir::Value>> &args) {
  if (auto const *gf_type = qt.type()->if_as<type::GenericFunction>()) {
    ir::GenericFn gen_fn =
        compiler.EmitValue(fn).get<ir::RegOr<ir::GenericFn>>().value();

    // TODO: declarations aren't callable so we shouldn't have to check this
    // here.
    if (auto const *decl = fn->if_as<ast::Declaration>()) {
      // TODO: make this more robust.
      fn = decl->init_val();
    }

    auto *parameterized_expr = &fn->as<ast::ParameterizedExpression>();

    auto find_subcontext_result =
        compiler.FindInstantiation(parameterized_expr, args);
    return std::make_tuple(ir::Fn(gen_fn.concrete(args)),
                           find_subcontext_result.fn_type,
                           &find_subcontext_result.context);
  } else if (auto const *f_type = qt.type()->if_as<type::Function>()) {
    return std::make_tuple(ComputeConcreteFn(&compiler, fn, f_type, qt.quals()),
                           f_type, nullptr);
  } else {
    UNREACHABLE(fn->DebugString(), "\n", qt.type()->to_string());
  }
}

template <typename Tag>
void EmitCall(Tag, Compiler &compiler, ast::Expression const *callee,
              core::Arguments<type::Typed<ir::Value>> args,
              absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  auto callee_qual_type = compiler.qual_type_of(callee);
  ASSERT(callee_qual_type.has_value() == true);

  auto [callee_fn, overload_type, dependent_data] =
      EmitCallee(compiler, callee, *callee_qual_type, args);

  Compiler c = compiler.MakeChild(Compiler::PersistentResources{
      .data = dependent_data ? *dependent_data : compiler.context(),
      .diagnostic_consumer = compiler.diag(),
      .importer            = compiler.importer(),
  });

  if (not callee_fn.is_reg()) {
    switch (callee_fn.value().kind()) {
      case ir::Fn::Kind::Native: {
        core::FillMissingArgs(
            core::ParamsRef(callee_fn.value().native()->params()), &args,
            [&c](auto const &p) {
              return type::Typed<ir::Value>(
                  c.EmitValue(ASSERT_NOT_NULL(p.get()->init_val())), p.type());
            });
      } break;
      default: break;
    }
  }

  auto out_params = SetReturns(Tag{}, c.builder(), overload_type, to);
  c.builder().Call(
      callee_fn, overload_type,
      c.PrepareCallArguments(nullptr, overload_type->params(), args),
      out_params);
  int i = -1;
  for (auto t : overload_type->output()) {
    ++i;
    if (t->is_big()) continue;
    c.EmitCopyAssign(to[i],
                     type::Typed<ir::Value>(ir::Value(out_params[i]), t));
  }
}

// TODO: Checking if an AST node is a builtin is problematic because something
// as simple as
// ```
// f ::= bytes
// f(int)
// ```
// breaks.
//
ir::Value EmitBuiltinCall(
    Compiler *c, ast::BuiltinFn const *callee,
    core::Arguments<ast::Expression const *> const &args) {
  switch (callee->value().which()) {
    case ir::BuiltinFn::Which::Foreign: {
      auto maybe_name         = c->EvaluateOrDiagnoseAs<ir::String>(args[0]);
      auto maybe_foreign_type = c->EvaluateOrDiagnoseAs<type::Type>(args[1]);
      if (not maybe_name or not maybe_foreign_type) { return ir::Value(); }

      return ir::Value(
          c->builder().LoadSymbol(*maybe_name, *maybe_foreign_type).get());
    } break;

    case ir::BuiltinFn::Which::Opaque:
      return ir::Value(c->builder().OpaqueType(&c->context().module()));
    case ir::BuiltinFn::Which::Bytes: {
      auto const &fn_type = *ir::BuiltinFn::Bytes().type();
      ir::OutParams outs  = c->builder().OutParams(fn_type.output());
      ir::Reg reg         = outs[0];
      c->builder().Call(
          ir::Fn{ir::BuiltinFn::Bytes()}, &fn_type,
          {ir::Value(c->EmitValue(args[0]).get<ir::RegOr<type::Type>>())},
          std::move(outs));

      return ir::Value(reg);
    } break;

    case ir::BuiltinFn::Which::Alignment: {
      auto const &fn_type = *ir::BuiltinFn::Alignment().type();
      ir::OutParams outs  = c->builder().OutParams(fn_type.output());
      ir::Reg reg         = outs[0];
      c->builder().Call(
          ir::Fn{ir::BuiltinFn::Alignment()}, &fn_type,
          {ir::Value(c->EmitValue(args[0]).get<ir::RegOr<type::Type>>())},
          std::move(outs));

      return ir::Value(reg);
    } break;

    case ir::BuiltinFn::Which::DebugIr:
      c->builder().DebugIr();
      return ir::Value();
  }
  UNREACHABLE();
}

}  // namespace

ir::Value Compiler::EmitValue(ast::Call const *node) {
  if (auto *b = node->callee()->if_as<ast::BuiltinFn>()) {
    return EmitBuiltinCall(this, b, node->args());
  }

  auto args = node->args().Transform([this](ast::Expression const *expr) {
    return type::Typed<ir::Value>(EmitValue(expr),
                                  context().qual_type(expr)->type());
  });

  // TODO: Support generic structs.
  auto qt = *ASSERT_NOT_NULL(context().qual_type(node));

  auto const &os = context().ViableOverloads(node->callee());
  ASSERT(os.members().size() == 1u);  // TODO: Support dynamic dispatch.
  switch (qt.expansion_size()) {
    case 0:
      EmitCall(MoveInitTag{}, *this, os.members().front(), args, {});
      return ir::Value();
    case 1: {
      // TODO: It'd be nice to not stack-allocate register-sized values.
      type::Typed<ir::RegOr<ir::Addr>> out(builder().TmpAlloca(qt.type()),
                                           qt.type());
      EmitCall(MoveInitTag{}, *this, os.members().front(), args,
               absl::MakeConstSpan(&out, 1));
      return ir::Value(builder().PtrFix(out->reg(), qt.type()));
    }
    default: NOT_YET();
  }
}

void Compiler::EmitMoveInit(
    ast::Call const *node,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  if (auto *b = node->callee()->if_as<ast::BuiltinFn>()) {
    auto result = EmitBuiltinCall(this, b, node->args());
    if (result.empty()) return;
    EmitCopyAssign(to[0], type::Typed<ir::Value>(
                              result, context().qual_type(node)->type()));
  }

  auto args = node->args().Transform([this](ast::Expression const *expr) {
    return type::Typed<ir::Value>(EmitValue(expr),
                                  context().qual_type(expr)->type());
  });

  auto const &os = context().ViableOverloads(node->callee());
  ASSERT(os.members().size() == 1u);  // TODO: Support dynamic dispatch.
  EmitCall(MoveInitTag{}, *this, os.members().front(), args, to);
}

void Compiler::EmitCopyInit(
    ast::Call const *node,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  if (auto *b = node->callee()->if_as<ast::BuiltinFn>()) {
    auto result = EmitBuiltinCall(this, b, node->args());
    if (result.empty()) return;
    EmitCopyAssign(to[0], type::Typed<ir::Value>(
                              result, context().qual_type(node)->type()));
  }

  auto args = node->args().Transform([this](ast::Expression const *expr) {
    return type::Typed<ir::Value>(EmitValue(expr),
                                  context().qual_type(expr)->type());
  });

  auto const &os = context().ViableOverloads(node->callee());
  ASSERT(os.members().size() == 1u);  // TODO: Support dynamic dispatch.
  return EmitCall(CopyInitTag{}, *this, os.members().front(), args, to);
}

void Compiler::EmitAssign(
    ast::Call const *node,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  if (auto *b = node->callee()->if_as<ast::BuiltinFn>()) {
    auto result = EmitBuiltinCall(this, b, node->args());
    if (result.empty()) return;
    EmitCopyAssign(to[0], type::Typed<ir::Value>(
                              result, context().qual_type(node)->type()));
  }

  auto args = node->args().Transform([this](ast::Expression const *expr) {
    return type::Typed<ir::Value>(EmitValue(expr),
                                  context().qual_type(expr)->type());
  });

  auto const &os = context().ViableOverloads(node->callee());
  ASSERT(os.members().size() == 1u);  // TODO: Support dynamic dispatch.

  return EmitCall(AssignTag{}, *this, os.members().front(), args, to);
}

}  // namespace compiler

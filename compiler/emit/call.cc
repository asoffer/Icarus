#include "ast/ast.h"
#include "base/meta.h"
#include "compiler/compiler.h"
#include "compiler/emit/common.h"
#include "compiler/resources.h"

namespace compiler {
namespace {

struct MoveInitTag {};
struct CopyInitTag {};
struct AssignTag {};

template <typename Tag>
ir::OutParams SetReturns(
    Tag, ir::Builder &bldr, type::Type type,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  if (auto *fn_type = type.if_as<type::Function>()) {
    if constexpr (base::meta<Tag> == base::meta<MoveInitTag>) {
      return bldr.OutParamsMoveInit(fn_type->output(), to);
    } else if constexpr (base::meta<Tag> == base::meta<CopyInitTag>) {
      return bldr.OutParamsCopyInit(fn_type->output(), to);
    } else if constexpr (base::meta<Tag> == base::meta<AssignTag>) {
      return bldr.OutParamsAssign(fn_type->output(), to);
    } else {
      static_assert(base::always_false<Tag>());
    }
  } else if (type.is<type::GenericFunction>()) {
    NOT_YET(type.to_string());
  } else {
    NOT_YET(type.to_string());
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
    const core::Arguments<type::Typed<ir::Value>> &constant_arguments) {
  if (auto const *gf_type = qt.type().if_as<type::GenericFunction>()) {
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
        compiler.FindInstantiation(parameterized_expr, constant_arguments);
    return std::make_tuple(ir::Fn(gen_fn.concrete(constant_arguments)),
                           find_subcontext_result.fn_type,
                           &find_subcontext_result.context);
  } else if (auto const *f_type = qt.type().if_as<type::Function>()) {
    return std::make_tuple(ComputeConcreteFn(&compiler, fn, f_type, qt.quals()),
                           f_type, nullptr);
  } else {
    UNREACHABLE(fn->DebugString(), "\n", qt.type().to_string());
  }
}

template <typename Tag>
void EmitCall(Tag, Compiler &compiler, ast::Expression const *callee,
              core::Arguments<type::Typed<ir::Value>> const &constant_arguments,
              core::Arguments<ast::Expression const *> const &arg_exprs,
              absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  auto callee_qual_type =
      *ASSERT_NOT_NULL(compiler.context().qual_type(callee));

  auto [callee_fn, overload_type, context] =
      EmitCallee(compiler, callee, callee_qual_type, constant_arguments);

  Compiler c = compiler.MakeChild(PersistentResources{
      .data                = context ? *context : compiler.context(),
      .diagnostic_consumer = compiler.diag(),
      .importer            = compiler.importer(),
  });

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
  std::vector<ir::Value> prepared_arguments;

  auto const &param_qts = overload_type->params();

  // TODO: With expansions, args().pos().size() could be wrong.
  for (size_t i = 0; i < arg_exprs.pos().size(); ++i) {
    prepared_arguments.push_back(PrepareArgument(
        compiler, *constant_arguments[i], arg_exprs[i], param_qts[i].value));
  }

  for (size_t i = arg_exprs.pos().size(); i < param_qts.size(); ++i) {
    std::string_view name                = param_qts[i].name;
    auto const *constant_typed_value     = constant_arguments.at_or_null(name);
    auto const *expr                     = arg_exprs.at_or_null(name);
    ast::Expression const *default_value = nullptr;

    if (not expr) {
      ASSERT(callee_fn.is_reg() == false);
      ASSERT(callee_fn.value().kind() == ir::Fn::Kind::Native);
      default_value = ASSERT_NOT_NULL(
          callee_fn.value().native()->params()[i].value.get()->init_val());
    }

    prepared_arguments.push_back(PrepareArgument(
        compiler, constant_typed_value ? **constant_typed_value : ir::Value(),
        expr ? *expr : default_value, param_qts[i].value));
  }

  auto out_params = SetReturns(Tag{}, c.builder(), overload_type, to);
  compiler.builder().Call(callee_fn, overload_type,
                          std::move(prepared_arguments), out_params);
  int i = -1;
  for (auto t : overload_type->output()) {
    ++i;
    if (t.get()->is_big()) { continue; }
    compiler.EmitCopyAssign(
        to[i], type::Typed<ir::Value>(ir::Value(out_params[i]), t));
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
      auto name_buffer = c->EvaluateToBufferOrDiagnose(
          type::Typed<ast::Expression const *>(args[0], type::Slc(type::Char)));
      if (name_buffer.empty()) { return ir::Value(); }

      auto maybe_foreign_type = c->EvaluateOrDiagnoseAs<type::Type>(args[1]);
      if (not maybe_foreign_type) { return ir::Value(); }
      auto slice = name_buffer.get<ir::Slice>(0);

      std::string name(ir::ReadOnlyData.lock()->raw(slice.data().rodata()),
                       slice.length());
      return ir::Value(
          c->builder().LoadSymbol(ir::String(name), *maybe_foreign_type).get());
    } break;

    case ir::BuiltinFn::Which::Opaque:
      return ir::Value(c->current_block()->Append(type::OpaqueTypeInstruction{
          .mod    = &c->context().module(),
          .result = c->builder().CurrentGroup()->Reserve()}));

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

  // Constant arguments need to be computed entirely before being used to
  // instantiate a generic function.
  core::Arguments<type::Typed<ir::Value>> constant_arguments =
      EmitConstantArguments(*this, node->args());

  // TODO: Support mixed overloads
  if (auto const *gs_type = context()
                                .qual_type(node->callee())
                                ->type()
                                .if_as<type::GenericStruct>()) {
    return ir::Value(
        type::Type(gs_type->Instantiate(constant_arguments).second));
  }

  auto qt = *ASSERT_NOT_NULL(context().qual_type(node));

  auto const &os = context().ViableOverloads(node->callee());
  ASSERT(os.members().size() == 1u);  // TODO: Support dynamic dispatch.
  switch (qt.expansion_size()) {
    case 0:
      EmitCall(MoveInitTag{}, *this, os.members().front(), constant_arguments,
               node->args(), {});
      return ir::Value();
    case 1: {
      // TODO: It'd be nice to not stack-allocate register-sized values.
      type::Typed<ir::RegOr<ir::Addr>> out(builder().TmpAlloca(qt.type()),
                                           qt.type());
      EmitCall(MoveInitTag{}, *this, os.members().front(), constant_arguments,
               node->args(), absl::MakeConstSpan(&out, 1));
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

  // Constant arguments need to be computed entirely before being used to
  // instantiate a generic function.
  core::Arguments<type::Typed<ir::Value>> constant_arguments =
      EmitConstantArguments(*this, node->args());

  // TODO: Support mixed overloads
  if (auto const *gs_type = context()
                                .qual_type(node->callee())
                                ->type()
                                .if_as<type::GenericStruct>()) {
    EmitCopyAssign(to[0],
                   type::Typed<ir::Value>(
                       ir::Value(type::Type(
                           gs_type->Instantiate(constant_arguments).second)),
                       type::Type_));
    return;
  }

  auto const &os = context().ViableOverloads(node->callee());
  ASSERT(os.members().size() == 1u);  // TODO: Support dynamic dispatch.
  EmitCall(MoveInitTag{}, *this, os.members().front(), constant_arguments,
           node->args(), to);
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

  // Constant arguments need to be computed entirely before being used to
  // instantiate a generic function.
  core::Arguments<type::Typed<ir::Value>> constant_arguments =
      EmitConstantArguments(*this, node->args());

  // TODO: Support mixed overloads
  if (auto const *gs_type = context()
                                .qual_type(node->callee())
                                ->type()
                                .if_as<type::GenericStruct>()) {
    EmitCopyAssign(to[0],
                   type::Typed<ir::Value>(
                       ir::Value(type::Type(
                           gs_type->Instantiate(constant_arguments).second)),
                       type::Type_));
    return;
  }

  auto const &os = context().ViableOverloads(node->callee());
  ASSERT(os.members().size() == 1u);  // TODO: Support dynamic dispatch.
  return EmitCall(CopyInitTag{}, *this, os.members().front(),
                  constant_arguments, node->args(), to);
}

void Compiler::EmitMoveAssign(
    ast::Call const *node,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  if (auto *b = node->callee()->if_as<ast::BuiltinFn>()) {
    auto result = EmitBuiltinCall(this, b, node->args());
    if (result.empty()) return;
    EmitMoveAssign(to[0], type::Typed<ir::Value>(
                              result, context().qual_type(node)->type()));
  }

  // Constant arguments need to be computed entirely before being used to
  // instantiate a generic function.
  core::Arguments<type::Typed<ir::Value>> constant_arguments =
      EmitConstantArguments(*this, node->args());

  // TODO: Support mixed overloads
  if (auto const *gs_type = context()
                                .qual_type(node->callee())
                                ->type()
                                .if_as<type::GenericStruct>()) {
    EmitMoveAssign(to[0],
                   type::Typed<ir::Value>(
                       ir::Value(type::Type(
                           gs_type->Instantiate(constant_arguments).second)),
                       type::Type_));
  }

  auto const &os = context().ViableOverloads(node->callee());
  ASSERT(os.members().size() == 1u);  // TODO: Support dynamic dispatch.

  return EmitCall(AssignTag{}, *this, os.members().front(), constant_arguments,
                  node->args(), to);
}

void Compiler::EmitCopyAssign(
    ast::Call const *node,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  if (auto *b = node->callee()->if_as<ast::BuiltinFn>()) {
    auto result = EmitBuiltinCall(this, b, node->args());
    if (result.empty()) return;
    EmitCopyAssign(to[0], type::Typed<ir::Value>(
                              result, context().qual_type(node)->type()));
  }

  // Constant arguments need to be computed entirely before being used to
  // instantiate a generic function.
  core::Arguments<type::Typed<ir::Value>> constant_arguments =
      EmitConstantArguments(*this, node->args());
  // TODO: Support mixed overloads
  if (auto const *gs_type = context()
                                .qual_type(node->callee())
                                ->type()
                                .if_as<type::GenericStruct>()) {
    EmitCopyAssign(to[0],
                   type::Typed<ir::Value>(
                       ir::Value(type::Type(
                           gs_type->Instantiate(constant_arguments).second)),
                       type::Type_));
  }

  auto const &os = context().ViableOverloads(node->callee());
  ASSERT(os.members().size() == 1u);  // TODO: Support dynamic dispatch.

  return EmitCall(AssignTag{}, *this, os.members().front(), constant_arguments,
                  node->args(), to);
}

}  // namespace compiler

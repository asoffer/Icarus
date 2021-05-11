#include "ast/ast.h"
#include "base/meta.h"
#include "compiler/compiler.h"
#include "compiler/emit/common.h"
#include "compiler/resources.h"
#include "ir/instruction/instructions.h"
#include "type/interface/ir.h"

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

ir::RegOr<ir::Fn> ComputeConcreteFn(Compiler &c, ast::Expression const *fn,
                                    type::Function const *f_type,
                                    type::Quals quals) {
  if (type::Quals::Const() <= quals) {
    return c.EmitValue(fn).get<ir::RegOr<ir::Fn>>();
  } else {
    // NOTE: If the overload is a declaration, it's not because a
    // declaration is syntactically the callee. Rather, it's because the
    // callee is an identifier (or module_name.identifier, etc.) and this
    // is one possible resolution of that identifier. We cannot directly
    // ask to emit IR for the declaration because that will emit the
    // initialization for the declaration. Instead, we need load the
    // address.
    if (auto *fn_decl = fn->if_as<ast::Declaration>()) {
      return c.builder().Load<ir::Fn>(c.context().addr(&fn_decl->ids()[0]));
    } else {
      return c.builder().Load<ir::Fn>(
          c.EmitValue(fn).get<ir::RegOr<ir::Addr>>(), f_type);
    }
  }
}

std::tuple<ir::RegOr<ir::Fn>, type::Function const *, Context *> EmitCallee(
    Compiler &c, ast::Expression const *fn, type::QualType qt,
    const core::Arguments<type::Typed<ir::Value>> &constant_arguments) {
  if (auto const *gf_type = qt.type().if_as<type::GenericFunction>()) {
    ir::GenericFn gen_fn =
        c.EmitValue(fn).get<ir::RegOr<ir::GenericFn>>().value();

    // TODO: declarations aren't callable so we shouldn't have to check this
    // here.
    if (auto const *id = fn->if_as<ast::Declaration::Id>()) {
      // TODO: make this more robust.
      // TODO: support multiple declarations
      fn = id->declaration().init_val();
    }

    auto *parameterized_expr = &fn->as<ast::ParameterizedExpression>();

    auto find_subcontext_result =
        c.FindInstantiation(parameterized_expr, constant_arguments);
    return std::make_tuple(ir::Fn(gen_fn.concrete(constant_arguments)),
                           find_subcontext_result.fn_type,
                           &find_subcontext_result.context);
  } else if (auto const *f_type = qt.type().if_as<type::Function>()) {
    return std::make_tuple(ComputeConcreteFn(c, fn, f_type, qt.quals()), f_type,
                           nullptr);
  } else {
    UNREACHABLE(fn->DebugString(), "\n", qt.type().to_string());
  }
}

template <typename Tag>
void EmitCall(Tag, Compiler &compiler, ast::Expression const *callee,
              core::Arguments<type::Typed<ir::Value>> const &constant_arguments,
              absl::Span<ast::Call::Argument const> arg_exprs,
              absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  CompiledModule *callee_mod = &callee->scope()
                                    ->Containing<ast::ModuleScope>()
                                    ->module()
                                    ->as<CompiledModule>();
  // Note: We only need to wait on the module if it's not this one, so even
  // though `callee_mod->context()` would be sufficient, we want to ensure that
  // we call the non-const overload if `callee_mod == &module()`.

  std::tuple<ir::RegOr<ir::Fn>, type::Function const *, Context *> results;
  if (callee_mod == &compiler.context().module()) {
    results =
        EmitCallee(compiler, callee, compiler.context().qual_types(callee)[0],
                   constant_arguments);
  } else {
    type::QualType callee_qual_type =
        callee_mod->context(&compiler.context().module()).qual_types(callee)[0];

    Compiler callee_compiler(PersistentResources{
        .data = callee_mod->context(&compiler.context().module()),
        .diagnostic_consumer = compiler.diag(),
        .importer            = compiler.importer(),
    });
    results = EmitCallee(callee_compiler, callee, callee_qual_type,
                         constant_arguments);
  }

  auto &[callee_fn, overload_type, context] = results;
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

  // TODO: With expansions, this might be wrong.
  {
    size_t i = 0;
    for (; i < arg_exprs.size() and not arg_exprs[i].named(); ++i) {
      prepared_arguments.push_back(
          PrepareArgument(compiler, *constant_arguments[i],
                          &arg_exprs[i].expr(), param_qts[i].value));
    }

    absl::flat_hash_map<std::string_view, ast::Expression const *> named;
    for (size_t j = i; j < arg_exprs.size(); ++j) {
      named.emplace(arg_exprs[j].name(), &arg_exprs[j].expr());
    }

    for (size_t j = i; j < param_qts.size(); ++j) {
      std::string_view name            = param_qts[j].name;
      auto const *constant_typed_value = constant_arguments.at_or_null(name);
      auto iter                        = named.find(name);
      auto const *expr = iter == named.end() ? nullptr : iter->second;
      ast::Expression const *default_value = nullptr;

      if (not expr) {
        ASSERT(callee_fn.is_reg() == false);
        ASSERT(callee_fn.value().kind() == ir::Fn::Kind::Native);
        default_value = ASSERT_NOT_NULL(
            callee_fn.value().native()->params()[j].value.get()->init_val());
      }

      prepared_arguments.push_back(PrepareArgument(
          compiler, constant_typed_value ? **constant_typed_value : ir::Value(),
          expr ? expr : default_value, param_qts[i].value));
    }
  }

  auto out_params = SetReturns(Tag{}, c.builder(), overload_type, to);
  compiler.builder().Call(callee_fn, overload_type,
                          std::move(prepared_arguments), out_params);
  int i = -1;
  for (type::Type t : overload_type->output()) {
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
ir::Value EmitBuiltinCall(Compiler &c, ast::BuiltinFn const *callee,
                          absl::Span<ast::Call::Argument const> args) {
  switch (callee->value().which()) {
    case ir::BuiltinFn::Which::Slice: {
      type::Slice const *slice_type =
          type::Slc(c.context()
                        .qual_types(&args[0].expr())[0]
                        .type()
                        .as<type::BufferPointer>()
                        .pointee());
      auto slice = c.builder().TmpAlloca(slice_type);

      // TODO: These have the wrong types, or at least these types are not the
      // types of the values held, but that's what's expected by EmitMoveAssign.
      type::Typed<ir::RegOr<ir::Addr>> data(
          c.current_block()->Append(type::SliceDataInstruction{
              .slice  = slice,
              .result = c.builder().CurrentGroup()->Reserve(),
          }),
          type::BufPtr(slice_type->data_type()));
      type::Typed<ir::RegOr<ir::Addr>> length(
          c.current_block()->Append(type::SliceLengthInstruction{
              .slice  = slice,
              .result = c.builder().CurrentGroup()->Reserve(),
          }),
          type::U64);

      c.EmitMoveAssign(
          data, type::Typed<ir::Value>(c.EmitValue(&args[0].expr()),
                                       type::BufPtr(slice_type->data_type())));
      c.EmitMoveAssign(length, type::Typed<ir::Value>(
                                   c.EmitValue(&args[1].expr()), type::U64));
      return ir::Value(slice);
    } break;
    case ir::BuiltinFn::Which::Foreign: {
      auto name_buffer =
          c.EvaluateToBufferOrDiagnose(type::Typed<ast::Expression const *>(
              &args[0].expr(), type::Slc(type::Char)));
      if (name_buffer.empty()) { return ir::Value(); }

      auto maybe_foreign_type =
          c.EvaluateOrDiagnoseAs<type::Type>(&args[1].expr());
      if (not maybe_foreign_type) { return ir::Value(); }
      auto slice = name_buffer.get<ir::Slice>(0);

      std::string name(ir::ReadOnlyData.lock()->raw(slice.data().rodata()),
                       slice.length());
      return ir::Value(c.current_block()->Append(ir::LoadSymbolInstruction{
          .name   = std::move(name),
          .type   = *maybe_foreign_type,
          .result = c.builder().CurrentGroup()->Reserve()}));
    } break;

    case ir::BuiltinFn::Which::Opaque:
      return ir::Value(c.current_block()->Append(type::OpaqueTypeInstruction{
          .mod    = &c.context().module(),
          .result = c.builder().CurrentGroup()->Reserve()}));

    case ir::BuiltinFn::Which::Bytes: {
      auto const &fn_type = *ir::Fn(ir::BuiltinFn::Bytes()).type();
      ir::OutParams outs  = c.builder().OutParams(fn_type.output());
      ir::Reg reg         = outs[0];
      c.builder().Call(
          ir::Fn{ir::BuiltinFn::Bytes()}, &fn_type,
          {ir::Value(
              c.EmitValue(&args[0].expr()).get<ir::RegOr<type::Type>>())},
          std::move(outs));

      return ir::Value(reg);
    } break;

    case ir::BuiltinFn::Which::Alignment: {
      auto const &fn_type = *ir::Fn(ir::BuiltinFn::Alignment()).type();
      ir::OutParams outs  = c.builder().OutParams(fn_type.output());
      ir::Reg reg         = outs[0];
      c.builder().Call(
          ir::Fn{ir::BuiltinFn::Alignment()}, &fn_type,
          {ir::Value(
              c.EmitValue(&args[0].expr()).get<ir::RegOr<type::Type>>())},
          std::move(outs));

      return ir::Value(reg);
    } break;

    case ir::BuiltinFn::Which::DebugIr:
      c.builder().DebugIr();
      return ir::Value();

    case ir::BuiltinFn::Which::Callable: {
      std::vector<ir::RegOr<type::Type>> pos;
      absl::flat_hash_map<std::string, ir::RegOr<type::Type>> named;
      for (auto const &arg : args) {
        if (arg.named()) {
          named.emplace(arg.name(),
                        c.EmitValue(&arg.expr()).get<ir::RegOr<type::Type>>());
        } else {
          pos.push_back(c.EmitValue(&arg.expr()).get<ir::RegOr<type::Type>>());
        }
      }
      return ir::Value(c.current_block()->Append(interface::CallableInstruction{
          .positional = std::move(pos),
          .named      = std::move(named),
          .result     = c.builder().CurrentGroup()->Reserve(),
      }));
    }
    case ir::BuiltinFn::Which::Abort:
      c.current_block()->Append(ir::AbortInstruction{});
      return ir::Value();
  }
  UNREACHABLE();
}

}  // namespace

ir::Value Compiler::EmitValue(ast::Call const *node) {
  if (auto *b = node->callee()->if_as<ast::BuiltinFn>()) {
    return EmitBuiltinCall(*this, b, node->arguments());
  }

  auto qts = context().qual_types(node);
  if (not qts.empty() and qts[0].type() == type::Interface) {
    return ir::Value();
  }

  // Constant arguments need to be computed entirely before being used to
  // instantiate a generic function.
  core::Arguments<type::Typed<ir::Value>> constant_arguments =
      EmitConstantArguments(*this, node->arguments());

  // TODO: Support mixed overloads
  if (auto const *gs_type = context()
                                .qual_types(node->callee())[0]
                                .type()
                                .if_as<type::GenericStruct>()) {
    return ir::Value(
        type::Type(gs_type->Instantiate(constant_arguments).second));
  }

  auto const &os = context().ViableOverloads(node->callee());
  ASSERT(os.members().size() == 1u);  // TODO: Support dynamic dispatch.
  // TODO: It'd be nice to not stack-allocate register-sized values.
  std::vector<type::Typed<ir::RegOr<ir::Addr>>> outs;
  outs.reserve(qts.size());
  for (type::QualType const &qt : qts) {
    outs.emplace_back(builder().TmpAlloca(qt.type()), qt.type());
  }
  EmitCall(MoveInitTag{}, *this, os.members().front(), constant_arguments,
           node->arguments(), outs);
  if (qts.size() == 1) {
    return ir::Value(builder().PtrFix(outs[0]->reg(), qts[0].type()));
  } else {
    return ir::Value();
  }
}

void Compiler::EmitMoveInit(
    ast::Call const *node,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  if (auto *b = node->callee()->if_as<ast::BuiltinFn>()) {
    auto result = EmitBuiltinCall(*this, b, node->arguments());
    if (result.empty()) return;
    EmitCopyAssign(to[0], type::Typed<ir::Value>(
                              result, context().qual_types(node)[0].type()));
    return;
  }

  // Constant arguments need to be computed entirely before being used to
  // instantiate a generic function.
  core::Arguments<type::Typed<ir::Value>> constant_arguments =
      EmitConstantArguments(*this, node->arguments());

  // TODO: Support mixed overloads
  if (auto const *gs_type = context()
                                .qual_types(node->callee())[0]
                                .type()
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
           node->arguments(), to);
}

void Compiler::EmitCopyInit(
    ast::Call const *node,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  if (auto *b = node->callee()->if_as<ast::BuiltinFn>()) {
    auto result = EmitBuiltinCall(*this, b, node->arguments());
    if (result.empty()) return;
    EmitCopyAssign(to[0], type::Typed<ir::Value>(
                              result, context().qual_types(node)[0].type()));
    return;
  }

  // Constant arguments need to be computed entirely before being used to
  // instantiate a generic function.
  core::Arguments<type::Typed<ir::Value>> constant_arguments =
      EmitConstantArguments(*this, node->arguments());

  // TODO: Support mixed overloads
  if (auto const *gs_type = context()
                                .qual_types(node->callee())[0]
                                .type()
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
                  constant_arguments, node->arguments(), to);
}

void Compiler::EmitMoveAssign(
    ast::Call const *node,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  if (auto *b = node->callee()->if_as<ast::BuiltinFn>()) {
    auto result = EmitBuiltinCall(*this, b, node->arguments());
    if (result.empty()) return;
    EmitMoveAssign(to[0], type::Typed<ir::Value>(
                              result, context().qual_types(node)[0].type()));
    return;
  }

  // Constant arguments need to be computed entirely before being used to
  // instantiate a generic function.
  core::Arguments<type::Typed<ir::Value>> constant_arguments =
      EmitConstantArguments(*this, node->arguments());

  // TODO: Support mixed overloads
  if (auto const *gs_type = context()
                                .qual_types(node->callee())[0]
                                .type()
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
                  node->arguments(), to);
}

void Compiler::EmitCopyAssign(
    ast::Call const *node,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  if (auto *b = node->callee()->if_as<ast::BuiltinFn>()) {
    auto result = EmitBuiltinCall(*this, b, node->arguments());
    if (result.empty()) return;
    EmitCopyAssign(to[0], type::Typed<ir::Value>(
                              result, context().qual_types(node)[0].type()));
    return;
  }

  // Constant arguments need to be computed entirely before being used to
  // instantiate a generic function.
  core::Arguments<type::Typed<ir::Value>> constant_arguments =
      EmitConstantArguments(*this, node->arguments());
  // TODO: Support mixed overloads
  if (auto const *gs_type = context()
                                .qual_types(node->callee())[0]
                                .type()
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
                  node->arguments(), to);
}

}  // namespace compiler

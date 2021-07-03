#include "ast/ast.h"
#include "base/meta.h"
#include "compiler/compiler.h"
#include "compiler/emit/common.h"
#include "compiler/resources.h"
#include "ir/instruction/instructions.h"
#include "type/instantiated_generic_struct.h"
#include "type/interface/ir.h"

namespace compiler {
namespace {

// TODO: Checking if an AST node is a builtin is problematic because something
// as simple as
// ```
// f ::= bytes
// f(int)
// ```
// breaks.
//
void EmitBuiltinCall(Compiler &c, ast::BuiltinFn const *callee,
                     absl::Span<ast::Call::Argument const> args,
                     base::untyped_buffer &out) {
  switch (callee->value().which()) {
    case ir::BuiltinFn::Which::ReserveMemory: {
      out.append(ir::RegOr<ir::addr_t>(
          c.builder().CurrentGroup()->Alloca(core::TypeContour(
              core::Bytes(*c.EvaluateOrDiagnoseAs<uint64_t>(&args[0].expr())),
              core::Alignment(
                  *c.EvaluateOrDiagnoseAs<uint64_t>(&args[1].expr()))))));
      return;
    } break;
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
      type::Typed<ir::RegOr<ir::addr_t>> data(
          c.current_block()->Append(type::SliceDataInstruction{
              .slice  = slice,
              .result = c.builder().CurrentGroup()->Reserve(),
          }),
          type::BufPtr(slice_type->data_type()));
      type::Typed<ir::RegOr<ir::addr_t>> length(
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
      out.append(ir::RegOr<ir::addr_t>(slice));
      return;
    } break;
    case ir::BuiltinFn::Which::Foreign: {
      auto name_buffer =
          c.EvaluateToBufferOrDiagnose(type::Typed<ast::Expression const *>(
              &args[0].expr(), type::Slc(type::Char)));
      if (auto *diagnostics =
              std::get_if<std::vector<diagnostic::ConsumedMessage>>(
                  &name_buffer)) {
        for (auto &d : *diagnostics) { c.diag().Consume(std::move(d)); }
        return;
      }

      auto maybe_foreign_type =
          c.EvaluateOrDiagnoseAs<type::Type>(&args[1].expr());
      if (not maybe_foreign_type) { return; }
      auto slice =
          std::get<base::untyped_buffer>(name_buffer).get<ir::Slice>(0);

      std::string name(slice);
      auto result = c.current_block()->Append(ir::LoadSymbolInstruction{
          .name   = std::move(name),
          .type   = *maybe_foreign_type,
          .result = c.builder().CurrentGroup()->Reserve()});
      if (maybe_foreign_type->is<type::Pointer>()) {
        out.append(ir::RegOr<ir::addr_t>(result));
      } else if (maybe_foreign_type->is<type::Function>()) {
        out.append(ir::RegOr<ir::Fn>(result));
      } else {
        UNREACHABLE();
      }
      return;
    } break;

    case ir::BuiltinFn::Which::Opaque:
      out.append(ir::RegOr<type::Type>(
          c.current_block()->Append(type::OpaqueTypeInstruction{
              .mod    = &c.context().module(),
              .result = c.builder().CurrentGroup()->Reserve()})));
      return;

    case ir::BuiltinFn::Which::Bytes: {
      auto const &fn_type = *ir::Fn(ir::BuiltinFn::Bytes()).type();
      ir::OutParams outs  = c.builder().OutParams(fn_type.output());
      ir::Reg reg         = outs[0];
      c.builder().Call(
          ir::Fn{ir::BuiltinFn::Bytes()}, &fn_type,
          {ir::Value(
              c.EmitValue(&args[0].expr()).get<ir::RegOr<type::Type>>())},
          std::move(outs));
      // TODO: Return an integer
      out.append(ir::RegOr<uint64_t>(reg));
      return;
    } break;

    case ir::BuiltinFn::Which::Alignment: {
      // TODO: Return an integer
      auto const &fn_type = *ir::Fn(ir::BuiltinFn::Alignment()).type();
      ir::OutParams outs  = c.builder().OutParams(fn_type.output());
      ir::Reg reg         = outs[0];
      c.builder().Call(
          ir::Fn{ir::BuiltinFn::Alignment()}, &fn_type,
          {ir::Value(
              c.EmitValue(&args[0].expr()).get<ir::RegOr<type::Type>>())},
          std::move(outs));
      out.append(ir::RegOr<uint64_t>(reg));
      return;
    } break;

    case ir::BuiltinFn::Which::DebugIr: c.builder().DebugIr(); return;

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
      out.append(ir::RegOr<interface::Interface>(
          c.current_block()->Append(interface::CallableInstruction{
              .positional = std::move(pos),
              .named      = std::move(named),
              .result     = c.builder().CurrentGroup()->Reserve(),
          })));
      return;
    }
    case ir::BuiltinFn::Which::Abort:
      c.current_block()->Append(ir::AbortInstruction{});
      return;
  }
  UNREACHABLE();
}

}  // namespace

void Compiler::EmitToBuffer(ast::Call const *node, base::untyped_buffer &out) {
  if (auto *b = node->callee()->if_as<ast::BuiltinFn>()) {
    EmitBuiltinCall(*this, b, node->arguments(), out);
    return;
  }

  auto qts = context().qual_types(node);
  if (not qts.empty() and qts[0].type() == type::Interface) { return; }

  // Constant arguments need to be computed entirely before being used to
  // instantiate a generic function.
  core::Arguments<type::Typed<ir::Value>> constant_arguments =
      EmitConstantArguments(*this, node->arguments());

  // TODO: Support mixed overloads
  if (auto const *gs_type = context()
                                .qual_types(node->callee())[0]
                                .type()
                                .if_as<type::GenericStruct>()) {
    out.append(ir::RegOr<type::Type>(
        type::Type(gs_type->Instantiate(constant_arguments).second)));
    return;
  }

  auto const &os = context().ViableOverloads(node->callee());
  ASSERT(os.members().size() == 1u);  // TODO: Support dynamic dispatch.
  // TODO: It'd be nice to not stack-allocate register-sized values.
  std::vector<type::Typed<ir::RegOr<ir::addr_t>>> outs;
  outs.reserve(qts.size());
  for (type::QualType const &qt : qts) {
    outs.emplace_back(builder().TmpAlloca(qt.type()), qt.type());
  }
  EmitCall(*this, os.members().front(), constant_arguments, node->arguments(),
           outs);
  if (qts.size() == 1) {
    FromValue(ir::Value(builder().PtrFix(outs[0]->reg(), qts[0].type())),
              qts[0].type(), out);
  }
}

void Compiler::EmitMoveInit(
    ast::Call const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  if (auto *b = node->callee()->if_as<ast::BuiltinFn>()) {
    base::untyped_buffer out;
    EmitBuiltinCall(*this, b, node->arguments(), out);
    if (out.empty()) { return; }
    ir::Value result = ToValue(out, context().qual_types(node)[0].type());
    EmitMoveAssign(to[0], type::Typed<ir::Value>(
                              result, context().qual_types(node)[0].type()));
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
  EmitCall(*this, os.members().front(), constant_arguments, node->arguments(),
           to);
}

void Compiler::EmitCopyInit(
    ast::Call const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  if (auto *b = node->callee()->if_as<ast::BuiltinFn>()) {
    base::untyped_buffer out;
    EmitBuiltinCall(*this, b, node->arguments(), out);
    if (out.empty()) { return; }
    ir::Value result = ToValue(out, context().qual_types(node)[0].type());
    EmitCopyAssign(to[0], type::Typed<ir::Value>(
                              result, context().qual_types(node)[0].type()));
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
  return EmitCall(*this, os.members().front(), constant_arguments,
                  node->arguments(), to);
}

void Compiler::EmitMoveAssign(
    ast::Call const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  if (auto *b = node->callee()->if_as<ast::BuiltinFn>()) {
    base::untyped_buffer out;
    EmitBuiltinCall(*this, b, node->arguments(), out);
    if (out.empty()) { return; }
    ir::Value result = ToValue(out, context().qual_types(node)[0].type());
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

  return EmitCall(*this, os.members().front(), constant_arguments,
                  node->arguments(), to);
}

void Compiler::EmitCopyAssign(
    ast::Call const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  if (auto *b = node->callee()->if_as<ast::BuiltinFn>()) {
    base::untyped_buffer out;
    EmitBuiltinCall(*this, b, node->arguments(), out);
    if (out.empty()) { return; }
    ir::Value result = ToValue(out, context().qual_types(node)[0].type());
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

  return EmitCall(*this, os.members().front(), constant_arguments,
                  node->arguments(), to);
}

bool Compiler::PatternMatch(
    ast::Call const *node, PatternMatchingContext &pmc,
    absl::flat_hash_map<ast::Declaration::Id const *, ir::Value> &bindings) {
  // TODO: Only supporting parameterized structs right now.
  if (pmc.type != type::Type_) { return false; }

  auto const *i =
      pmc.value.get<type::Type>(0).if_as<type::InstantiatedGenericStruct>();

  if (not i) { return false; }

  // TODO: Named arguments as well.
  size_t index = 0;
  for (auto const &a : i->arguments().pos()) {
    // TODO: Check that the type is what we expect.
    base::untyped_buffer buffer;
    buffer.append(a->get<type::Type>());

    // TODO: Support non-type parameters.
    EnqueuePatternMatch(&node->arguments()[index].expr(),
                        {.type = type::Type_, .value = std::move(buffer)});
    ++i;
  }

  return true;
}

}  // namespace compiler

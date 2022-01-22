#include "ast/ast.h"
#include "base/meta.h"
#include "compiler/compiler.h"
#include "compiler/emit/common.h"
#include "compiler/emit/compiler_common.h"
#include "compiler/emit/copy_move_assignment.h"
#include "compiler/emit/initialize.h"
#include "compiler/instantiate.h"
#include "compiler/module.h"
#include "compiler/resources.h"
#include "ir/instruction/instructions.h"
#include "type/generic.h"
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
                     ir::PartialResultBuffer &out) {
  switch (callee->value().which()) {
    case ir::BuiltinFn::Which::ReserveMemory: {
      out.append(c.current().subroutine->Alloca(core::TypeContour(
          core::Bytes(*c.EvaluateOrDiagnoseAs<uint64_t>(&args[0].expr())),
          core::Alignment(
              *c.EvaluateOrDiagnoseAs<uint64_t>(&args[1].expr())))));
      return;
    } break;
    case ir::BuiltinFn::Which::Slice: {
      type::Slice const *slice_type =
          type::Slc(c.context()
                        .qual_types(&args[0].expr())[0]
                        .type()
                        .as<type::BufferPointer>()
                        .pointee());
      auto slice = c.state().TmpAlloca(slice_type);

      // TODO: These have the wrong types, or at least these types are not the
      // types of the values held, but that's what's expected by EmitMoveAssign.
      type::Typed<ir::RegOr<ir::addr_t>> data(
          c.current_block()->Append(type::SliceDataInstruction{
              .slice  = slice,
              .result = c.current().subroutine->Reserve(),
          }),
          type::BufPtr(slice_type->data_type()));
      type::Typed<ir::RegOr<ir::addr_t>> length(
          c.current_block()->Append(type::SliceLengthInstruction{
              .slice  = slice,
              .result = c.current().subroutine->Reserve(),
          }),
          type::Slice::LengthType());

      ir::PartialResultBuffer buffer;
      c.EmitToBuffer(&args[0].expr(), buffer);
      MoveAssignmentEmitter emitter(c);
      emitter(data,
              type::Typed(buffer[0],
                          type::Type(type::BufPtr(slice_type->data_type()))));
      buffer.clear();
      c.EmitToBuffer(&args[1].expr(), buffer);
      emitter(length, type::Typed(buffer[0], type::Slice::LengthType()));
      out.append(slice);
      return;
    } break;
    case ir::BuiltinFn::Which::HasBlock: {
      ir::ScopeContext sc =
          *c.EvaluateOrDiagnoseAs<ir::ScopeContext>(&args[0].expr());
      auto name_buffer =
          *c.EvaluateToBufferOrDiagnose(type::Typed<ast::Expression const *>(
              &args[1].expr(), type::Slc(type::Char)));
      out.append(sc.find(name_buffer[0].get<ir::Slice>()) !=
                 ir::Block::Invalid());
      return;
    } break;
    case ir::BuiltinFn::Which::Foreign: {
      // `EvaluateOrDiagnoseAs` cannot yet support slices because we it
      // internally converts compile-time types to a type::Type and it doesn't
      // know which instance of type::Slice it should use.
      auto name_buffer =
          c.EvaluateToBufferOrDiagnose(type::Typed<ast::Expression const *>(
              &args[0].expr(), type::Slc(type::Char)));
      if (not name_buffer) { return; }

      auto maybe_foreign_type =
          c.EvaluateOrDiagnoseAs<type::Type>(&args[1].expr());
      if (not maybe_foreign_type) { return; }
      auto slice = name_buffer->get<ir::Slice>(0);

      std::string name(slice);
      auto result = c.current_block()->Append(
          ir::LoadSymbolInstruction{.name   = std::move(name),
                                    .type   = *maybe_foreign_type,
                                    .result = c.current().subroutine->Reserve()});
      if (maybe_foreign_type->is<type::Pointer>() or
          maybe_foreign_type->is<type::Function>()) {
        out.append(result);
      } else {
        UNREACHABLE();
      }
      return;
    } break;

    case ir::BuiltinFn::Which::Opaque:
      out.append(c.current_block()->Append(
          type::OpaqueTypeInstruction{.mod    = c.resources().module,
                                      .result = c.current().subroutine->Reserve()}));
      return;

    case ir::BuiltinFn::Which::Bytes: {
      auto const &fn_type = *ir::Fn(ir::BuiltinFn::Bytes()).type();
      ir::OutParams outs  = c.OutParams(fn_type.return_types());
      ir::Reg reg         = outs[0];
      ir::PartialResultBuffer buffer;
      c.EmitToBuffer(&args[0].expr(), buffer);
      c.current_block()->Append(
          ir::CallInstruction(&fn_type, ir::Fn{ir::BuiltinFn::Bytes()},
                              std::move(buffer), std::move(outs)));

      // TODO: Return an integer
      out.append(reg);
      return;
    } break;

    case ir::BuiltinFn::Which::Alignment: {
      // TODO: Return an integer
      auto const &fn_type = *ir::Fn(ir::BuiltinFn::Alignment()).type();
      ir::OutParams outs  = c.OutParams(fn_type.return_types());
      ir::Reg reg         = outs[0];
      ir::PartialResultBuffer buffer;
      c.EmitToBuffer(&args[0].expr(), buffer);
      c.current_block()->Append(
          ir::CallInstruction(&fn_type, ir::Fn{ir::BuiltinFn::Alignment()},
                              std::move(buffer), std::move(outs)));
      out.append(reg);
      return;
    } break;

    case ir::BuiltinFn::Which::DebugIr:
      c.current_block()->Append(
          ir::DebugIrInstruction{.fn = c.current().subroutine});
      return;

    case ir::BuiltinFn::Which::Abort:
      c.current_block()->Append(ir::AbortInstruction{});
      return;
    case ir::BuiltinFn::Which::CompilationError: UNREACHABLE();
  }
  UNREACHABLE();
}

}  // namespace

void Compiler::EmitToBuffer(ast::Call const *node,
                            ir::PartialResultBuffer &out) {
  if (auto *b = node->callee()->if_as<ast::BuiltinFn>()) {
    EmitBuiltinCall(*this, b, node->arguments(), out);
    return;
  }

  auto qts = context().qual_types(node);

  // Constant arguments need to be computed entirely before being used to
  // instantiate a generic function.
  ir::CompleteResultBuffer buffer;
  auto constant_arguments =
      EmitConstantArguments(*this, node->arguments(), buffer);

  // TODO: Support mixed overloads
  if (auto const *gs_type = context()
                                .qual_types(node->callee())[0]
                                .type()
                                .if_as<type::Generic<type::Struct>>()) {
    out.append(
        type::Type(gs_type->Instantiate(work_resources(), constant_arguments)));
    return;
  }

  // TODO: It'd be nice to not stack-allocate register-sized values.
  std::vector<type::Typed<ir::RegOr<ir::addr_t>>> outs;
  outs.reserve(qts.size());
  for (type::QualType const &qt : qts) {
    outs.emplace_back(state().TmpAlloca(qt.type()), qt.type());
  }

  EmitCall(*this, context().CallMetadata(node).resolved(), constant_arguments,
           node->arguments(), outs);
  // TODO: Why is this conditional on the size of qts?
  if (qts.size() == 1) {
    out.append(PtrFix(current(), outs[0]->reg(), qts[0].type()));
  }
}

void Compiler::EmitMoveInit(
    ast::Call const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  if (auto *b = node->callee()->if_as<ast::BuiltinFn>()) {
    ir::PartialResultBuffer out;
    EmitBuiltinCall(*this, b, node->arguments(), out);
    if (out.empty()) { return; }
    MoveAssignmentEmitter emitter(*this);
    emitter(to[0], type::Typed(out[0], context().qual_types(node)[0].type()));
    return;
  }

  // Constant arguments need to be computed entirely before being used to
  // instantiate a generic function.
  ir::CompleteResultBuffer buffer;
  auto constant_arguments =
      EmitConstantArguments(*this, node->arguments(), buffer);
  // TODO: Support mixed overloads
  if (auto const *gs_type = context()
                                .qual_types(node->callee())[0]
                                .type()
                                .if_as<type::Generic<type::Struct>>()) {
    ir::RegOr<type::Type> t(
        type::Type(gs_type->Instantiate(work_resources(), constant_arguments)));
    ir::PartialResultBuffer t_buf;
    t_buf.append(t);

    CopyAssignmentEmitter emitter(*this);
    emitter(to[0], type::Typed(t_buf[0], type::Type_));
    return;
  }

  EmitCall(*this, context().CallMetadata(node).resolved(), constant_arguments,
           node->arguments(), to);
}

void Compiler::EmitCopyInit(
    ast::Call const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  if (auto *b = node->callee()->if_as<ast::BuiltinFn>()) {
    ir::PartialResultBuffer out;
    EmitBuiltinCall(*this, b, node->arguments(), out);
    if (out.empty()) { return; }
    CopyInitializationEmitter emitter(*this);
    emitter(to[0], out);
    return;
  }

  // Constant arguments need to be computed entirely before being used to
  // instantiate a generic function.
  ir::CompleteResultBuffer buffer;
  auto constant_arguments =
      EmitConstantArguments(*this, node->arguments(), buffer);

  // TODO: Support mixed overloads
  if (auto const *gs_type = context()
                                .qual_types(node->callee())[0]
                                .type()
                                .if_as<type::Generic<type::Struct>>()) {
    ir::RegOr<type::Type> t(
        type::Type(gs_type->Instantiate(work_resources(), constant_arguments)));
    ir::PartialResultBuffer t_buf;
    t_buf.append(t);
    CopyAssignmentEmitter emitter(*this);
    emitter(to[0], type::Typed(t_buf[0], type::Type_));
    return;
  }

  EmitCall(*this, context().CallMetadata(node).resolved(), constant_arguments,
           node->arguments(), to);
}

void Compiler::EmitMoveAssign(
    ast::Call const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  if (auto *b = node->callee()->if_as<ast::BuiltinFn>()) {
    ir::PartialResultBuffer out;
    EmitBuiltinCall(*this, b, node->arguments(), out);
    if (out.empty()) { return; }
    MoveAssignmentEmitter emitter(*this);
    emitter(to[0], type::Typed(out[0], context().qual_types(node)[0].type()));
    return;
  }

  // Constant arguments need to be computed entirely before being used to
  // instantiate a generic function.
  ir::CompleteResultBuffer buffer;
  auto constant_arguments =
      EmitConstantArguments(*this, node->arguments(), buffer);

  // TODO: Support mixed overloads
  if (auto const *gs_type = context()
                                .qual_types(node->callee())[0]
                                .type()
                                .if_as<type::Generic<type::Struct>>()) {
    ir::RegOr<type::Type> t(
        type::Type(gs_type->Instantiate(work_resources(), constant_arguments)));
    ir::PartialResultBuffer t_buf;
    t_buf.append(t);
    MoveAssignmentEmitter emitter(*this);
    emitter(to[0], type::Typed(t_buf[0], type::Type_));
  }

  EmitCall(*this, context().CallMetadata(node).resolved(), constant_arguments,
           node->arguments(), to);
}

void Compiler::EmitCopyAssign(
    ast::Call const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  if (auto *b = node->callee()->if_as<ast::BuiltinFn>()) {
    ir::PartialResultBuffer out;
    EmitBuiltinCall(*this, b, node->arguments(), out);
    if (out.empty()) { return; }
    CopyAssignmentEmitter emitter(*this);
    emitter(to[0], type::Typed(out[0], context().qual_types(node)[0].type()));
    return;
  }

  // Constant arguments need to be computed entirely before being used to
  // instantiate a generic function.
  ir::CompleteResultBuffer buffer;
  auto constant_arguments =
      EmitConstantArguments(*this, node->arguments(), buffer);

  // TODO: Support mixed overloads
  if (auto const *gs_type = context()
                                .qual_types(node->callee())[0]
                                .type()
                                .if_as<type::Generic<type::Struct>>()) {
    ir::RegOr<type::Type> t(
        type::Type(gs_type->Instantiate(work_resources(), constant_arguments)));
    ir::PartialResultBuffer t_buf;
    t_buf.append(t);
    CopyAssignmentEmitter emitter(*this);
    emitter(to[0], type::Typed(t_buf[0], type::Type_));
  }

  EmitCall(*this, context().CallMetadata(node).resolved(), constant_arguments,
           node->arguments(), to);
}

bool Compiler::PatternMatch(
    ast::Call const *node, PatternMatchingContext &pmc,
    absl::flat_hash_map<ast::Declaration::Id const *, ir::CompleteResultBuffer>
        &bindings) {
  // TODO: Only supporting parameterized structs right now.
  if (pmc.type != type::Type_) { return false; }

  auto const *i = pmc.value.get<type::Type>(0)
                      .if_as<type::InstantiatedGeneric<type::Struct>>();

  if (not i) { return false; }

  // TODO: Named arguments as well.
  size_t index = 0;
  for (auto const &a : i->arguments().pos()) {
    // TODO: Check that the type is what we expect.
    ir::CompleteResultBuffer buffer;
    buffer.append(a->get<type::Type>(0));

    // TODO: Support non-type parameters.
    state().EnqueuePatternMatch(
        &node->arguments()[index].expr(),
        {.type = type::Type_, .value = std::move(buffer)});
    ++i;
  }

  return true;
}

}  // namespace compiler

#include "ast/ast.h"
#include "base/meta.h"
#include "compiler/builtin_module.h"
#include "compiler/compiler.h"
#include "compiler/emit/common.h"
#include "compiler/emit/compiler_common.h"
#include "compiler/emit/copy_move_assignment.h"
#include "compiler/emit/initialize.h"
#include "compiler/instantiate.h"
#include "compiler/interface_instructions.h"
#include "compiler/module.h"
#include "compiler/resources.h"
#include "ir/instruction/instructions.h"
#include "type/legacy_generic.h"

namespace compiler {
namespace {

// TODO: Replace with `builtin` module.
bool EmitBuiltinCall(Compiler &c, std::string_view f,
                     absl::Span<ast::Call::Argument const> args,
                     ir::PartialResultBuffer &out) {
  if (f == "slice") {
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
    return true;
  }

  if (f == "foreign") {
    // `EvaluateOrDiagnoseAs` cannot yet support slices because we it
    // internally converts compile-time types to a type::Type and it doesn't
    // know which instance of type::Slice it should use.
    auto name_buffer =
        c.EvaluateToBufferOrDiagnose(type::Typed<ast::Expression const *>(
            &args[0].expr(), type::Slc(type::Char)));
    if (not name_buffer) { return true; }

    auto maybe_foreign_type =
        c.EvaluateOrDiagnoseAs<type::Type>(&args[1].expr());
    if (not maybe_foreign_type) { return true; }
    auto slice = name_buffer->get<ir::Slice>(0);

    std::string name(slice);
    if (maybe_foreign_type->is<type::Pointer>()) {
      auto result = c.current_block()->Append(ir::LoadDataSymbolInstruction{
          .name   = std::move(name),
          .result = c.current().subroutine->Reserve()});
      out.append(result);
    } else if (auto const *f = maybe_foreign_type->if_as<type::Function>()) {
      out.append(c.shared_context().ForeignFunction(std::move(name), f));
    } else {
      UNREACHABLE();
    }
    return true;
  }

  if (f == "callable") {
    core::Arguments<ir::RegOr<ir::Interface>> callable_interface_arguments;
    ir::PartialResultBuffer buffer;
    for (auto const &arg : args) {
      buffer.clear();
      type::QualType qt = c.context().qual_types(&arg.expr())[0];
      c.EmitToBuffer(&arg.expr(), buffer);
      ApplyImplicitCasts(c, qt.type(),
                         type::QualType::NonConstant(type::Interface), buffer);
      if (arg.named()) {
        callable_interface_arguments.named_emplace(
            arg.name(), buffer[0].get<ir::Interface>());
      } else {
        callable_interface_arguments.pos_emplace(
            buffer[0].get<ir::Interface>());
      }
    }

    out.append(c.current_block()->Append(CallableInterfaceInstruction{
        .manager   = c.current_block()->Append(LoadInterfaceManagerInstruction{
            .result = c.current().subroutine->Reserve(),
        }),
        .arguments = std::move(callable_interface_arguments),
        .result    = c.current().subroutine->Reserve(),
    }));
    return true;
  }

  if (f == "debug_ir") {
    c.current_block()->Append(
        ir::DebugIrInstruction{.fn = c.current().subroutine});
    return true;
  }

  if (f == "compilation_error") { UNREACHABLE(); }

  return false;
}

}  // namespace

void Compiler::EmitToBuffer(ast::Call const *node,
                            ir::PartialResultBuffer &out) {
  if (auto const *a = node->callee()->if_as<ast::Access>()) {
    if (context().qual_types(a->operand())[0] ==
        type::QualType::Constant(type::Module)) {
      if (*EvaluateOrDiagnoseAs<ir::ModuleId>(a->operand()) ==
          ir::ModuleId::Builtin()) {
        if (EmitBuiltinCall(*this, a->member_name(), node->arguments(), out)) {
          return;
        }
      }
    }
  }

  auto qts = context().qual_types(node);

  // Constant arguments need to be computed entirely before being used to
  // instantiate a Legacygeneric function.
  ir::CompleteResultBuffer buffer;
  auto constant_arguments =
      EmitConstantArguments(*this, node->arguments(), buffer);

  // TODO: Support mixed overloads
  if (auto const *gs_type = context()
                                .qual_types(node->callee())[0]
                                .type()
                                .if_as<type::LegacyGeneric<type::Struct>>()) {
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
  if (auto const *a = node->callee()->if_as<ast::Access>()) {
    if (context().qual_types(a->operand())[0] ==
        type::QualType::Constant(type::Module)) {
      if (*EvaluateOrDiagnoseAs<ir::ModuleId>(a->operand()) ==
          ir::ModuleId::Builtin()) {
        ir::PartialResultBuffer out;
        if (EmitBuiltinCall(*this, a->member_name(), node->arguments(), out)) {
          if (out.empty()) { return; }
          MoveInitializationEmitter emitter(*this);
          emitter(to[0], out);
          return;
        }
      }
    }
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
                                .if_as<type::LegacyGeneric<type::Struct>>()) {
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
  if (auto const *a = node->callee()->if_as<ast::Access>()) {
    if (context().qual_types(a->operand())[0] ==
        type::QualType::Constant(type::Module)) {
      if (*EvaluateOrDiagnoseAs<ir::ModuleId>(a->operand()) ==
          ir::ModuleId::Builtin()) {
        ir::PartialResultBuffer out;
        if (EmitBuiltinCall(*this, a->member_name(), node->arguments(), out)) {
          if (out.empty()) { return; }

          CopyInitializationEmitter emitter(*this);
          emitter(to[0], out);
          return;
        }
      }
    }
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
                                .if_as<type::LegacyGeneric<type::Struct>>()) {
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
  if (auto const *a = node->callee()->if_as<ast::Access>()) {
    if (context().qual_types(a->operand())[0] ==
        type::QualType::Constant(type::Module)) {
      if (*EvaluateOrDiagnoseAs<ir::ModuleId>(a->operand()) ==
          ir::ModuleId::Builtin()) {
        ir::PartialResultBuffer out;
        if (EmitBuiltinCall(*this, a->member_name(), node->arguments(), out)) {
          if (out.empty()) { return; }

          MoveAssignmentEmitter emitter(*this);
          emitter(to[0],
                  type::Typed(out[0], context().qual_types(node)[0].type()));
          return;
        }
      }
    }
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
                                .if_as<type::LegacyGeneric<type::Struct>>()) {
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
  if (auto const *a = node->callee()->if_as<ast::Access>()) {
    if (context().qual_types(a->operand())[0] ==
        type::QualType::Constant(type::Module)) {
      if (*EvaluateOrDiagnoseAs<ir::ModuleId>(a->operand()) ==
          ir::ModuleId::Builtin()) {
        ir::PartialResultBuffer out;
        if (EmitBuiltinCall(*this, a->member_name(), node->arguments(), out)) {
          if (out.empty()) { return; }

          CopyAssignmentEmitter emitter(*this);
          emitter(to[0],
                  type::Typed(out[0], context().qual_types(node)[0].type()));
          return;
        }
      }
    }
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
                                .if_as<type::LegacyGeneric<type::Struct>>()) {
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
  NOT_YET();
}

}  // namespace compiler

#include <utility>

#include "absl/types/span.h"
#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/emit/common.h"
#include "compiler/emit/copy_move_assignment.h"
#include "compiler/emit/destroy.h"
#include "compiler/emit/initialize.h"
#include "compiler/module.h"
#include "ir/interpreter/interpreter.h"
#include "ir/value/addr.h"
#include "ir/value/reg.h"
#include "ir/value/reg_or.h"
#include "type/type.h"
#include "type/typed_value.h"

namespace compiler {
// TODO: Remove forward declaration.
absl::Span<type::QualType const> VerifyType(CompilationDataReference data,
                                            ast::Node const *node);

namespace {

ir::Fn InsertGeneratedMoveInit(Compiler &c, type::Struct *s) {
  auto [fn, inserted] =
      c.context().ir().InsertMoveInit(s, s, [&](ir::Subroutine &subroutine) {
        c.push_current(&subroutine);
        absl::Cleanup cleanup = [&] { c.state().current.pop_back(); };
        c.current_block()     = subroutine.entry();

        auto from = ir::Reg::Parameter(0);
        auto to   = ir::Reg::Output(0);

        size_t i = 0;
        for (auto const &field : s->fields()) {
          auto to_ref = c.current_block()->Append(
              ir::StructIndexInstruction{.addr        = to,
                                         .index       = i,
                                         .struct_type = s,
                                         .result      = subroutine.Reserve()});
          auto from_val = c.current_block()->Append(
              ir::StructIndexInstruction{.addr        = from,
                                         .index       = i,
                                         .struct_type = s,
                                         .result      = subroutine.Reserve()});

          ir::RegOr<ir::addr_t> r(PtrFix(c.current(), from_val, field.type));
          ir::PartialResultBuffer buffer;
          buffer.append(r);
          MoveInitializationEmitter emitter(c);
          emitter(field.type, to_ref, buffer);
          ++i;
        }
        c.current_block()->set_jump(ir::JumpCmd::Return());
      });
  return fn;
}

ir::Fn InsertGeneratedCopyInit(Compiler &c, type::Struct *s) {
  return c.context()
      .ir()
      .InsertCopyInit(
          s, s,
          [&](ir::Subroutine &subroutine) {
            c.push_current(&subroutine);
            absl::Cleanup cleanup = [&] { c.state().current.pop_back(); };
            c.current_block()     = subroutine.entry();

            auto from = ir::Reg::Parameter(0);
            auto to   = ir::Reg::Output(0);

            size_t i = 0;
            for (auto const &field : s->fields()) {
              auto to_ref = c.current_block()->Append(
                  ir::StructIndexInstruction{.addr        = to,
                                             .index       = i,
                                             .struct_type = s,
                                             .result = subroutine.Reserve()});
              auto from_val = c.current_block()->Append(
                  ir::StructIndexInstruction{.addr        = from,
                                             .index       = i,
                                             .struct_type = s,
                                             .result = subroutine.Reserve()});

              ir::RegOr<ir::addr_t> r(
                  PtrFix(c.current(), from_val, field.type));
              ir::PartialResultBuffer buffer;
              buffer.append(r);
              CopyInitializationEmitter emitter(c);
              emitter(field.type, to_ref, buffer);
              ++i;
            }
            c.current_block()->set_jump(ir::JumpCmd::Return());
          })
      .first;
}

ir::Fn InsertGeneratedMoveAssign(Compiler &c, type::Struct *s) {
  return c.context()
      .ir()
      .InsertMoveAssign(
          s, s,
          [&](ir::Subroutine &subroutine) {
            c.push_current(&subroutine);
            absl::Cleanup cleanup = [&] { c.state().current.pop_back(); };
            c.current_block()     = subroutine.entry();
            auto var              = ir::Reg::Parameter(0);
            auto val              = ir::Reg::Parameter(1);

            for (size_t i = 0; i < s->fields().size(); ++i) {
              ir::Reg to_ref = c.current_block()->Append(
                  ir::StructIndexInstruction{.addr        = var,
                                             .index       = i,
                                             .struct_type = s,
                                             .result = subroutine.Reserve()});
              ir::Reg from_ref = c.current_block()->Append(
                  ir::StructIndexInstruction{.addr        = val,
                                             .index       = i,
                                             .struct_type = s,
                                             .result = subroutine.Reserve()});

              ir::PartialResultBuffer buffer;
              buffer.append(PtrFix(c.current(), from_ref, s->fields()[i].type));

              MoveAssignmentEmitter emitter(c);
              emitter(s->fields()[i].type, to_ref,
                      type::Typed(buffer[0], s->fields()[i].type));
            }

            c.current_block()->set_jump(ir::JumpCmd::Return());
          })
      .first;
}

ir::Fn InsertGeneratedCopyAssign(Compiler &c, type::Struct *s) {
  return c.context()
      .ir()
      .InsertCopyAssign(
          s, s,
          [&](ir::Subroutine &subroutine) {
            c.push_current(&subroutine);
            absl::Cleanup cleanup = [&] { c.state().current.pop_back(); };
            c.current_block()     = subroutine.entry();
            auto var              = ir::Reg::Parameter(0);
            auto val              = ir::Reg::Parameter(1);

            for (size_t i = 0; i < s->fields().size(); ++i) {
              ir::Reg to_ref = c.current_block()->Append(
                  ir::StructIndexInstruction{.addr        = var,
                                             .index       = i,
                                             .struct_type = s,
                                             .result = subroutine.Reserve()});
              ir::Reg from_ref = c.current_block()->Append(
                  ir::StructIndexInstruction{.addr        = val,
                                             .index       = i,
                                             .struct_type = s,
                                             .result = subroutine.Reserve()});

              ir::PartialResultBuffer buffer;
              buffer.append(PtrFix(c.current(), from_ref, s->fields()[i].type));

              CopyAssignmentEmitter emitter(c);
              emitter(s->fields()[i].type, to_ref,
                      type::Typed(buffer[0], s->fields()[i].type));
            }

            c.current_block()->set_jump(ir::JumpCmd::Return());
          })
      .first;
}

}  // namespace

void EmitStructCompletion(CompilationDataReference data, type::Struct *s,
                          absl::Span<ast::Declaration const> field_decls) {
  ASSERT(s->completeness() == type::Completeness::DataComplete);
  Compiler c(data);

  std::vector<type::StructInstruction::Field> constants;
  bool needs_dtor = false;
  for (auto const &field : s->fields()) {
    needs_dtor = needs_dtor or field.type.get()->HasDestructor();
  }

  std::optional<ir::Fn> user_dtor;
  std::vector<std::pair<ir::Fn, type::Type>> move_inits, copy_inits,
      move_assignments, copy_assignments;
  for (auto const &field_decl : field_decls) {
    if (not(field_decl.flags() & ast::Declaration::f_IsConst)) { continue; }
    VerifyType(data, &field_decl);

    // TODO: Access to init_val is not correct here because that may
    // initialize multiple values.
    for (auto const &id : field_decl.ids()) {
      // TODO: handle potential errors on each of these.
      if (id.name() == "destroy") {
        user_dtor = c.EmitAs<ir::Fn>(id.declaration().init_val()).value();
      } else if (id.name() == "move") {
        auto f    = c.EmitAs<ir::Fn>(id.declaration().init_val());
        auto info = data.shared_context().Function(f.value());
        switch (info.type->parameters().size()) {
          case 1: move_inits.emplace_back(f.value(), info.type); break;
          case 2: move_assignments.emplace_back(f.value(), info.type); break;
          default: UNREACHABLE();
        }
      } else if (id.name() == "copy") {
        auto f    = c.EmitAs<ir::Fn>(id.declaration().init_val());
        auto info = data.shared_context().Function(f.value());
        switch (info.type->parameters().size()) {
          case 1: copy_inits.emplace_back(f.value(), info.type); break;
          case 2: copy_assignments.emplace_back(f.value(), info.type); break;
          default: UNREACHABLE();
        }
      } else {
        if (auto const *init_val = id.declaration().init_val()) {
          // TODO init_val type may not be the same.
          type::Type field_type = data.context().qual_types(init_val)[0].type();

          ASSIGN_OR(NOT_YET(),  //
                    auto result,
                    data.EvaluateToBufferOrDiagnose(
                        type::Typed(init_val, field_type)));

          constants.emplace_back(id.name(), field_type, std::move(result))
              .set_export(
                  id.declaration().hashtags.contains(ir::Hashtag::Export));
        } else {
          // TODO: Failed evaluation
          // TODO: Type expression actually refers potentially to multiple
          // declaration ids.
          type::Type field_type = data.EvaluateOrDiagnoseAs<type::Type>(
                                          id.declaration().type_expr())
                                      .value();
          constants.emplace_back(id.name(), field_type)
              .set_export(
                  id.declaration().hashtags.contains(ir::Hashtag::Export));
        }
      }
    }
  }

  std::optional<ir::Fn> dtor;
  if (needs_dtor) {
    dtor =
        data.context()
            .ir()
            .InsertDestroy(
                s,
                [&](ir::Subroutine &subroutine) {
                  data.push_current(&subroutine);
                  absl::Cleanup cleanup = [&] { c.state().current.pop_back(); };
                  data.current_block()  = subroutine.entry();
                  auto var              = ir::Reg::Parameter(0);
                  if (user_dtor) {
                    // TODO: Should probably force-inline this.
                    ir::PartialResultBuffer args;
                    args.append(var);
                    // TODO: Constants
                    data.current_block()->Append(ir::CallInstruction(
                        &subroutine.type()->as<type::Function>(), *user_dtor,
                        std::move(args), ir::OutParams()));
                  }
                  for (int i = s->fields().size() - 1; i >= 0; --i) {
                    DestructionEmitter de(c);
                    de(s->fields()[i].type,
                       c.current_block()->Append(ir::StructIndexInstruction{
                           .addr        = var,
                           .index       = i,
                           .struct_type = s,
                           .result      = subroutine.Reserve()}));
                  }

                  data.current_block()->set_jump(ir::JumpCmd::Return());
                })
            .first;
  } else {
    if (user_dtor) { dtor = *user_dtor; }
  }

  if (move_inits.empty() and copy_inits.empty()) {
    ir::Fn fm = InsertGeneratedMoveInit(c, s);
    ir::Fn fc = InsertGeneratedCopyInit(c, s);
    move_inits.emplace_back(fm, c.shared_context().Function(fm).type);
    copy_inits.emplace_back(fc, c.shared_context().Function(fc).type);
  }

  if (move_assignments.empty() and copy_assignments.empty()) {
    ir::Fn fm = InsertGeneratedMoveAssign(c, s);
    ir::Fn fc = InsertGeneratedCopyAssign(c, s);
    move_assignments.emplace_back(fm, c.shared_context().Function(fm).type);
    copy_assignments.emplace_back(fc, c.shared_context().Function(fc).type);
  }

  data.current_block()->Append(
      type::StructInstruction{.struct_          = type::Type(s),
                              .constants        = std::move(constants),
                              .move_inits       = std::move(move_inits),
                              .copy_inits       = std::move(copy_inits),
                              .move_assignments = std::move(move_assignments),
                              .copy_assignments = std::move(copy_assignments),
                              .dtor             = dtor});
  data.current_block()->set_jump(ir::JumpCmd::Return());
}

std::optional<ir::Subroutine> StructCompletionFn(
    CompilationDataReference data, type::Struct *s,
    absl::Span<ast::Declaration const> field_decls) {
  ASSERT(s->completeness() == type::Completeness::DataComplete);

  ir::Subroutine fn(type::Func({}, {}));
  data.push_current(&fn);
  absl::Cleanup cleanup = [&] { data.state().current.pop_back(); };
  Compiler c(data);
  // TODO this is essentially a copy of the body of
  // FunctionLiteral::EmitToBuffer. Factor these out together.
  data.current_block() = fn.entry();
  EmitStructCompletion(data, s, field_decls);

  return fn;
}

void Compiler::EmitToBuffer(ast::StructLiteral const *node,
                            ir::PartialResultBuffer &out) {
  LOG("StructLiteral", "Starting struct-literal emission: %p", node);

  auto [t, inserted] = context().EmplaceType<type::Struct>(
      node, resources().module,
      type::Struct::Options{
          .is_copyable = not node->hashtags.contains(ir::Hashtag::Uncopyable),
          .is_movable  = not node->hashtags.contains(ir::Hashtag::Immovable),
      });

  if (inserted) {
    // Strictly speaking this conditional is not needed. Enqueuing the same work
    // item twice will be deduplicated.
    Enqueue({.kind    = WorkItem::Kind::CompleteStructData,
             .node    = node,
             .context = &context()},
            {WorkItem{.kind    = WorkItem::Kind::VerifyStructBody,
                      .node    = node,
                      .context = &context()}});
  }
  out.append(t);
}

bool Compiler::CompleteStruct(ast::StructLiteral const *node) {
  LOG("StructLiteral", "Completing struct-literal emission: %p", node);

  // TODO: Find a way around these const casts.
  type::Struct *s =
      &const_cast<type::Struct &>(context().LoadType(node).as<type::Struct>());
  if (s->completeness() == type::Completeness::Complete) {
    LOG("StructLiteral", "Already complete, exiting: %p", node);
    return true;
  }

  ASSIGN_OR(return false,  //
                   auto fn, StructCompletionFn(*this, s, node->fields()));
  // TODO: What if execution fails.
  ir::interpreter::Interpret(shared_context(), fn);

  LOG("StructLiteral", "Completed %s which is a struct %s with %u field(s).",
      node->DebugString(), *s, s->fields().size());
  return true;
}

void Compiler::EmitCopyAssign(
    ast::StructLiteral const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  auto t = context().qual_types(node)[0].type();
  ASSERT(to.size() == 1u);
  ir::PartialResultBuffer buffer;
  EmitToBuffer(node, buffer);
  CopyAssignmentEmitter emitter(*this);
  emitter(to[0], type::Typed(buffer[0], t));
}

void Compiler::EmitMoveAssign(
    ast::StructLiteral const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  auto t = context().qual_types(node)[0].type();
  ASSERT(to.size() == 1u);
  ir::PartialResultBuffer buffer;
  EmitToBuffer(node, buffer);
  MoveAssignmentEmitter emitter(*this);
  emitter(to[0], type::Typed(buffer[0], t));
}

void Compiler::EmitCopyInit(
    ast::StructLiteral const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  auto t = context().qual_types(node)[0].type();
  ASSERT(to.size() == 1u);
  ir::PartialResultBuffer buffer;
  EmitToBuffer(node, buffer);
  CopyAssignmentEmitter emitter(*this);
  emitter(to[0], type::Typed(buffer[0], t));
}

void Compiler::EmitMoveInit(
    ast::StructLiteral const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  auto t = context().qual_types(node)[0].type();
  ASSERT(to.size() == 1u);
  ir::PartialResultBuffer buffer;
  EmitToBuffer(node, buffer);
  MoveAssignmentEmitter emitter(*this);
  emitter(to[0], type::Typed(buffer[0], t));
}

}  // namespace compiler

#include <utility>

#include "absl/types/span.h"
#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/emit/common.h"
#include "compiler/emit/destroy.h"
#include "compiler/instructions.h"
#include "compiler/module.h"
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
  auto [fn, inserted] = c.context().ir().InsertMoveInit(s, s);
  if (inserted) {
    c.push_current(&*fn);
    absl::Cleanup cleanup = [&] { c.state().current.pop_back(); };
    c.current_block() = c.current().group->entry();

    auto from = ir::Reg::Arg(0);
    auto to   = ir::Reg::Out(0);

    size_t i = 0;
    for (auto const &field : s->fields()) {
      auto to_ref = c.current_block()->Append(
          ir::StructIndexInstruction{.addr        = to,
                                     .index       = i,
                                     .struct_type = s,
                                     .result = c.current().group->Reserve()});
      auto from_val = c.current_block()->Append(
          ir::StructIndexInstruction{.addr        = from,
                                     .index       = i,
                                     .struct_type = s,
                                     .result = c.current().group->Reserve()});

      ir::RegOr<ir::addr_t> r(PtrFix(c.current(), from_val, field.type));
      ir::PartialResultBuffer buffer;
      buffer.append(r);
      c.EmitMoveInit(type::Typed<ir::Reg>(to_ref, field.type), buffer);
      ++i;
    }
    c.current_block()->set_jump(ir::JumpCmd::Return());
    c.context().ir().WriteByteCode<EmitByteCode>(fn);
  }
  return fn;
}

ir::Fn InsertGeneratedCopyInit(Compiler &c, type::Struct *s) {
  auto [fn, inserted] = c.context().ir().InsertCopyInit(s, s);
  if (inserted) {
    c.push_current(&*fn);
    absl::Cleanup cleanup = [&] { c.state().current.pop_back(); };
    c.current_block() = c.current().group->entry();

    auto from = ir::Reg::Arg(0);
    auto to   = ir::Reg::Out(0);

    size_t i = 0;
    for (auto const &field : s->fields()) {
      auto to_ref = c.current_block()->Append(
          ir::StructIndexInstruction{.addr        = to,
                                     .index       = i,
                                     .struct_type = s,
                                     .result = c.current().group->Reserve()});
      auto from_val = c.current_block()->Append(
          ir::StructIndexInstruction{.addr        = from,
                                     .index       = i,
                                     .struct_type = s,
                                     .result = c.current().group->Reserve()});

      ir::PartialResultBuffer buffer;
      buffer.append(PtrFix(c.current(), from_val, field.type));
      c.EmitCopyInit(type::Typed<ir::Reg>(to_ref, field.type), buffer);
      ++i;
    }
    c.current_block()->set_jump(ir::JumpCmd::Return());
    c.context().ir().WriteByteCode<EmitByteCode>(fn);
  }
  return fn;
}

ir::Fn InsertGeneratedMoveAssign(Compiler &c, type::Struct *s) {
  auto [fn, inserted] = c.context().ir().InsertMoveAssign(s, s);
  if (inserted) {
    c.push_current(&*fn);
    absl::Cleanup cleanup = [&] { c.state().current.pop_back(); };
    c.current_block() = fn->entry();
    auto var          = ir::Reg::Arg(0);
    auto val          = ir::Reg::Arg(1);

    for (size_t i = 0; i < s->fields().size(); ++i) {
      ir::Reg to_ref = c.current_block()->Append(
          ir::StructIndexInstruction{.addr        = var,
                                     .index       = i,
                                     .struct_type = s,
                                     .result = c.current().group->Reserve()});
      ir::Reg from_ref = c.current_block()->Append(
          ir::StructIndexInstruction{.addr        = val,
                                     .index       = i,
                                     .struct_type = s,
                                     .result = c.current().group->Reserve()});

      ir::PartialResultBuffer buffer;
      buffer.append(PtrFix(c.current(), from_ref, s->fields()[i].type));
      c.EmitCopyAssign(
          type::Typed<ir::RegOr<ir::addr_t>>(to_ref, s->fields()[i].type),
          type::Typed(buffer[0], s->fields()[i].type));
    }

    c.current_block()->set_jump(ir::JumpCmd::Return());
    c.context().ir().WriteByteCode<EmitByteCode>(fn);
  }
  return fn;
}

ir::Fn InsertGeneratedCopyAssign(Compiler &c, type::Struct *s) {
  auto [fn, inserted] = c.context().ir().InsertCopyAssign(s, s);
  if (inserted) {
    c.push_current(&*fn);
    absl::Cleanup cleanup = [&] { c.state().current.pop_back(); };
    c.current_block() = fn->entry();
    auto var          = ir::Reg::Arg(0);
    auto val          = ir::Reg::Arg(1);

    for (size_t i = 0; i < s->fields().size(); ++i) {
      ir::Reg to_ref = c.current_block()->Append(
          ir::StructIndexInstruction{.addr        = var,
                                     .index       = i,
                                     .struct_type = s,
                                     .result = c.current().group->Reserve()});
      ir::Reg from_ref = c.current_block()->Append(
          ir::StructIndexInstruction{.addr        = val,
                                     .index       = i,
                                     .struct_type = s,
                                     .result = c.current().group->Reserve()});
      ir::PartialResultBuffer buffer;
      buffer.append(PtrFix(c.current(), from_ref, s->fields()[i].type));
      c.EmitCopyAssign(
          type::Typed<ir::RegOr<ir::addr_t>>(to_ref, s->fields()[i].type),
          type::Typed(buffer[0], s->fields()[i].type));
    }

    c.current_block()->set_jump(ir::JumpCmd::Return());
    c.context().ir().WriteByteCode<EmitByteCode>(fn);
  }
  return fn;
}

}  // namespace

std::optional<ir::CompiledFn> StructCompletionFn(
    CompilationDataReference data, type::Struct *s,
    absl::Span<ast::Declaration const> field_decls) {
  ASSERT(s->completeness() == type::Completeness::DataComplete);

  ir::CompiledFn fn(type::Func({}, {}));
  data.push_current(&fn);
  absl::Cleanup cleanup = [&] { data.state().current.pop_back(); };
  Compiler c(data);
  // TODO this is essentially a copy of the body of
  // FunctionLiteral::EmitToBuffer. Factor these out together.
  data.current_block() = fn.entry();

  std::vector<type::StructInstruction::Field> constants;
  bool needs_dtor = false;
  for (auto const &field : s->fields()) {
    needs_dtor = needs_dtor or field.type.get()->HasDestructor();
  }

  std::optional<ir::Fn> user_dtor;
  std::vector<ir::Fn> move_inits, copy_inits, move_assignments,
      copy_assignments;
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
        auto f = c.EmitAs<ir::Fn>(id.declaration().init_val());
        switch (f.value().type()->params().size()) {
          case 1: move_inits.push_back(f.value()); break;
          case 2: move_assignments.push_back(f.value()); break;
          default: UNREACHABLE();
        }
      } else if (id.name() == "copy") {
        auto f = c.EmitAs<ir::Fn>(id.declaration().init_val());
        switch (f.value().type()->params().size()) {
          case 1: copy_inits.push_back(f.value()); break;
          case 2: copy_assignments.push_back(f.value()); break;
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
    auto [full_dtor, inserted] = data.context().ir().InsertDestroy(s);
    if (inserted) {
      data.push_current(&*full_dtor);
      absl::Cleanup cleanup = [&] { c.state().current.pop_back(); };
      data.current_block() = data.current().group->entry();
      auto var             = ir::Reg::Arg(0);
      if (user_dtor) {
        // TODO: Should probably force-inline this.
        ir::PartialResultBuffer args;
        args.append(var);
        // TODO: Constants
        data.current_block()->Append(ir::CallInstruction(
            full_dtor.type(), *user_dtor, std::move(args), ir::OutParams()));
      }
      for (int i = s->fields().size() - 1; i >= 0; --i) {
        DestructionEmitter de(c);
        de(s->fields()[i].type,
           c.current_block()->Append(ir::StructIndexInstruction{
               .addr        = var,
               .index       = i,
               .struct_type = s,
               .result      = c.current().group->Reserve()}));
      }

      data.current_block()->set_jump(ir::JumpCmd::Return());
      data.context().ir().WriteByteCode<EmitByteCode>(full_dtor);

      dtor = full_dtor;
    }
  } else {
    if (user_dtor) { dtor = *user_dtor; }
  }

  if (move_inits.empty() and copy_inits.empty()) {
    move_inits.push_back(InsertGeneratedMoveInit(c, s));
    copy_inits.push_back(InsertGeneratedCopyInit(c, s));
  }

  if (move_assignments.empty() and copy_assignments.empty()) {
    move_assignments.push_back(InsertGeneratedMoveAssign(c, s));
    copy_assignments.push_back(InsertGeneratedCopyAssign(c, s));
  }

  data.current_block()->Append(
      type::StructInstruction{.struct_          = s,
                              .constants        = std::move(constants),
                              .move_inits       = std::move(move_inits),
                              .copy_inits       = std::move(copy_inits),
                              .move_assignments = std::move(move_assignments),
                              .copy_assignments = std::move(copy_assignments),
                              .dtor             = dtor});
  data.current_block()->set_jump(ir::JumpCmd::Return());

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
  InterpretAtCompileTime(fn);

  LOG("StructLiteral", "Completed %s which is a struct %s with %u field(s).",
      node->DebugString(), *s, s->fields().size());
  return true;
}

}  // namespace compiler

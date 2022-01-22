#include <string_view>

#include "absl/types/span.h"
#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/emit/copy_move_assignment.h"
#include "compiler/emit/initialize.h"
#include "ir/value/addr.h"
#include "ir/value/reg_or.h"
#include "type/typed_value.h"

namespace compiler {

void Compiler::EmitToBuffer(ast::DesignatedInitializer const *node,
                            ir::PartialResultBuffer &out) {
  auto t     = context().qual_types(node)[0].type();
  auto alloc = state().TmpAlloca(t);
  auto typed_alloc =
      type::Typed<ir::RegOr<ir::addr_t>>(ir::RegOr<ir::addr_t>(alloc), t);
  EmitMoveInit(node, absl::MakeConstSpan(&typed_alloc, 1));
  out.append(alloc);
}

void Compiler::EmitMoveAssign(
    ast::DesignatedInitializer const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  ir::PartialResultBuffer buffer;
  EmitToBuffer(node, buffer);
  MoveAssignmentEmitter emitter(*this);
  emitter(to[0], type::Typed(buffer[0], context().qual_types(node)[0].type()));
}

void Compiler::EmitCopyAssign(
    ast::DesignatedInitializer const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  ir::PartialResultBuffer buffer;
  EmitToBuffer(node, buffer);
  CopyAssignmentEmitter emitter(*this);
  emitter(to[0], type::Typed(buffer[0], context().qual_types(node)[0].type()));
}

void Compiler::EmitMoveInit(
    ast::DesignatedInitializer const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  // TODO actual initialization with these field members.
  auto &struct_type  = context().qual_types(node)[0].type().as<type::Struct>();
  auto const &fields = struct_type.fields();
  for (size_t i = 0; i < fields.size(); ++i) {
    auto const &field = fields[i];

    for (auto const *assignment : node->assignments()) {
      for (auto const *expr : assignment->lhs()) {
        std::string_view field_name = expr->as<ast::Identifier>().name();
        // Skip default initialization if we're going to use the designated
        // initializer.
        if (field_name == field.name) { goto next_field; }
      }
    }

    {
      type::Typed<ir::Reg> field_reg(
          current_block()->Append(
              ir::StructIndexInstruction{.addr        = to[0]->reg(),
                                         .index       = i,
                                         .struct_type = &struct_type,
                                         .result = current().subroutine->Reserve()}),
          struct_type.fields()[i].type);
      if (field.initial_value.empty()) {
        DefaultInitializationEmitter emitter(*this);
        emitter(field_reg.type(), *field_reg);
      } else {
        CopyAssignmentEmitter emitter(*this);
        emitter(field_reg, type::Typed(field.initial_value[0], field.type));
      }
    }
  next_field:;
  }

  for (auto const *assignment : node->assignments()) {
    if (assignment->lhs().size() != 1) { NOT_YET(assignment->lhs().size()); }
    if (assignment->rhs().size() != 1) { NOT_YET(assignment->rhs().size()); }

    auto const &id     = assignment->lhs()[0]->as<ast::Identifier>();
    auto const *f      = struct_type.field(id.name());
    size_t field_index = struct_type.index(f->name);

    type::Typed<ir::RegOr<ir::addr_t>> field_reg(
        current_block()->Append(
            ir::StructIndexInstruction{.addr        = to[0]->reg(),
                                       .index       = field_index,
                                       .struct_type = &struct_type,
                                       .result = current().subroutine->Reserve()}),
        struct_type.fields()[field_index].type);
    EmitMoveInit(assignment->rhs()[0], absl::MakeConstSpan(&field_reg, 1));
  }
}

void Compiler::EmitCopyInit(
    ast::DesignatedInitializer const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  // TODO actual initialization with these field members.
  auto &struct_type  = context().qual_types(node)[0].type().as<type::Struct>();
  auto const &fields = struct_type.fields();
  for (size_t i = 0; i < fields.size(); ++i) {
    auto const &field = fields[i];

    for (auto const *assignment : node->assignments()) {
      for (auto const *expr : assignment->lhs()) {
        std::string_view field_name = expr->as<ast::Identifier>().name();
        // Skip default initialization if we're going to use the designated
        // initializer.
        if (field_name == field.name) { goto next_field; }
      }
    }

    {
      type::Typed<ir::Reg> field_reg(
          current_block()->Append(
              ir::StructIndexInstruction{.addr        = to[0]->reg(),
                                         .index       = i,
                                         .struct_type = &struct_type,
                                         .result = current().subroutine->Reserve()}),
          struct_type.fields()[i].type);
      if (field.initial_value.empty()) {
        DefaultInitializationEmitter emitter(*this);
        emitter(field_reg.type(), *field_reg);
      } else {
        CopyAssignmentEmitter emitter(*this);
        emitter(field_reg, type::Typed(field.initial_value[0], field.type));
      }
    }
  next_field:;
  }

  for (auto const *assignment : node->assignments()) {
    if (assignment->lhs().size() != 1) { NOT_YET(assignment->lhs().size()); }
    if (assignment->rhs().size() != 1) { NOT_YET(assignment->rhs().size()); }

    auto const &id     = assignment->lhs()[0]->as<ast::Identifier>();
    auto const *f      = struct_type.field(id.name());
    size_t field_index = struct_type.index(f->name);
    type::Typed<ir::RegOr<ir::addr_t>> field_reg(
        current_block()->Append(
            ir::StructIndexInstruction{.addr        = to[0]->reg(),
                                       .index       = field_index,
                                       .struct_type = &struct_type,
                                       .result = current().subroutine->Reserve()}),
        struct_type.fields()[field_index].type);
    EmitCopyInit(assignment->rhs()[0], absl::MakeConstSpan(&field_reg, 1));
  }
}

}  // namespace compiler

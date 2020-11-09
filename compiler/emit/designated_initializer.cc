#include <string_view>

#include "absl/types/span.h"
#include "ast/ast.h"
#include "compiler/compiler.h"
#include "ir/builder.h"
#include "ir/value/addr.h"
#include "ir/value/reg_or.h"
#include "ir/value/value.h"
#include "type/typed_value.h"

namespace compiler {

ir::Value Compiler::EmitValue(ast::DesignatedInitializer const *node) {
  auto t     = context().qual_type(node)->type();
  auto alloc = builder().TmpAlloca(t);
  auto typed_alloc =
      type::Typed<ir::RegOr<ir::Addr>>(ir::RegOr<ir::Addr>(alloc), t);
  EmitMoveInit(node, absl::MakeConstSpan(&typed_alloc, 1));
  return ir::Value(alloc);
}

void Compiler::EmitAssign(
    ast::DesignatedInitializer const *node,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  ASSERT(to.size() == 1u);
  EmitMoveAssign(to[0],
                 type::Typed<ir::Value>(EmitValue(node),
                                        context().qual_type(node)->type()));
}

void Compiler::EmitMoveInit(
    ast::DesignatedInitializer const *node,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  ASSERT(to.size() == 1u);
  // TODO actual initialization with these field members.
  auto &struct_type  = context().qual_type(node)->type().as<type::Struct>();
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
      auto field_reg = builder().FieldRef(to[0]->reg(), &struct_type, i);
      if (field.initial_value.empty()) {
        EmitDefaultInit(field_reg);
      } else {
        EmitCopyAssign(field_reg,
                       type::Typed<ir::Value>(field.initial_value, field.type));
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
    type::Typed<ir::RegOr<ir::Addr>> field_reg =
        builder().FieldRef(to[0]->reg(), &struct_type, field_index);
    EmitMoveInit(assignment->rhs()[0], absl::MakeConstSpan(&field_reg, 1));
  }
}

void Compiler::EmitCopyInit(
    ast::DesignatedInitializer const *node,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  ASSERT(to.size() == 1u);
  // TODO actual initialization with these field members.
  auto &struct_type  = context().qual_type(node)->type().as<type::Struct>();
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
      auto field_reg = builder().FieldRef(to[0]->reg(), &struct_type, i);
      if (field.initial_value.empty()) {
        EmitDefaultInit(field_reg);
      } else {
        EmitCopyAssign(field_reg,
                       type::Typed<ir::Value>(field.initial_value, field.type));
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
    auto field_reg =
        builder().FieldRef(to[0]->reg(), &struct_type, field_index);
    type::Typed<ir::RegOr<ir::Addr>> lhs(*field_reg, field_reg.type());
    EmitCopyInit(assignment->rhs()[0], absl::MakeConstSpan(&lhs, 1));
  }
}

}  // namespace compiler

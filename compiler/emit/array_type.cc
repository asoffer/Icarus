#include "ast/ast.h"
#include "compiler/compiler.h"
#include "type/array.h"

namespace compiler {

ir::Value Compiler::EmitValue(ast::ArrayType const *node) {
  auto t = EmitValue(node->data_type()).get<ir::RegOr<type::Type>>();
  // Size must be at least 1 by construction, so `.size() - 1` will not
  // overflow.
  for (int i = node->lengths().size() - 1; i >= 0; --i) {
    ir::Value len = EmitValue(node->length(i));
    t             = current_block()->Append(type::ArrayInstruction{
        .length =
            builder().CastTo<type::Array::length_t>(type::Typed<ir::Value>(
                len, context().qual_types(node->length(i))[0].type())),
        .data_type = t,
        .result    = builder().CurrentGroup()->Reserve()});
  }
  return ir::Value(t);
}

void Compiler::EmitCopyAssign(
    ast::ArrayType const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  builder().Store(EmitValue(node).get<ir::RegOr<type::Type>>(), *to[0]);
}

void Compiler::EmitMoveAssign(
    ast::ArrayType const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  builder().Store(EmitValue(node).get<ir::RegOr<type::Type>>(), *to[0]);
}

void Compiler::EmitCopyInit(
    ast::ArrayType const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  builder().Store(EmitValue(node).get<ir::RegOr<type::Type>>(), *to[0]);
}

void Compiler::EmitMoveInit(
    ast::ArrayType const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  builder().Store(EmitValue(node).get<ir::RegOr<type::Type>>(), *to[0]);
}

bool Compiler::PatternMatch(
    ast::ArrayType const *node, PatternMatchingContext &pmc,
    absl::flat_hash_map<ast::Declaration::Id const *, ir::Value> &bindings) {
  type::Type t         = pmc.value.get<type::Type>(0);

  type::Array const *a = t.if_as<type::Array>();
  if (not a) { return false; }

  size_t index = pmc.array_type_index;

  base::untyped_buffer length_buffer;
  length_buffer.append(a->length());
  EnqueuePatternMatch(node->length(index),
                      {.type = type::I64, .value = std::move(length_buffer)});

  base::untyped_buffer data_type_buffer;
  data_type_buffer.append(a->data_type());

  if (index + 1 == node->lengths().size()) {
    EnqueuePatternMatch(
        node->data_type(),
        {.type = type::Type_, .value = std::move(data_type_buffer)});
  } else {
    // TODO: Support integer constants in general.
    EnqueuePatternMatch(node, {.type             = type::I64,
                               .value            = std::move(data_type_buffer),
                               .array_type_index = index + 1});
  }

  return true;
}

}  // namespace compiler

#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/emit/common.h"
#include "compiler/emit/compiler_common.h"
#include "type/array.h"

namespace compiler {

void Compiler::EmitToBuffer(ast::ArrayType const *node,
                            ir::PartialResultBuffer &out) {
  EmitToBuffer(node->data_type(), out);
  ir::RegOr<type::Type> t = out.get<type::Type>(0);
  out.pop_back();
  // Size must be at least 1 by construction, so `.size() - 1` will not
  // overflow.
  for (int i = node->lengths().size() - 1; i >= 0; --i) {
    auto len = EmitCast(*this, context().typed(node->length(i)), type::Integer)
                   .back()
                   .get<ir::addr_t>();
    t = current_block()->Append(type::ArrayInstruction{
        .length = len, .data_type = t, .result = current().subroutine->Reserve()});
  }
  out.append(t);
}

void Compiler::EmitCopyAssign(
    ast::ArrayType const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  current_block()->Append(ir::StoreInstruction<type::Type>{
      .value    = EmitAs<type::Type>(node),
      .location = *to[0],
  });
}

void Compiler::EmitMoveAssign(
    ast::ArrayType const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  current_block()->Append(ir::StoreInstruction<type::Type>{
      .value    = EmitAs<type::Type>(node),
      .location = *to[0],
  });
}

void Compiler::EmitCopyInit(
    ast::ArrayType const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  current_block()->Append(ir::StoreInstruction<type::Type>{
      .value    = EmitAs<type::Type>(node),
      .location = *to[0],
  });
}

void Compiler::EmitMoveInit(
    ast::ArrayType const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  current_block()->Append(ir::StoreInstruction<type::Type>{
      .value    = EmitAs<type::Type>(node),
      .location = *to[0],
  });
}

bool Compiler::PatternMatch(
    ast::ArrayType const *node, PatternMatchingContext &pmc,
    absl::flat_hash_map<ast::Declaration::Id const *, ir::CompleteResultBuffer>
        &bindings) {
  type::Type t = pmc.value.get<type::Type>(0);

  type::Array const *a = t.if_as<type::Array>();
  if (not a) { return false; }

  size_t index = pmc.array_type_index;

  ir::CompleteResultBuffer length_buffer;
  length_buffer.append(a->length());
  state().EnqueuePatternMatch(
      node->length(index),
      {.type = type::Integer, .value = std::move(length_buffer)});

  ir::CompleteResultBuffer data_type_buffer;
  data_type_buffer.append(a->data_type());

  if (index + 1 == node->lengths().size()) {
    state().EnqueuePatternMatch(
        node->data_type(),
        {.type = type::Type_, .value = std::move(data_type_buffer)});
  } else {
    state().EnqueuePatternMatch(node, {.type  = type::Integer,
                                       .value = std::move(data_type_buffer),
                                       .array_type_index = index + 1});
  }

  return true;
}

}  // namespace compiler

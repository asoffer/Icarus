#include "ast/ast.h"
#include "compiler/compiler.h"
#include "type/slice.h"

namespace compiler {

void Compiler::EmitToBuffer(ast::SliceType const *node,
                            ir::PartialResultBuffer &out) {
  EmitToBuffer(node->data_type(), out);
  auto t = out.get<type::Type>(0);
  out.clear();
  out.append(current_block()->Append(type::SliceInstruction{
      .data_type = t, .result = builder().CurrentGroup()->Reserve()}));
}

void Compiler::EmitCopyAssign(
    ast::SliceType const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  builder().Store(EmitAs<type::Type>(node), *to[0]);
}

void Compiler::EmitMoveAssign(
    ast::SliceType const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  builder().Store(EmitAs<type::Type>(node), *to[0]);
}

void Compiler::EmitCopyInit(
    ast::SliceType const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  builder().Store(EmitAs<type::Type>(node), *to[0]);
}

void Compiler::EmitMoveInit(
    ast::SliceType const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  builder().Store(EmitAs<type::Type>(node), *to[0]);
}

bool Compiler::PatternMatch(
    ast::SliceType const *node, PatternMatchingContext &pmc,
    absl::flat_hash_map<ast::Declaration::Id const *, ir::CompleteResultBuffer>
        &bindings) {
  auto t = pmc.value.get<type::Type>(0);
  if (not t.is<type::Slice>()) { return false; }
  pmc.value.clear();
  pmc.value.append(type::Type(t.as<type::Slice>().data_type()));
  return PatternMatch(node->data_type(), pmc, bindings);
}

}  // namespace compiler

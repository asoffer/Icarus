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

bool Compiler::PatternMatch(ast::ArrayType const *node,
                            PatternMatchingContext &pmc) {
  type::Type t = pmc.value.get<type::Type>(0);
  for (auto const *len : node->lengths()) {
    LOG("", "Comparing %s on %s", len->DebugString(), t);
    type::Array const *a = t.if_as<type::Array>();
    if (not a) { return false; }

    base::untyped_buffer buff;
    buff.append(a->length());
    auto old_type   = std::exchange(pmc.type, type::I64);
    auto old_value  = std::exchange(pmc.value, std::move(buff));
    absl::Cleanup c = [&] {
      pmc.type  = old_type;
      pmc.value = old_value;
    };

    if (PatternMatch(len, pmc)) {
      t = a->data_type();
    } else {
      return false;
    }
  }

  pmc.type  = type::Type_;
  pmc.value.set<type::Type>(0, t);
  return PatternMatch(node->data_type(), pmc);
}

}  // namespace compiler

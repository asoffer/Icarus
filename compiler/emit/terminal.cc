#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/context.h"

namespace compiler {

struct TerminalMatchError {
  static constexpr std::string_view kCategory = "terminal-match-error";
  static constexpr std::string_view kName     = "pattern-error";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(R"(Pattern matching failed due to unequal values.
  Pattern value: %s
  Matched value: %s)",
                         pattern_value, matched_value),
        diagnostic::SourceQuote(src).Highlighted(
            range, diagnostic::Style::ErrorText()));
  }

  frontend::SourceRange range;
  std::string pattern_value;
  std::string matched_value;
};

void Compiler::EmitToBuffer(ast::Terminal const *node,
                            base::untyped_buffer &out) {
  out = node->value();
}

// TODO: Unit tests
void Compiler::EmitCopyAssign(
    ast::Terminal const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  auto t = context().qual_types(node)[0].type();
  ASSERT(to.size() == 1u);
  base::untyped_buffer buffer;
  EmitToBuffer(node, buffer);
  EmitCopyAssign(to[0], type::Typed<ir::Value>(ToValue(buffer, t), t));
}

// TODO: Unit tests
void Compiler::EmitMoveAssign(
    ast::Terminal const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  auto t = context().qual_types(node)[0].type();
  ASSERT(to.size() == 1u);
  base::untyped_buffer buffer;
  EmitToBuffer(node, buffer);
  EmitMoveAssign(to[0], type::Typed<ir::Value>(ToValue(buffer, t), t));
}

// TODO: Unit tests
void Compiler::EmitCopyInit(
    ast::Terminal const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  auto t = context().qual_types(node)[0].type();
  ASSERT(to.size() == 1u);
  base::untyped_buffer buffer;
  EmitToBuffer(node, buffer);
  EmitCopyAssign(to[0], type::Typed<ir::Value>(ToValue(buffer, t), t));
}

// TODO: Unit tests
void Compiler::EmitMoveInit(
    ast::Terminal const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  auto t = context().qual_types(node)[0].type();
  ASSERT(to.size() == 1u);
  base::untyped_buffer buffer;
  EmitToBuffer(node, buffer);
  EmitMoveAssign(to[0], type::Typed<ir::Value>(ToValue(buffer, t), t));
}

bool Compiler::PatternMatch(
    ast::Terminal const *node, PatternMatchingContext &pmc,
    absl::flat_hash_map<ast::Declaration::Id const *, ir::Value> &bindings) {
  auto t        = context().qual_types(node)[0].type();
  auto const &p = t.as<type::Primitive>();
  return p.Apply([&]<typename T>()->bool {
    T pattern_value = node->value().template get<T>(0);
    T matched_value = pmc.value.template get<T>(0);
    if (matched_value == pattern_value) { return true; }

    diag().Consume(TerminalMatchError{
        .range         = node->range(),
        .pattern_value = base::stringify(pattern_value),
        .matched_value = base::stringify(matched_value),
    });
    return false;
  });
}

}  // namespace compiler

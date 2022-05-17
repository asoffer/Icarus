#include "ast/ast.h"
#include "base/universal_print.h"
#include "compiler/compiler.h"
#include "compiler/context.h"
#include "compiler/emit/copy_move_assignment.h"

namespace compiler {

struct TerminalMatchError {
  static constexpr std::string_view kCategory = "terminal-match-error";
  static constexpr std::string_view kName     = "pattern-error";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(R"(Pattern matching failed due to unequal values.
  Pattern value: %s
  Matched value: %s)",
                         pattern_value, matched_value),
        diagnostic::SourceQuote().Highlighted(range,
                                              diagnostic::Style::ErrorText()));
  }

  std::string_view range;
  std::string pattern_value;
  std::string matched_value;
};

void Compiler::EmitToBuffer(ast::Terminal const *node,
                            ir::PartialResultBuffer &out) {
  if (node->type() == base::meta<ir::Slice>) {
    auto *sp = reinterpret_cast<ir::Slice const *>(node->value().raw().data());
    out.append(node->value().raw().data());
  } else if (node->type() == base::meta<ir::Integer>) {
    auto alloc = state().TmpAlloca(type::Integer);
    current_block()->Append(ir::CompileTime<ir::Action::CopyInit, ir::Integer>{
        .from = const_cast<std::byte *>(node->value().raw().data()),
        .to   = alloc});
    out.append(alloc);
  } else {
    out.append(node->value());
  }
}

void Compiler::EmitCopyAssign(
    ast::Terminal const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  auto t = context().qual_types(node)[0].type();
  ASSERT(to.size() == 1u);
  ir::PartialResultBuffer buffer;
  EmitToBuffer(node, buffer);
  CopyAssignmentEmitter emitter(*this);
  emitter(to[0], type::Typed(buffer[0], t));
}

void Compiler::EmitMoveAssign(
    ast::Terminal const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  auto t = context().qual_types(node)[0].type();
  ASSERT(to.size() == 1u);
  ir::PartialResultBuffer buffer;
  EmitToBuffer(node, buffer);
  MoveAssignmentEmitter emitter(*this);
  emitter(to[0], type::Typed(buffer[0], t));
}

void Compiler::EmitCopyInit(
    ast::Terminal const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  auto t = context().qual_types(node)[0].type();
  ASSERT(to.size() == 1u);
  ir::PartialResultBuffer buffer;
  EmitToBuffer(node, buffer);
  CopyAssignmentEmitter emitter(*this);
  emitter(to[0], type::Typed(buffer[0], t));
}

void Compiler::EmitMoveInit(
    ast::Terminal const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  auto t = context().qual_types(node)[0].type();
  ASSERT(to.size() == 1u);
  ir::PartialResultBuffer buffer;
  EmitToBuffer(node, buffer);
  MoveAssignmentEmitter emitter(*this);
  emitter(to[0], type::Typed(buffer[0], t));
}

bool Compiler::PatternMatch(
    ast::Terminal const *node, PatternMatchingContext &pmc,
    absl::flat_hash_map<ast::Declaration::Id const *, ir::CompleteResultBuffer>
        &bindings) {
  auto t        = context().qual_types(node)[0].type();
  auto const &p = t.as<type::Primitive>();
  return p.Apply([&]<typename T>() -> bool {
    if constexpr (base::meta<T> == base::meta<type::Argument>) {
      UNREACHABLE();
    } else {
      T pattern_value = node->value().template get<T>();
      T matched_value = pmc.value.template get<T>(0);
      if (matched_value == pattern_value) { return true; }

      diag().Consume(TerminalMatchError{
          .range         = node->range(),
          .pattern_value = base::UniversalPrintToString(pattern_value),
          .matched_value = base::UniversalPrintToString(matched_value),
      });
      return false;
    }
  });
}

}  // namespace compiler

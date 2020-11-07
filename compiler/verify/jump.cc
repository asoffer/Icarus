#include "type/jump.h"
#include "ast/ast.h"
#include "compiler/compiler.h"
#include "core/params.h"
#include "type/pointer.h"
#include "type/qual_type.h"

namespace compiler {
namespace {

struct JumpStateInitialValue {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "jump-state-initial-value";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "The state in a `jump` must be declared without an initial value."),
        diagnostic::SourceQuote(src).Highlighted(
            range, diagnostic::Style::ErrorText()));
  }

  frontend::SourceRange range;
};

struct ConstantJumpState {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "constant-jump-state";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "The state in a `jump` must be declared as non-constant."),
        diagnostic::SourceQuote(src).Highlighted(
            range, diagnostic::Style::ErrorText()));
  }

  frontend::SourceRange range;
};

struct NonPointerJumpState {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "non-pointer-jump-state";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "The state in a `jump` must be a pointer, but I found a `%s`.",
            type.to_string()),
        diagnostic::SourceQuote(src).Highlighted(
            range, diagnostic::Style::ErrorText()));
  }

  frontend::SourceRange range;
  type::Type type;
};

struct BufferPointerJumpState {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "buffer-pointer-jump-state";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "The state in a `jump` must be a pointer, not a buffer-pointer."),
        diagnostic::SourceQuote(src).Highlighted(
            range, diagnostic::Style::ErrorText()));
  }

  frontend::SourceRange range;
};

}  // namespace

type::QualType Compiler::VerifyType(ast::Jump const *node) {
  LOG("Jump", "%s", node->DebugString());

  bool err         = false;
  type::Type state = nullptr;

  if (node->state()) {
    auto state_qual_type = VerifyType(node->state());
    err                  = not state_qual_type.ok();
    if (not err) {
      state = state_qual_type.type();
      if (state_qual_type.constant()) {
        // Intentionally not setting `err` because there is an obvious recovery
        // mechanism (make the state non-constant).
        diag().Consume(ConstantJumpState{.range = node->state()->range()});
      }

      if (not state_qual_type.type().is<type::Pointer>()) {
        diag().Consume(NonPointerJumpState{
            .range = node->state()->range(),
            .type  = state,
        });
        err = true;
      } else if (auto const *buf_ptr =
                     state_qual_type.type().if_as<type::BufferPointer>()) {
        // Intentionally not setting `err` because there is an obvious recovery
        // mechanism (drop the buffer-ness of the pointer type).
        diag().Consume(BufferPointerJumpState{.range = node->state()->range()});

        // Recover by removing the buffer-ness.
        state = type::Ptr(buf_ptr->pointee());
      }

      if (node->state()->init_val()) {
        // Intentionally not setting `err` because there is an obvious recovery
        // mechanism (Use the type that we inferred. Note that the inferred type
        // may not be a pointer, but in such cases, we've already set `err`
        // above).
        diag().Consume(JumpStateInitialValue{
            .range = node->state()->init_val()->range(),
        });
      }
    }
  }

  core::Params<type::Type> param_types =
      node->params().Transform([&](auto const &param) {
        auto v = VerifyType(param.get());
        err |= not v.ok();
        return v.type();
      });

  if (err) { return type::QualType::Error(); }

  for (auto const *stmt : node->stmts()) { err &= not VerifyType(stmt).ok(); }

  return context().set_qual_type(
      node, type::QualType::Constant(type::Jmp(state, param_types)));
}

}  // namespace compiler

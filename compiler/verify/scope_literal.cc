#include "ast/ast.h"
#include "compiler/compiler.h"
#include "type/jump.h"
#include "type/primitive.h"
#include "type/qual_type.h"
#include "type/typed_value.h"

namespace compiler {
namespace {

struct StateTypeMismatch {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "state-type-mismatch";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    if (expected_type) {
      if (actual_type) {
        return diagnostic::DiagnosticMessage(
            diagnostic::Text("Jump state expected to be a pointer to `%s`, but "
                             "encountered a `%s`.",
                             expected_type.to_string(),
                             actual_type.to_string()),
            diagnostic::SourceQuote(src).Highlighted(range,
                                                     diagnostic::Style{}));
      } else {
        return diagnostic::DiagnosticMessage(
            diagnostic::Text("Jump state expected to be a pointer to `%s`, but "
                             "is stateless.",
                             expected_type.to_string()),
            diagnostic::SourceQuote(src).Highlighted(range,
                                                     diagnostic::Style{}));
      }
    } else {
      return diagnostic::DiagnosticMessage(
          diagnostic::Text(
              "Jump expected to be stateless, but has state of type `%s`.",
              actual_type.to_string()),
          diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
    }
  }

  type::Type actual_type;
  type::Type expected_type;
  frontend::SourceRange range;
};

struct NonJumpInit {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "non-jump-enter";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Scope `enter` must have a `jump` type, but here it "
                         "was defined to have type `%s`.",
                         type.to_string()),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  type::Type type;
  frontend::SourceRange range;
};

struct NonCallableDone {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "non-callable-exit";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Scope `exit` must have a callable type but here it "
                         "was defined to have type `%s`.",
                         type.to_string()),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  type::Type type;
  frontend::SourceRange range;
};

struct NonTypeScopeState {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "non-type-scope-state";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Scope state must be a type, but encountered a `%s`.",
                         type.to_string()),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  type::Type type;
  frontend::SourceRange range;
};

struct NonConstantScopeMember {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "non-constant-scope-member";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("All members of scope literals must be constant."),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  frontend::SourceRange range;
};

struct NonConstantScopeStateType {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "non-constant-scope-state-type";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Scope state types must be constant."),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  frontend::SourceRange range;
};

bool VerifyInit(diagnostic::DiagnosticConsumer &diag,
                ast::Declaration const *decl, type::Type decl_type,
                type::Pointer const *state_type_ptr) {
  auto *jump_type = decl_type.if_as<type::Jump>();
  if (not jump_type) {
    diag.Consume(NonJumpInit{
        .type  = decl_type,
        .range = decl->id_range(),
    });
    return false;
  } else if (type::Type(state_type_ptr) != jump_type->state()) {
    // TODO: Generics where the state-type is generic?
    diag.Consume(StateTypeMismatch{
        .actual_type   = jump_type->state(),
        .expected_type = state_type_ptr ? state_type_ptr->pointee() : nullptr,
        .range         = decl->id_range(),
    });
    return false;
  } else {
    return true;
  }
}

bool VerifyDone(diagnostic::DiagnosticConsumer &diag,
                ast::Declaration const *decl, type::Type decl_type) {
  auto const *callable = decl_type.if_as<type::Callable>();
  if (not callable) {
    diag.Consume(NonCallableDone{
        .type  = decl_type,
        .range = decl->id_range(),
    });
    return false;
  } else {
    return true;
  }
}

}  // namespace

type::QualType Compiler::VerifyType(ast::ScopeLiteral const *node) {
  LOG("ScopeLiteral", "Verifying body of %p: %s", node, node->DebugString());

  auto qt =
      context().set_qual_type(node, type::QualType::Constant(type::Scope));

  type::Type state_type = nullptr;
  if (node->state_type()) {
    type::QualType state_qual_type = VerifyType(node->state_type());
    if (state_qual_type.type() != type::Type_) {
      diag().Consume(NonTypeScopeState{
          .type  = state_qual_type.type(),
          .range = node->state_type()->range(),
      });
      qt.MarkError();
    }

    if (not(state_qual_type.quals() >= type::Quals::Const())) {
      diag().Consume(NonConstantScopeStateType{
          .range = node->state_type()->range(),
      });
      qt.MarkError();
    }

    ASSIGN_OR(return type::QualType::Error(),  //
                     state_type, EvaluateAs<type::Type>(node->state_type()));
  }

  auto const *state_type_ptr = state_type ? type::Ptr(state_type) : nullptr;

  for (auto const &decl : node->decls()) {
    auto qual_type = VerifyType(&decl);
    if (not qual_type.constant()) {
      diag().Consume(NonConstantScopeMember{.range = decl.range()});
      qt.MarkError();
    }

    if (decl.id() == "enter") {
      if (not VerifyInit(diag(), &decl, qual_type.type(), state_type_ptr)) {
        qt.MarkError();
      }
    } else if (decl.id() == "exit") {
      if (not VerifyDone(diag(), &decl, qual_type.type())) { qt.MarkError(); }
    } else {
      // TODO: Quick-exit
      // TODO: Anything else must be a block.
    }
  }

  return qt;
}

}  // namespace compiler

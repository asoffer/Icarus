#include "compiler/verify/internal/assignment_and_initialization.h"

#include <type_traits>

#include "compiler/verify/common.h"
#include "type/cast.h"

namespace compiler::internal {
namespace {

struct MismatchedAssignmentCount {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "mismatched-assignment-count";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "Assigning multiple values but left-hand and right-hand side have "
            "different numbers of elements (`%d` vs. `%d`).",
            to, from),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  size_t to;
  size_t from;
  frontend::SourceRange range;
};

struct MismatchedInitializationCount {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName = "mismatched-initialization-count";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "Initializing multiple values but left-hand and right-hand side "
            "have different numbers of elements (`%d` vs. `%d`).",
            to, from),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  size_t to;
  size_t from;
  frontend::SourceRange range;
};

template <bool IsInit>
bool VerifyImpl(diagnostic::DiagnosticConsumer &diag,
                frontend::SourceRange const &range, type::QualType to,
                type::QualType from) {
  if constexpr (not IsInit) {
    // `to` cannot be a constant if we're assigning, but for initializations
    // it's okay (we could be initializing a constant).
    if (to.constant()) {
      diag.Consume(AssigningToConstant{.to = to.type(), .range = range});
      return false;
    }
  }

  if constexpr (not IsInit) {
    // Initializations do not care about movability.
    if (not from.type().get()->IsMovable()) {
      diag.Consume(ImmovableType{.from = from.type(), .range = range});
      return false;
    }
  }

  if (not type::CanCastImplicitly(from.type(), to.type())) {
    // TODO: Wire through the expressions relevant to this type so we can emit
    // better error messages.
    diag.Consume(InvalidCast{.from  = from.type().to_string(),
                             .to    = to.type().to_string(),
                             .range = range});
    return false;
  } else {
    return true;
  }
}

}  // namespace

bool VerifyInitialization(diagnostic::DiagnosticConsumer &diag,
                          frontend::SourceRange const &range, type::QualType to,
                          type::QualType from) {
  return VerifyImpl<true>(diag, range, to, from);
}

bool VerifyAssignment(diagnostic::DiagnosticConsumer &diag,
                      frontend::SourceRange const &range, type::QualType to,
                      type::QualType from) {
  return VerifyImpl<false>(diag, range, to, from);
}

}  // namespace compiler::internal

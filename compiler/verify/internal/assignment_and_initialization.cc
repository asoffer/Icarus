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

  size_t expansion_size = to.expansion_size();
  if (expansion_size != from.expansion_size()) {
    using DiagnosticType =
        std::conditional_t<IsInit, MismatchedInitializationCount,
                           MismatchedAssignmentCount>;
    diag.Consume(DiagnosticType{.to    = to.expansion_size(),
                                .from  = from.expansion_size(),
                                .range = range});
    return false;
  }

  type::Type to_type =
      expansion_size == 1
          ? to.type()
          : type::Tup({to.expanded().begin(), to.expanded().end()});
  type::Type from_type =
      expansion_size == 1
          ? from.type()
          : type::Tup({from.expanded().begin(), from.expanded().end()});

  if constexpr (not IsInit) {
    // Initializations do not care about movability.
    if (not from_type.get()->IsMovable()) {
      diag.Consume(ImmovableType{.from = from_type, .range = range});
      return false;
    }
  }

  if (not type::CanCast(from_type, to_type)) {
    // TODO Really CanCast should be able to log errors.
    diag.Consume(InvalidCast{.from = from_type, .to = to_type, .range = range});
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

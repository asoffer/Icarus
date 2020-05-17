#include "compiler/verify_assignment_and_initialization.h"

#include <type_traits>

#include "diagnostic/errors.h"
#include "type/cast.h"

namespace compiler {

template <bool IsInit>
static bool VerifyImpl(diagnostic::DiagnosticConsumer &diag,
                       frontend::SourceRange const &range, type::QualType to,
                       type::QualType from) {
  if constexpr (not IsInit) {
    // `to` cannot be a constant if we're assigning, but for initializations
    // it's okay (we could be initializing a constant).
    if (to.constant()) {
      diag.Consume(diagnostic::AssigningToConstant{
          .to    = to.type(),
          .range = range,
      });

      return false;
    }
  }

  size_t expansion_size = to.expansion_size();
  if (expansion_size != from.expansion_size()) {
    using DiagnosticType =
        std::conditional_t<IsInit, diagnostic::MismatchedInitializationCount,
                           diagnostic::MismatchedAssignmentCount>;
    diag.Consume(DiagnosticType{
        .to    = to.expansion_size(),
        .from  = from.expansion_size(),
        .range = range,
    });
    return false;
  }

  type::Type const *to_type =
      expansion_size == 1
          ? to.type()
          : type::Tup({to.expanded().begin(), to.expanded().end()});
  type::Type const *from_type =
      expansion_size == 1
          ? from.type()
          : type::Tup({from.expanded().begin(), from.expanded().end()});

  if constexpr (not IsInit) {
    // Initializations do not care about movability.
    if (not from_type->IsMovable()) {
      diag.Consume(diagnostic::ImmovableType{
          .from  = from_type,
          .range = range,
      });
      return false;
    }
  }

  if (not type::CanCast(from_type, to_type)) {
    // TODO Really CanCast should be able to log errors.
    diag.Consume(diagnostic::InvalidCast{
        .from  = from_type,
        .to    = to_type,
        .range = range,
    });
    return false;
  } else {
    return true;
  }
}

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

}  // namespace compiler

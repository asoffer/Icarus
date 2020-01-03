#include "compiler/verify_assignment_and_initialization.h"

#include "type/cast.h"

namespace compiler {

template <bool IsInit>
static bool VerifyImpl(error::Log *error_log, frontend::SourceRange const &span,
                       type::QualType to, type::QualType from) {
  if constexpr (not IsInit) {
    // `to` cannot be a constant if we're assigning, but for initializations
    // it's okay (we could be initializing a constant).
    if (to.constant()) { return false; }
  }

  if (to.expansion_size() != from.expansion_size()) {
    // TODO Log an error
    return false;
  }
  type::Type const *to_type   = to.type();
  type::Type const *from_type = from.type();

  if constexpr (not IsInit) {
    // Initializations do not care about movability.
    if (not from_type->IsMovable()) {
      error_log->NotMovable(span, from_type->to_string());
      return false;
    }
  }

  if (not type::CanCast(from_type, to_type)) {
    // Really CanCast should be able to log errors.
    // TODO Log an error
    return false;
  } else {
    return true;
  }
}

bool VerifyInitialization(error::Log *error_log,
                          frontend::SourceRange const &span, type::QualType to,
                          type::QualType from) {
  return VerifyImpl<true>(error_log, span, to, from);
}

bool VerifyAssignment(error::Log * error_log, frontend::SourceRange const &span,
                      type::QualType to, type::QualType from) {
  return VerifyImpl<false>(error_log, span, to, from);
}

}  // namespace compiler

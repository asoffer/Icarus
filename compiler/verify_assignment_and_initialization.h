#ifndef ICARUS_COMPILER_VERIFY_ASSIGNMENT_AND_INITIALIZATION_H
#define ICARUS_COMPILER_VERIFY_ASSIGNMENT_AND_INITIALIZATION_H

#include "diagnostic/consumer/consumer.h"
#include "frontend/source/range.h"
#include "type/qual_type.h"

namespace compiler {

// TODO instead accept a diagnostic consumer, and have a mock we can query to
// ensure the proper errors were logged.
bool VerifyInitialization(diagnostic::DiagnosticConsumer& diag,
                          frontend::SourceRange const& range, type::QualType to,
                          type::QualType from);
bool VerifyAssignment(diagnostic::DiagnosticConsumer& diag,
                      frontend::SourceRange const& range, type::QualType to,
                      type::QualType from);

}  // namespace compiler

#endif  // ICARUS_COMPILER_VERIFY_ASSIGNMENT_AND_INITIALIZATION_H

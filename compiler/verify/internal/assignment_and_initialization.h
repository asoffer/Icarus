#ifndef ICARUS_COMPILER_VERIFY_INTERNAL_ASSIGNMENT_AND_INITIALIZATION_H
#define ICARUS_COMPILER_VERIFY_INTERNAL_ASSIGNMENT_AND_INITIALIZATION_H

#include "diagnostic/consumer/consumer.h"
#include "frontend/source/buffer.h"
#include "frontend/source/view.h"
#include "type/qual_type.h"

namespace compiler::internal {

// TODO instead accept a diagnostic consumer, and have a mock we can query to
// ensure the proper errors were logged.
bool VerifyInitialization(diagnostic::DiagnosticConsumer& diag,
                          frontend::SourceView const& view, type::QualType to,
                          type::QualType from);
bool VerifyAssignment(diagnostic::DiagnosticConsumer& diag,
                      frontend::SourceView const& view, type::QualType to,
                      type::QualType from);

}  // namespace compiler::internal

#endif  // ICARUS_COMPILER_VERIFY_INTERNAL_ASSIGNMENT_AND_INITIALIZATION_H

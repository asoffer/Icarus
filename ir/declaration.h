#ifndef ICARUS_IR_DECLARATION_H
#define ICARUS_IR_DECLARATION_H

#include "diagnostics/consumer/consumer.h"
#include "parse/tree.h"

namespace ic {

// Modifies each identifier node in the parse tree to have a corresponding
// declaration. Returns `false` if any diagnostics were emitted and `true`
// otherwise.
bool AssignDeclarationsToIdentifiers(ParseTree& tree,
                                     diag::DiagnosticConsumer& diag);

}  // namespace ic

#endif  // ICARUS_IR_DECLARATION_H

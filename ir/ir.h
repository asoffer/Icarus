#ifndef ICARUS_IR_IR_H
#define ICARUS_IR_IR_H

#include "diagnostics/consumer/consumer.h"
#include "ir/emit.h"

namespace ic {

void ProcessIr(EmitContext& emit_context, diag::DiagnosticConsumer& diag);

}  // namespace ic

#endif  // ICARUS_IR_IR_H

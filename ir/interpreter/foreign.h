#ifndef ICARUS_IR_INTERPRETER_FOREIGN_H
#define ICARUS_IR_INTERPRETER_FOREIGN_H

#include <string_view>

#include "absl/status/statusor.h"
#include "absl/types/span.h"
#include "base/untyped_buffer.h"
#include "base/untyped_buffer_view.h"
#include "ir/value/addr.h"
#include "ir/value/foreign_fn.h"

namespace interpreter {

absl::StatusOr<void *> LoadDataSymbol(std::string_view name);
absl::StatusOr<void (*)()> LoadFunctionSymbol(std::string_view name);

// TODO arguments can be a view now.
void CallFn(ir::ForeignFn f, base::untyped_buffer const &arguments,
            absl::Span<ir::Addr const> return_slots,
            base::untyped_buffer_view stack);

}  // namespace interpreter

#endif  // ICARUS_IR_INTERPRETER_FOREIGN_H

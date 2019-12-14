#ifndef ICARUS_INTERPRETTER_FOREIGN_H
#define ICARUS_INTERPRETTER_FOREIGN_H

#include <string_view>

#include "absl/types/span.h"
#include "base/untyped_buffer.h"
#include "ir/addr.h"
#include "ir/foreign_fn.h"

namespace interpretter {

void *LoadDataSymbol(std::string_view name);
void (*LoadFunctionSymbol(std::string_view name))();

void CallForeignFn(ir::ForeignFn f, base::untyped_buffer const &arguments,
                   absl::Span<ir::Addr const> return_slots,
                   base::untyped_buffer *stack);

}  // namespace interpretter

#endif  // ICARUS_INTERPRETTER_FOREIGN_H

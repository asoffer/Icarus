#ifndef ICARUS_IR_EMIT_COMMON_H
#define ICARUS_IR_EMIT_COMMON_H

#include <optional>

#include "absl/types/span.h"
#include "ast/ast.h"
#include "compiler/compiler.h"
#include "ir/compiled_fn.h"
#include "type/struct.h"

namespace compiler {

// Returns A function which can be executed to complete the incomplete struct
// type pointed to by `s`.
std::optional<ir::CompiledFn> StructCompletionFn(
    Compiler &c, type::Struct *s, absl::Span<ast::Declaration const> fields);

}  // namespace compiler

#endif  // ICARUS_IR_EMIT_COMMON_H

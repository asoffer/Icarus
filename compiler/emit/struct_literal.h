#ifndef ICARUS_COMPILER_STRUCT_LITERAL_H
#define ICARUS_COMPILER_STRUCT_LITERAL_H

#include <optional>

#include "absl/types/span.h"
#include "ast/declaration.h"
#include "compiler/compilation_data.h"
#include "ir/compiled_fn.h"
#include "type/struct.h"

namespace compiler {

// Returns A function which can be executed to complete the data-complete struct
// type pointed to by `s`.
std::optional<ir::CompiledFn> StructCompletionFn(
    CompilationDataReference data, type::Struct *s,
    absl::Span<ast::Declaration const> field_decls);

}  // namespace compiler

#endif  // ICARUS_COMPILER_STRUCT_LITERAL_H

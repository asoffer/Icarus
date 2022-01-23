#include "absl/types/span.h"
#include "ast/declaration.h"
#include "compiler/compilation_data.h"
#include "type/struct.h"

namespace compiler {

void EmitStructDataCompletion(CompilationDataReference c,
                              ir::RegOr<type::Type> s,
                              absl::Span<ast::Declaration const> field_decls);

}  // namespace compiler

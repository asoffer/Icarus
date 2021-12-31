#include "ast/node.h"
#include "frontend/source/buffer.h"
#include "frontend/source/view.h"
#include "module/module.h"

namespace compiler {

module::BasicModule *ModuleFor(ast::Node const *node);
frontend::SourceBuffer const *SourceBufferFor(ast::Node const *node);
frontend::SourceView SourceViewFor(ast::Node const *node);

}  // namespace compiler

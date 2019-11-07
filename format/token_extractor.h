#ifndef ICARUS_FORMAT_TOKEN_EXTRACTOR_H
#define ICARUS_FORMAT_TOKEN_EXTRACTOR_H

#include <iostream>
#include "ast/ast_fwd.h"
#include "ast/visitor.h"
#include "base/debug.h"

namespace format {
struct LineBuilder {
  void write(std::string_view token) { std::cerr << token; }
};

struct TokenExtractor : ast::Visitor<void()> {
  void Visit(ast::Node const *node) { ast::Visitor<void()>::Visit(node); }

#define ICARUS_AST_NODE_X(name) void Visit(ast::name const *node);
#include "ast/node.xmacro.h"
#undef ICARUS_AST_NODE_X

  LineBuilder line_builder_;
};

}  // namespace format

#endif  // ICARUS_FORMAT_TOKEN_EXTRACTOR_H

#ifndef ICARUS_FORMAT_TOKEN_EXTRACTOR_H
#define ICARUS_FORMAT_TOKEN_EXTRACTOR_H

#include <iostream>
#include "ast/ast_fwd.h"
#include "base/debug.h"

namespace format {
struct LineBuilder {
  void write(std::string_view token) { std::cerr << token; }
};

struct TokenExtractor {
  void operator()(ast::Node const *node) { UNREACHABLE(); }
#define ICARUS_AST_NODE_X(name) void operator()(ast::name const *node);
#include "ast/node.xmacro.h"
#undef ICARUS_AST_NODE_X

  LineBuilder line_builder_;
};

}  // namespace format

#endif  // ICARUS_FORMAT_TOKEN_EXTRACTOR_H

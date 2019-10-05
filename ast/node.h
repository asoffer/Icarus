#ifndef ICARUS_AST_NODE_H
#define ICARUS_AST_NODE_H

#include <utility>

#include "base/util.h"
#include "frontend/source/range.h"

#include ICARUS_AST_VISITOR_DEPENDENCIES

namespace core {
struct Scope;
}  // namespace core

namespace ast {
struct Node : public base::Cast<Node> {
  Node(frontend::SourceRange span = frontend::SourceRange())
      : span(std::move(span)) {}
  virtual ~Node() {}

#define ICARUS_AST_VISITOR(signature, body) virtual signature body
#include ICARUS_AST_VISITOR_METHODS
#undef ICARUS_AST_VISITOR

  core::Scope *scope_ = nullptr;
  frontend::SourceRange span;
};

}  // namespace ast

#define ICARUS_AST_VISITOR(signature, body) signature override body

#endif  // ICARUS_AST_NODE_H

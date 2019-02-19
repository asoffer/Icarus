#ifndef ICARUS_FRONTEND_TAGGED_NODE_H
#define ICARUS_FRONTEND_TAGGED_NODE_H

#include <memory>

#include "frontend/tag.h"

struct TextSpan;

namespace frontend {

struct TaggedNode {
  std::unique_ptr<ast::Node> node_;
  Tag tag_ = bof;

  TaggedNode() = default;
  TaggedNode(std::unique_ptr<ast::Node> node, Tag tag)
      : node_(std::move(node)), tag_(tag) {}

  static TaggedNode TerminalExpression(const TextSpan &span, ir::Val val);
  TaggedNode(const TextSpan &span, const std::string &token, Tag tag);

  bool valid() const { return node_ != nullptr; }
  static TaggedNode Invalid() { return TaggedNode{}; }
};
}  // namespace frontend

#endif  // ICARUS_FRONTEND_TAGGED_NODE_H

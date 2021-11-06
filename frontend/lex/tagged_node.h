#ifndef ICARUS_FRONTEND_LEX_TAGGED_NODE_H
#define ICARUS_FRONTEND_LEX_TAGGED_NODE_H

#include <memory>
#include <sstream>
#include <utility>

#include "base/meta.h"
#include "frontend/lex/lexeme.h"
#include "frontend/lex/tag.h"
#include "frontend/lex/token.h"

namespace frontend {
struct SourceRange;

struct TaggedNode {
  std::unique_ptr<ast::Node> node_;
  Tag tag_{};

  TaggedNode() = default;

  /* implicit */ TaggedNode(Lexeme &&l) : tag_(l.tag()) {
    auto range = l.range();
    std::visit(
        [&](auto &&x) {
          constexpr auto type = base::meta<std::decay_t<decltype(x)>>;
          if constexpr (type == base::meta<Syntax>) {
            std::stringstream ss;
            ss << x;
            node_ = std::make_unique<Token>(range, ss.str(), false);
          } else if constexpr (type == base::meta<Operator>) {
            std::stringstream ss;
            ss << x;
            node_ = std::make_unique<Token>(range, ss.str(), false);
          } else if constexpr (type == base::meta<std::unique_ptr<ast::Node>>) {
            node_ = std::move(x);
          } else {
            node_ = std::make_unique<Token>(
                range, std::string(ir::ToStringView(x)), true);
          }
        },
        std::move(l).get());
  }

  TaggedNode(std::unique_ptr<ast::Node> node, Tag tag)
      : node_(std::move(node)), tag_(tag) {}

  TaggedNode(const SourceRange &range, const std::string &token, Tag tag);

  bool valid() const { return node_ != nullptr; }
  static TaggedNode Invalid() { return TaggedNode{}; }
};
}  // namespace frontend

#endif  // ICARUS_FRONTEND_LEX_TAGGED_NODE_H

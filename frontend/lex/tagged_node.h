#ifndef ICARUS_FRONTEND_LEX_TAGGED_NODE_H
#define ICARUS_FRONTEND_LEX_TAGGED_NODE_H

#include <memory>

#include "frontend/lex/lexeme.h"
#include "frontend/lex/tag.h"
#include "frontend/lex/token.h"

namespace frontend {
struct SourceRange;

struct TaggedNode {
  std::unique_ptr<ast::Node> node_;
  Tag tag_ = bof;

  TaggedNode() = default;

  /* implicit */ TaggedNode(Lexeme &&l) : tag_(l.tag()) {
    auto range = l.range();
    std::visit(
        [&](auto &&x) {
          using T = std::decay_t<decltype(x)>;
          if constexpr (std::is_same_v<T, Syntax>) {
            node_ = std::make_unique<Token>(range, stringify(x), false);
          } else if constexpr (std::is_same_v<T, Operator>) {
            node_ = std::make_unique<Token>(range, stringify(x), false);
          } else if constexpr (std::is_same_v<T, std::unique_ptr<ast::Node>>) {
            node_ = std::move(x);
          } else {
            std::string token;
            switch (x.kind()) {
              case ast::Hashtag::Builtin::Export: token = "{export}"; break;
              case ast::Hashtag::Builtin::NoDefault:
                token = "{no_default}";
                break;
              case ast::Hashtag::Builtin::Uncopyable:
                token = "{uncopyable}";
                break;
              case ast::Hashtag::Builtin::Immovable:
                token = "{immovable}";
                break;
              case ast::Hashtag::Builtin::Inline: token = "{inline}"; break;
              case ast::Hashtag::Builtin::User: token = x.user_tag(); break;
            }
            node_ = std::make_unique<Token>(range, token, true);
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

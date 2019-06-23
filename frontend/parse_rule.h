#ifndef ICARUS_FRONTEND_PARSE_RULE_H
#define ICARUS_FRONTEND_PARSE_RULE_H

#include <memory>

#include "absl/types/span.h"
#include "ast/node.h"
#include "frontend/tag.h"

struct Module;

namespace error {
struct Log;
}  // namespace error

namespace frontend {
struct ParseRule {
 public:
  explicit ParseRule(
      Tag output, std::vector<uint64_t> input,
      std::unique_ptr<ast::Node> (*fn)(absl::Span<std::unique_ptr<ast::Node>>,
                                       Module *, error::Log *))
      : output_(output), input_(std::move(input)), fn_(fn) {}

  size_t size() const { return input_.size(); }

  bool Match(absl::Span<Tag const> tag_stack) const;
  void Apply(std::vector<std::unique_ptr<ast::Node>> *node_stack,
             std::vector<Tag> *tag_stack, Module *mod,
             error::Log *error_log) const;

 private:
  Tag output_;
  std::vector<uint64_t> input_;
  std::unique_ptr<ast::Node> (*fn_)(absl::Span<std::unique_ptr<ast::Node>>,
                                    Module *, error::Log *);
};

}  // namespace frontend

#endif  // ICARUS_FRONTEND_PARSE_RULE_H

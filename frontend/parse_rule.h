#ifndef ICARUS_FRONTEND_PARSE_RULE_H
#define ICARUS_FRONTEND_PARSE_RULE_H

#include <memory>
#include <utility>
#include <vector>

#include "absl/types/span.h"
#include "ast/node.h"
#include "base/debug.h"
#include "base/log.h"
#include "diagnostic/consumer/consumer.h"
#include "frontend/lex/tag.h"

namespace frontend {

template <size_t N>
struct MatchSequence {
  template <typename... Ts>
  constexpr MatchSequence(Ts... ts)
      : MatchSequence(absl::Span<uint64_t const>{ts...}) {}

  size_t constexpr size() const { return size_; }

  bool operator()(absl::Span<Tag const> tag_stack) const {
    // The stack needs to be long enough to match.
    if (size() > tag_stack.size()) { return false; }

    auto stack_iter = tag_stack.rbegin();
    auto rule_iter  = matches_.rbegin();
    auto end_iter   = matches_.rbegin() + size();
    while (rule_iter != end_iter) {
      if ((*rule_iter & *stack_iter) == 0) { return false; }
      ++rule_iter;
      ++stack_iter;
    }

    return true;
  }

 private:
  constexpr explicit MatchSequence(absl::Span<uint64_t const> tags)
      : size_(tags.size()) {
    ASSERT(tags.size() <= N);
    std::copy(tags.begin(), tags.end(), matches_.begin() + N - tags.size());
  }

  std::array<uint64_t, N> matches_{};
  size_t size_;
};

template <size_t N>
struct Rule {
  MatchSequence<N> match;
  Tag output;
  std::unique_ptr<ast::Node> (*execute)(absl::Span<std::unique_ptr<ast::Node>>,
                                        diagnostic::DiagnosticConsumer &diag);
};

}  // namespace frontend

#endif  // ICARUS_FRONTEND_PARSE_RULE_H

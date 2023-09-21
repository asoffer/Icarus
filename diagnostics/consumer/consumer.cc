#include "diagnostics/consumer/consumer.h"

#include "nth/debug/debug.h"

namespace ic::diag {

ParseTree const &DiagnosticConsumer::parse_tree() const {
  NTH_REQUIRE(tree_ != nullptr);
  return *tree_;
}

void DiagnosticConsumer::set_source(std::string_view source) {
  source_ = source;
  for (size_t i = 0; i < source_.size(); i = source_.find('\n', i + 1)) {
    offsets_.push_back(i);
  }
  offsets_.push_back(source_.size());
}

void DiagnosticConsumer::Consume(Token location, Message const &message) {
  for (auto const &component : message.components()) {
    Start(component);
    Process(component);
    Complete(component);
  }
  ++count_;
}

std::pair<uint32_t, uint32_t> DiagnosticConsumer::LineAndColumn(
    Token token) const {
  auto iter =
      std::lower_bound(offsets_.begin(), offsets_.end(), token.offset());
  NTH_REQUIRE(iter != offsets_.end());
  if (*iter != token.offset()) { --iter; }
  return std::pair<uint32_t, uint32_t>(
      std::distance(offsets_.begin(), iter) + 1, token.offset() - *iter);
}

std::string_view DiagnosticConsumer::Line(uint32_t line) const {
  NTH_REQUIRE(source_ != "");
  NTH_REQUIRE(line > 0);
  NTH_REQUIRE(line < offsets_.size());
  return std::string_view(source_.data() + offsets_[line - 1],
                          source_.data() + offsets_[line]);
}

}  // namespace ic::diag

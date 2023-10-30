#ifndef ICARUS_DIAGNOSTICS_CONSUMER_CONSUMER_H
#define ICARUS_DIAGNOSTICS_CONSUMER_CONSUMER_H

#include <string_view>
#include <utility>
#include <vector>

#include "diagnostics/message.h"
#include "lexer/token.h"
#include "nth/base/attributes.h"
#include "parse/parse_tree.h"

namespace ic::diag {

struct DiagnosticConsumer {
  void set_source(std::string_view source);
  void set_parse_tree(NTH_ATTRIBUTE(lifetimebound) ParseTree const &tree) {
    tree_ = &tree;
  }

  void Consume(Message const &message) { Consume(Token::Invalid(), message); }
  void Consume(Token location, Message const &message);

  ParseTree const &parse_tree() const;

  std::string_view Symbol(Token token) const;

  std::pair<uint32_t, uint32_t> LineAndColumn(Token token) const;

  std::string_view Line(uint32_t line) const;

  size_t lines() const { return offsets_.size() - 1; }
  size_t count() const { return count_; }

  virtual ~DiagnosticConsumer()                            = default;
  virtual void Start(MessageComponent const &component)    = 0;
  virtual void Process(MessageComponent const &component)  = 0;
  virtual void Complete(MessageComponent const &component) = 0;

 private:
  size_t count_ = 0;
  ParseTree const *tree_   = nullptr;
  std::string_view source_ = "";
  std::vector<uint32_t> offsets_;
};

}  // namespace ic::diag

#endif  // ICARUS_DIAGNOSTICS_CONSUMER_CONSUMER_H

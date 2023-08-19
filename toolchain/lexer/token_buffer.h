#ifndef ICARUS_TOOLCHAIN_LEXER_TOKEN_BUFFER_H
#define ICARUS_TOOLCHAIN_LEXER_TOKEN_BUFFER_H

#include <string_view>
#include <vector>

#include "nth/container/flyweight_set.h"
#include "toolchain/lexer/token.h"

namespace ic {

struct DiagnosticConsumer {
  void Consume() {}
};

struct TokenBuffer {
  using const_iterator = std::vector<Token>::const_iterator;

  void Append(Token token) { tokens_.push_back(token); }

  void AppendInteger(std::string_view integer, uint32_t offset);
  void AppendKeywordOrIdentifier(std::string_view identifier, uint32_t offset);

  Token operator[](size_t index) const { return tokens_[index]; }
  size_t size() const { return tokens_.size(); }

  auto begin() const { return tokens_.begin(); }
  auto end() const { return tokens_.end(); }

 private:
  friend TokenBuffer Lex(std::string_view source,
                         DiagnosticConsumer& diagnostic_consumer);

  nth::flyweight_set<std::string_view> identifiers_;
  nth::flyweight_set<uint64_t> integers_;
  std::vector<Token> tokens_;
};

}  // namespace ic

#endif  // ICARUS_TOOLCHAIN_LEXER_TOKEN_BUFFER_H

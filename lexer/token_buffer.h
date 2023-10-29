#ifndef ICARUS_LEXER_TOKEN_BUFFER_H
#define ICARUS_LEXER_TOKEN_BUFFER_H

#include <string_view>
#include <vector>

#include "diagnostics/consumer/consumer.h"
#include "lexer/token.h"
#include "nth/container/flyweight_set.h"
#include "nth/io/string_printer.h"

namespace ic {

struct TokenBuffer {
  using const_iterator = std::vector<Token>::const_iterator;

  void Append(Token token) { tokens_.push_back(token); }
  void AppendClose(Token::Kind kind, uint32_t open_index, uint32_t offset);

  void AppendIntegerLiteral(std::string_view integer, uint32_t offset);
  void AppendStringLiteral(std::string s, uint32_t offset);
  void AppendKeywordOrIdentifier(std::string_view identifier, uint32_t offset);

  Token operator[](size_t index) const { return tokens_[index]; }
  size_t size() const { return tokens_.size(); }

  auto begin() const { return tokens_.begin(); }
  auto end() const { return tokens_.end(); }

  friend void NthPrint(auto& printer, TokenBuffer const& token_buffer);

 private:
  friend TokenBuffer Lex(std::string_view source,
                         diag::DiagnosticConsumer& diagnostic_consumer);

  std::vector<Token> tokens_;
};

void NthPrint(auto& printer, TokenBuffer const& token_buffer) {
  nth::universal_formatter f({
      .depth    = 5,
      .fallback = "...",
  });

  for (auto const& token : token_buffer.tokens_) {
    nth::Interpolate<"\n  {}">(printer, f, token);
  }
}

}  // namespace ic

#endif  // ICARUS_LEXER_TOKEN_BUFFER_H

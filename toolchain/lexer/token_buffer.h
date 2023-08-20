#ifndef ICARUS_TOOLCHAIN_LEXER_TOKEN_BUFFER_H
#define ICARUS_TOOLCHAIN_LEXER_TOKEN_BUFFER_H

#include <string_view>
#include <vector>

#include "nth/container/flyweight_set.h"
#include "nth/io/string_printer.h"
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

  uint32_t IdentifierIndex(std::string_view identifier);

  Token operator[](size_t index) const { return tokens_[index]; }
  size_t size() const { return tokens_.size(); }

  auto begin() const { return tokens_.begin(); }
  auto end() const { return tokens_.end(); }

  friend void NthPrint(auto& printer, TokenBuffer const& token_buffer);

 private:
  friend TokenBuffer Lex(std::string_view source,
                         DiagnosticConsumer& diagnostic_consumer);

  nth::flyweight_set<std::string_view> identifiers_;
  nth::flyweight_set<uint64_t> integers_;
  std::vector<Token> tokens_;
};

void NthPrint(auto& printer, TokenBuffer const& token_buffer) {
  std::vector<std::string> strs;
  strs.reserve(token_buffer.tokens_.size());
  nth::universal_formatter f({
      .depth    = 3,
      .fallback = "...",
  });

  size_t size = 0;
  for (auto const& token : token_buffer.tokens_) {
    nth::string_printer p(strs.emplace_back());
    nth::Interpolate<" {} ">(p, f, token);
    size = std::max(strs.back().size(), size);
  }

  std::string border = "╭";
  for (size_t i = 0; i < size; ++i) { border.append("─"); }
  border.append("╮");

  size_t length      = std::strlen("╮");
  char* left_border  = border.data();
  char* right_border = border.data() + border.size() - length;
  for (std::string_view s : strs) {
    nth::Interpolate<"{}\n">(printer, f, border);
    std::memcpy(left_border, "├", length);
    std::memcpy(right_border, "┤", length);
    nth::Interpolate<"│{}{}│\n">(printer, f, s,
                                 std::string(size - s.size(), ' '));
  }
  std::memcpy(left_border, "╰", length);
  std::memcpy(right_border, "╯", length);
  nth::Interpolate<"{}\n">(printer, f, border);
}

}  // namespace ic

#endif  // ICARUS_TOOLCHAIN_LEXER_TOKEN_BUFFER_H

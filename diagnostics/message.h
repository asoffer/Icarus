#ifndef ICARUS_DIAGNOSTICS_MESSAGE_H
#define ICARUS_DIAGNOSTICS_MESSAGE_H

#include <initializer_list>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "lexer/token.h"

namespace ic::diag {

enum class MessageKind { Error };

struct Header {
  explicit Header(MessageKind kind) : kind_(kind) {}

 private:
  MessageKind kind_;
};

struct Text {
  explicit Text(std::string s) : text_(std::move(s)) {}

  std::string_view text() const { return text_; }

 private:
  std::string text_;
};

struct SourceQuote {
  explicit SourceQuote(Token token) : token_(token) {}

  Token token() const { return token_; };

 private:
  Token token_;
};


struct MessageComponent {
  template <typename T>
  MessageComponent(T t) : component_(std::move(t)) {}

  template <typename T>
  T const *As() const {
    return std::get_if<T>(&component_);
  }

 private:
  std::variant<Header, Text, SourceQuote> component_;
};

struct Message {
  Message(std::initializer_list<MessageComponent> components)
      : components_(components.begin(), components.end()) {}
  explicit Message(std::vector<MessageComponent> components)
      : components_(std::move(components)) {}

  std::span<MessageComponent const> components() const { return components_; }

 private:
  std::vector<MessageComponent> components_;
};

}  // namespace ic::diag

#endif  // ICARUS_DIAGNOSTICS_MESSAGE_H

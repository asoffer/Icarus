#ifndef ICARUS_DIAGNOSTICS_MESSAGE_H
#define ICARUS_DIAGNOSTICS_MESSAGE_H

#include <initializer_list>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

namespace ic::diag {

enum class MessageKind { Error };

struct Header {
  explicit Header(MessageKind kind, size_t offset = -1) : kind_(kind), offset_(offset) {}

  size_t offset() const { return offset_; }

 private:
  MessageKind kind_;
  size_t offset_;
};

struct Text {
  explicit Text(std::string s) : text_(std::move(s)) {}

  std::string_view text() const { return text_; }

 private:
  std::string text_;
};

struct MessageComponent {
  template <typename T>
  MessageComponent(T t) : component_(std::move(t)) {}

  template <typename T>
  T const *As() const {
    return std::get_if<T>(&component_);
  }

 private:
  std::variant<Header, Text> component_;
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

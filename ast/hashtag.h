#ifndef ICARUS_AST_HASHTAG_H
#define ICARUS_AST_HASHTAG_H

#include <string>
#include <variant>

#include "base/debug.h"

namespace ast {
// TODO move out of ast into ir/value.
struct Hashtag {
  enum class Builtin {
    Export,
    NoDefault,
    Uncopyable,
    Immovable,
    Inline,
    User,
  };

  Builtin kind() const { return kind_; }
  std::string_view user_tag() const {
    ASSERT(kind_ == Builtin::User);
    return user_;
  }

  Hashtag() = delete;
  explicit Hashtag(Builtin b) : kind_(b) {}
  explicit Hashtag(std::string tag)
      : kind_(Builtin::User), user_(std::move(tag)) {}

  friend bool operator==(Hashtag const &lhs, Hashtag const &rhs) {
    return (lhs.kind_ == rhs.kind_) and
           (lhs.kind_ != Builtin::User or lhs.user_ == rhs.user_);
  }
  friend bool operator!=(Hashtag const &lhs, Hashtag const &rhs) {
    return not(lhs == rhs);
  }

 private:
  Builtin kind_;
  std::string user_;  // Only meaningful when `kind_ == Builtin::User`.
};
}  // namespace ast

#endif  // ICARUS_AST_HASHTAG_H

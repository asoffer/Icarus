#ifndef ICARUS_AST_HASHTAG_H
#define ICARUS_AST_HASHTAG_H

namespace ast {
struct Hashtag {
  enum class Builtin { Export, NoDefault } kind_;

  Hashtag() = delete;
  Hashtag(Builtin b) : kind_(b) {}

  Hashtag(Hashtag&&) noexcept      = default;
  Hashtag(Hashtag const&) noexcept = default;
  Hashtag& operator=(Hashtag&&) noexcept = default;
  Hashtag& operator=(Hashtag const&) noexcept = default;

  // In the future we will probably also store a string for user-defined
  // hashtags.
};
}  // namespace ast

#endif // ICARUS_AST_HASHTAG_H

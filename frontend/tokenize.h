#ifndef ICARUS_FRONTEND_TOKENIZE_H
#define ICARUS_FRONTEND_TOKENIZE_H

#include <memory>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

#include "base/stringify.h"
#include "frontend/operators.h"
#include "frontend/tag.h"

namespace ast {
struct Node;
}  // namespace ast

namespace frontend {
struct Src;

struct TaggedToken {
  TaggedToken(Tag tag, std::string_view token)
      : tag(tag), token(std::string{token}) {}
  template <typename Tk>
  TaggedToken(Tag tag, Tk&& token) : tag(tag), token(std::forward<Tk>(token)) {}
  Tag tag;
  std::variant<std::string, int64_t, double> token;
};

inline std::string stringify(TaggedToken t) {
  using base::stringify;
  return "tagged-token(" + stringify(t.tag) + ", " + stringify(t.token) + ")";
}

inline bool operator==(TaggedToken lhs, TaggedToken rhs) {
  return lhs.tag == rhs.tag && lhs.token == rhs.token;
}
inline bool operator!=(TaggedToken lhs, TaggedToken rhs) {
  return !(lhs == rhs);
}

// Reads in a line from Src and pushes tokens onto out. Returns false if we have
// reached the end of the file.
bool TokenizeLine(Src* line, std::vector<TaggedToken>* out);

}  // namespace frontend

#endif  // ICARUS_FRONTEND_TOKENIZE_H

#ifndef ICARUS_LEXER_LEX_IMPL_H
#define ICARUS_LEXER_LEX_IMPL_H

#include <concepts>
#include <string_view>

namespace ic::lex {

template <auto F>
requires(std::is_invocable_r_v<bool, decltype(F), char>)  //
    std::string_view ConsumeWhile(std::string_view& source) {
  char const* end   = source.data() + source.size();
  char const* start = source.data();
  char const* p     = start;
  for (; p != end and F(*p); ++p) {}
  size_t count            = std::distance(start, p);
  std::string_view result = source.substr(0, count);
  source.remove_prefix(count);
  return result;
}

}  // namespace ic::lex

#endif  // ICARUS_LEXER_LEX_IMPL_H

#ifndef ICARUS_FRONTEND_PARSER_DSL_H
#define ICARUS_FRONTEND_PARSER_DSL_H

#include <concepts>
#include <type_traits>

#include "absl/types/span.h"
#include "base/meta.h"
#include "frontend/lexeme.h"

namespace frontend {

template <typename P>
concept Parser = requires(P p) {
  // clang-format off
  { base::meta<typename P::type> };
  { P::Parse(std::declval<absl::Span<Lexeme const>&>(),
             std::ignore) } -> std::same_as<bool>;
  { P::Parse(std::declval<absl::Span<Lexeme const>&>(),
             std::declval<typename P::type&>()) } -> std::same_as<bool>;
  // clang-format on
};

namespace internal_parser_dsl {
#if defined(ICARUS_DEBUG)
inline int log_indentation = 0;
bool LoggingEnabled() {
  static bool b = true or base::LoggingEnabled("parser");
  return b;
}

constexpr std::string_view Prettify(std::string_view sv,
                                    std::string_view func) {
  auto end = sv.find("::Parse(");
  if (end == std::string_view::npos) { return func; }
  auto start = sv.find("(anonymous namespace)::") + 23;
  if (start > sv.size()) { start = sv.find("bool ") + 5; }
  if (start > sv.size()) { start = 0; }
  return sv.substr(start, end - start);
}

#define PARSE_DEBUG_LOG()                                                      \
  if (::frontend::internal_parser_dsl::LoggingEnabled()) {                     \
    absl::Format(                                                              \
        &std::cerr, "%s%s\n",                                                  \
        std::string(2 * (::frontend::internal_parser_dsl::log_indentation++),  \
                    ' '),                                                      \
        ::frontend::internal_parser_dsl::Prettify(__PRETTY_FUNCTION__,         \
                                                  __func__));                  \
  }                                                                            \
  absl::Cleanup c = [&, f = ::frontend::internal_parser_dsl::Prettify(         \
                            __PRETTY_FUNCTION__, __func__)] {                  \
    if (::frontend::internal_parser_dsl::LoggingEnabled()) {                   \
      absl::Format(                                                            \
          &std::cerr, "%sFinished %s\n",                                       \
          std::string(2 * --::frontend::internal_parser_dsl::log_indentation,  \
                      ' '),                                                    \
          f);                                                                  \
    }                                                                          \
  }
#else
#define PARSE_DEBUG_LOG() (void)0
#endif

template <Parser L, Parser R>
struct DisjunctionImpl {
  using type = typename L::type;
  static bool Parse(absl::Span<Lexeme const> &lexemes, auto &&out) {
    return L::Parse(lexemes, out) or R::Parse(lexemes, out);
  }
};

}  // namespace internal_parser_dsl

// Given two parsers `L` and `R` for the same type `T`, returns a parser for `T`
// which attempts to match `T` first by using `L`, and if that fails, then by
// using `R`.
template <Parser L, Parser R>
constexpr auto operator|(L, R) requires(base::meta<typename L::type> ==
                                        base::meta<typename R::type>) {
  return internal_parser_dsl::DisjunctionImpl<L, R>{};
}

// A parser that matches any one lexeme whose kind is `K`.
template <Lexeme::Kind K>
struct Kind {
  using type = std::string_view;
  static bool Parse(absl::Span<Lexeme const> &lexemes, auto &&out) {
    PARSE_DEBUG_LOG();
    if (lexemes.empty() or lexemes[0].kind() != K) { return false; }
    out = lexemes[0].content();
    lexemes.remove_prefix(1);
    return true;
  }
};

// A parser that attempts to parse `P`, binding if `P` parses successfully, but
// otherwise consumes no lexemes and is considered to have parsed successfully.
template <Parser P>
struct Optional {
  using type = typename P::type;

  explicit constexpr Optional(P) {}

  static bool Parse(absl::Span<Lexeme const> &lexemes, auto &&out) {
    P::Parse(lexemes, out);
    return true;
  }
};

}  // namespace frontend

#endif  // ICARUS_FRONTEND_PARSER_DSL_H

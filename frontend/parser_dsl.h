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

template <Lexeme::Kind Separator, Parser P,
          template <typename...> typename Container = std::vector>
struct SeparatedListImpl {
  using type = Container<typename P::type>;
  static bool Parse(absl::Span<Lexeme const> &lexemes, auto &&out) {
    type result;
    auto span        = lexemes;
    using value_type = typename type::value_type;
    value_type v;
    if (not P::Parse(span, v)) {
      out = std::move(result);
      return true;
    }
    result.push_back(std::move(v));
    while (true) {
      if (span.empty() or span[0].kind() != Separator) {
        lexemes = span;
        out     = std::move(result);
        return true;
      }
      span.remove_prefix(1);
      if (P::Parse(span, v)) {
        result.emplace_back(std::move(v));
      } else {
        return false;
      }
    }
  }
};

template <char C, typename P>
struct DelimitedByImpl {
  using type = typename P::type;
  static bool Parse(absl::Span<Lexeme const> &lexemes, auto &&out) {
    auto range = CheckBounds(lexemes);
    if (not range.data()) { return false; }
    bool result = P::Parse(range, out) and range.empty();
    if (result) { lexemes.remove_prefix(lexemes.front().match_offset() + 1); }
    return result;
  }

 private:
  static absl::Span<Lexeme const> CheckBounds(
      absl::Span<Lexeme const> &lexemes) {
    if (lexemes.empty() or lexemes.front().content().size() != 1 or
        lexemes.front().content()[0] != C) {
      return absl::Span<Lexeme const>(nullptr, 0);
    } else {
      size_t offset = lexemes.front().match_offset();
      return lexemes.subspan(1, offset - 1);
    }
  }
};

struct IgnoredType {};

template <Parser P>
struct Ignored {
  using type = IgnoredType;

  static bool Parse(absl::Span<Lexeme const> &lexemes, auto &&out) {
    return P::Parse(lexemes, std::ignore);
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

template <Parser P>
constexpr Parser auto operator--(P) {
  return internal_parser_dsl::Ignored<P>();
}

template <char C>
constexpr auto DelimitedBy(auto P) {
  return internal_parser_dsl::DelimitedByImpl<C, decltype(P)>();
}

constexpr auto Bracketed(auto P) { return DelimitedBy<'['>(P); }
constexpr auto Parenthesized(auto P) { return DelimitedBy<'('>(P); }
constexpr auto Braced(auto P) { return DelimitedBy<'{'>(P); }
constexpr auto CommaSeparatedListOf(auto P) {
  return internal_parser_dsl::SeparatedListImpl<Lexeme::Kind::Comma,
                                                decltype(P)>();
}
constexpr auto NewlineSeparatedListOf(auto P) {
  return internal_parser_dsl::SeparatedListImpl<Lexeme::Kind::Newline,
                                                decltype(P)>();
}

}  // namespace frontend

#endif  // ICARUS_FRONTEND_PARSER_DSL_H

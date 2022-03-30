#ifndef ICARUS_FRONTEND_PARSER_DSL_H
#define ICARUS_FRONTEND_PARSER_DSL_H

#include <concepts>
#include <type_traits>

#include "absl/cleanup/cleanup.h"
#include "absl/types/span.h"
#include "base/meta.h"
#include "frontend/lexeme.h"

namespace frontend {

template <typename P>
concept ParserImplementation = std::is_empty_v<P> and requires(P p) {
  typename P::match_type;
  // TODO: Reimplement
};

template <typename P>
concept Parser = ParserImplementation<decltype(P::parser)>;

namespace internal_parser_dsl {
#if defined(ICARUS_DEBUG)
inline int log_indentation = 0;
inline bool LoggingEnabled() {
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

inline std::string_view ExtractRange(absl::Span<Lexeme const> &lexemes,
                                     absl::Span<Lexeme const> remaining) {
  ASSERT(lexemes.size() != 0);
  Lexeme const &last   = *(remaining.data() - 1);
  char const *endpoint = last.content().data() + last.content().size();
  size_t length        = endpoint - lexemes.front().content().data();
  std::string_view result(lexemes.front().content().data(), length);
  lexemes = remaining;
  return result;
}

template <Parser L, Parser R>
struct Disjunction {
  using match_type = typename decltype(L::parser)::match_type;
  static bool Parse(absl::Span<Lexeme const> &lexemes,
                    std::string_view &consumed, auto &&out) {
    absl::Span span = lexemes;
    bool result =
        L::parser.Parse(span, consumed, out) or R::Parse(span, consumed, out);
    if (result) { consumed = ExtractRange(lexemes, span); }
    return result;
  }
};

template <Parser... Ps>
struct Sequenced {
 private:
  struct Impl {
    using match_type =
        base::type_list_cat<typename decltype(Ps::parser)::match_type...>;
    static bool Parse(absl::Span<Lexeme const> &lexemes,
                      std::string_view &consumed, auto &&... outs) {
      auto out_tuple_tuple = base::SplitTuple<base::Length(typename decltype(
          Ps::parser)::match_type{})...>(std::forward_as_tuple(outs...));

      absl::Span span = lexemes;
      std::string_view ignore;
      bool result = std::apply(
          [&](auto &... out_tuples) {
            return (std::apply(
                        [&](auto &... outs) {
                          return Ps::parser.Parse(span, ignore, outs...);
                        },
                        out_tuples) and
                    ...);
          },
          out_tuple_tuple);

      if (result) { consumed = ExtractRange(lexemes, span); }
      return result;
    }
  };

 public:
  static constexpr auto parser = Impl();
};

template <Lexeme::Kind Separator, Parser P, size_t MinLength,
          template <typename...> typename Container = std::vector>
struct SeparatedList {
 private:
  static constexpr size_t kNumMatches =
      base::Length(typename decltype(P::parser)::match_type{});
  using element_type = std::conditional_t<
      kNumMatches == 1, base::head<typename decltype(P::parser)::match_type>,
      base::reduce_t<std::tuple, typename decltype(P::parser)::match_type>>;

  struct Impl {
    using match_type = base::type_list<Container<element_type>>;

    static bool Parse(absl::Span<Lexeme const> &lexemes,
                      std::string_view &consumed, auto &&out) {
      Container<element_type> result;
      auto span = lexemes;
      element_type v;
      if (not P::parser.Parse(span, consumed, v)) { return MinLength == 0; }
      result.push_back(std::move(v));
      while (true) {
        if (result.size() >= MinLength and
            (span.empty() or span[0].kind() != Separator)) {
          consumed = ExtractRange(lexemes, span);
          out      = std::move(result);
          return true;
        }
        span.remove_prefix(1);
        if (P::parser.Parse(span, consumed, v)) {
          result.emplace_back(std::move(v));
        } else {
          return false;
        }
      }
    }
  };

 public:
  static constexpr auto parser = Impl();
};

template <char C, typename P>
struct DelimitedBy {
 private:
  struct Impl {
    using match_type = typename decltype(P::parser)::match_type;
    static bool Parse(absl::Span<Lexeme const> &lexemes,
                      std::string_view &consumed, auto &&... out) {
      auto range = CheckBounds(lexemes);
      if (not range.data()) { return false; }
      bool result = P::parser.Parse(range, consumed, out...) and range.empty();
      if (result) {
        // TODO: Update `consumed`
        lexemes.remove_prefix(lexemes.front().match_offset() + 1);
      }
      return result;
    }

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

 public:
  static constexpr auto parser = Impl();
};

template <typename P, size_t... Ns>
bool CallWithIgnores(std::index_sequence<Ns...>,
                     absl::Span<Lexeme const> &lexemes,
                     std::string_view &consumed) {
  return P::parser.Parse(lexemes, consumed, (std::ignore = Ns)...);
}

template <Parser P>
struct Ignored {
 private:
  struct Impl {
    using match_type = base::type_list<>;

    static bool Parse(absl::Span<Lexeme const> &lexemes,
                      std::string_view &consumed) {
      constexpr size_t N =
          base::Length(typename decltype(P::parser)::match_type{});
      return CallWithIgnores<P>(std::make_index_sequence<N>{}, lexemes,
                                consumed);
    }
  };

 public:
  static constexpr auto parser = Impl();
};

template <size_t N>
struct FixedString {
  constexpr FixedString(char const (&s)[N + 1]) {
    for (size_t i = 0; i < N; ++i) { data[i] = s[i]; }
  }

  constexpr bool operator==(FixedString const &) const = default;
  constexpr bool operator!=(FixedString const &) const = default;

  friend constexpr bool operator==(std::string_view s, FixedString const &f) {
    return s.size() == N and std::memcmp(s.data(), f.data.data(), N) == 0;
  }
  friend constexpr bool operator==(FixedString const &f, std::string_view s) {
    return s.size() == N and std::memcmp(s.data(), f.data.data(), N) == 0;
  }
  friend constexpr bool operator!=(std::string_view s, FixedString const &f) {
    return not(s == f);
  }
  friend constexpr bool operator!=(FixedString const &f, std::string_view s) {
    return not(s == f);
  }

  std::array<char, N> data;
};

template <size_t N>
FixedString(char const (&s)[N]) -> FixedString<N - 1>;

template <typename F>
struct InvokeResultT {
  template <typename... ArgTs>
  struct Get {
    using type = std::invoke_result_t<F, ArgTs...>;
  };
};

template <Parser P, typename F, bool UseConsumedRange>
struct ParserWith {
  using match_type = base::type_list<typename base::reduce_t<
      InvokeResultT<F>::template Get,
      std::conditional_t<
          UseConsumedRange,
          base::type_list_cat<base::type_list<std::string_view>,
                              typename decltype(P::parser)::match_type>,
          typename decltype(P::parser)::match_type>>::type>;

  static bool Parse(absl::Span<Lexeme const> &lexemes,
                    std::string_view &consumed, auto &&out) {
    base::reduce_t<std::tuple, typename decltype(P::parser)::match_type>
        value_tuple;
    bool result = std::apply(
        [&](auto &... values) {
          return P::parser.Parse(lexemes, consumed, values...);
        },
        value_tuple);
    if (not result) { return false; }
    F f;
    if constexpr (UseConsumedRange) {
      out = std::apply(std::bind_front(f, consumed), std::move(value_tuple));
    } else {
      out = std::apply(f, std::move(value_tuple));
    }
    return true;
  }
};

template <typename F, bool B>
struct BindImpl {
  template <Parser P>
  friend constexpr auto operator<<(P, BindImpl) {
    return ParserWith<P, F, B>();
  }
};

}  // namespace internal_parser_dsl

// Given two parsers `L` and `R` for the same type `T`, returns a parser for `T`
// which attempts to match `T` first by using `L`, and if that fails, then by
// using `R`.
template <Parser L, Parser R>
constexpr auto operator|(L, R) requires(
    base::meta<typename decltype(L::parser)::match_type> ==
    base::meta<typename decltype(R::parser)::match_type>) {
  return internal_parser_dsl::Disjunction<L, R>();
}

// Given two parsers `L` and `R` returns a parser matching those lexeme streams
// which matching `L` and then `R`.`
template <Parser L, Parser R>
constexpr auto operator+(L, R) {
  return internal_parser_dsl::Sequenced<L, R>();
}

// A parser that matches any one lexeme whose kind is `K`.
template <Lexeme::Kind K>
struct Kind {
 private:
  struct Impl {
    using match_type = base::type_list<std::string_view>;
    static bool Parse(absl::Span<Lexeme const> &lexemes,
                      std::string_view &consumed, auto &&out) {
      PARSE_DEBUG_LOG();
      if (lexemes.empty() or lexemes[0].kind() != K) { return false; }
      out      = lexemes[0].content();
      consumed = internal_parser_dsl::ExtractRange(lexemes, lexemes.subspan(1));
      return true;
    }
  };

 public:
  static constexpr auto parser = Impl();
};

// A parser that matches a lexeme exactly.
template <internal_parser_dsl::FixedString S>
struct Match {
 private:
  struct Impl {
    using match_type = base::type_list<Lexeme>;
    static bool Parse(absl::Span<Lexeme const> &lexemes,
                      std::string_view &consumed, auto &&out) {
      PARSE_DEBUG_LOG();
      if (lexemes.empty() or S != lexemes[0].content()) { return false; }
      out      = lexemes[0];
      consumed = internal_parser_dsl::ExtractRange(lexemes, lexemes.subspan(1));
      return true;
    }
  };

 public:
  static constexpr auto parser = Impl();
};

// A parser that attempts to parse `P`, binding if `P` parses successfully, but
// otherwise consumes no lexemes and is considered to have parsed successfully.
template <Parser P>
struct Optional {
  static constexpr auto parser = Impl(P());

 private:
  struct Impl {
    using match_type = typename decltype(P::parser)::match_type;

    static bool Parse(absl::Span<Lexeme const> &lexemes,
                      std::string_view &consumed, auto &&out) {
      P::parser.Parse(lexemes, consumed, out);
      return true;
    }
  };
};

template <Parser P>
constexpr auto operator~(P) {
  return internal_parser_dsl::Ignored<P>();
}

template <char C>
constexpr auto DelimitedBy(auto P) {
  return internal_parser_dsl::DelimitedBy<C, decltype(P)>();
}

constexpr auto Bracketed(Parser auto P) { return DelimitedBy<'['>(P); }
constexpr auto Parenthesized(Parser auto P) { return DelimitedBy<'('>(P); }
constexpr auto Braced(Parser auto P) { return DelimitedBy<'{'>(P); }

template <size_t MinLength = 0>
constexpr auto CommaSeparatedListOf(Parser auto P) {
  return internal_parser_dsl::SeparatedList<Lexeme::Kind::Comma, decltype(P),
                                            MinLength>();
}
constexpr auto NewlineSeparatedListOf(Parser auto P) {
  return internal_parser_dsl::SeparatedList<Lexeme::Kind::Newline, decltype(P),
                                            0>();
}

template <typename F>
constexpr auto Bind(F) requires(std::is_empty_v<F>) {
  return internal_parser_dsl::BindImpl<F, false>();
}
template <typename F>
constexpr auto BindWithRange(F) requires(std::is_empty_v<F>) {
  return internal_parser_dsl::BindImpl<F, true>();
}

}  // namespace frontend

#endif  // ICARUS_FRONTEND_PARSER_DSL_H

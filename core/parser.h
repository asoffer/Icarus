#ifndef ICARUS_CORE_PARSER_H
#define ICARUS_CORE_PARSER_H

#include <type_traits>

#include "absl/types/span.h"
#include "base/compile_time_string.h"
#include "base/meta.h"
#include "core/lexeme.h"

namespace core {
// The concept `core::Parser` models types codify a recursive-descent parsing
// strategy. Parsing comes in two parts: First a token stream needs to be
// matched. Second the matched tokens need to be transfigured into the entity
// that the parser ultimately creates. We provide two mechanisms by which one
// can define a type that satisfies the `Parser` concept.
//
// One such possibility is that the type has a static member function `Parse`
// and a type member `match_type`. In this case, `match_type` must be a
// `base::type_list` consisting of the types that will be matched by the parser.
// The function `Parse` will be invoked with a reference to an
// `absl::Span<Lexeme const>` and a sequence of references to the types named in
// `match_type`. The function must return a bool indicating if parsing occurred
// successfully. If parsing was successful, the function must remove all
// consumed lexemes from the span. If parsing failed, the function must not
// modify the span of lexemes at all.
//
// Alternatively, you can take an existing parser and modify how it creates
// objects by specifying an existing `Parser` and how its matches should be
// bound. This can be done by providing a static constant `Parser` named
// `parser` and a static member function named `bind`.
//
namespace internal_parser {

template <typename P>
struct BindInvokeResult {
  template <typename... Args>
  struct Get {
    using type = decltype(
        P::bind(std::declval<std::string_view>(), std::declval<Args>()...));
  };
};

template <typename P, typename = void>
struct MatchTypeImpl {
  using type = base::type_list<typename base::reduce_t<
      BindInvokeResult<P>::template Get,
      typename MatchTypeImpl<decltype(P::parser)>::type>::type>;
};

template <typename P>
struct MatchTypeImpl<P, std::void_t<typename P::match_type>> {
  using type = typename P::match_type;
};

template <typename P, typename TypeList, typename = void>
struct BasicParseableWith : std::false_type {};

template <typename P, typename... Ts>
struct BasicParseableWith<
    P, base::type_list<Ts...>,
    std::void_t<decltype(P::Parse(std::declval<absl::Span<Lexeme const> &>(),
                                  std::declval<std::string_view &>(),
                                  std::declval<Ts &>()...))>>
    : std::bool_constant<base::meta<decltype(P::Parse(
                             std::declval<absl::Span<Lexeme const> &>(),
                             std::declval<std::string_view &>(),
                             std::declval<Ts &>()...))> == base::meta<bool>> {};

template <typename P>
concept BasicParser = (requires { typename P::match_type; } and
                       BasicParseableWith<P, typename P::match_type>::value);

template <typename P, typename TypeList, typename = void>
struct BindableWith : std::false_type {};

template <typename P, typename... Ts>
struct BindableWith<
    P, base::type_list<Ts...>,
    std::void_t<decltype(P::bind(std::declval<std::string_view>(),
                                 std::declval<Ts>()...))>> : std::true_type {};

template <typename P>
concept ParserAdaptor = (requires {
  P::parser;
  P::bind;
} and BindableWith<P, typename ::core::internal_parser::
                             MatchTypeImpl<decltype(P::parser)>::type>::value);

// Given a reference to a span of `lexemes` and a suffix `remaining`, replaces
// `lexemes` with `remaining`, and returns a `std::string_view` covering those
// tokens that were in `lexemes` but not `remaining`.
inline std::string_view ExtractRange(absl::Span<Lexeme const> &lexemes,
                                     absl::Span<Lexeme const> remaining) {
  ASSERT(lexemes.size() != 0);
  std::string_view result;
  if (lexemes.size() == remaining.size()) {
    result = std::string_view(lexemes.front().content().begin(), 0);
  } else {
    Lexeme const &last   = *(remaining.data() - 1);
    char const *endpoint = last.content().data() + last.content().size();
    size_t length        = endpoint - lexemes.front().content().data();
    result = std::string_view(lexemes.front().content().data(), length);
  }
  lexemes = remaining;
  return result;
}

}  // namespace internal_parser

template <typename P>
using MatchType = typename internal_parser::MatchTypeImpl<P>::type;

template <typename P>
concept Parser = std::is_empty_v<P> and (internal_parser::BasicParser<P> or
                                         internal_parser::ParserAdaptor<P>);

template <Parser P, typename... MatchTypes>
bool Parse(P, absl::Span<Lexeme const> &lexemes, std::string_view &consumed,
           MatchTypes &... matches) {
  // TODO: Add the requires-clause that demands each `matches` can be assigned
  // to based on the match_type.
  if constexpr (internal_parser::ParserAdaptor<P>) {
    static_assert(sizeof...(matches) == 1);

    using match_tuple_t =
        base::reduce_t<std::tuple, MatchType<decltype(P::parser)>>;
    match_tuple_t out_tuple;
    bool parsed = std::apply(
        [&](auto &... outs) {
          return ::core::Parse(P::parser, lexemes, consumed, outs...);
        },
        out_tuple);
    if (not parsed) { return false; }
    ((matches = std::apply(
          [&](auto &&... outs) { return P::bind("", std::move(outs)...); },
          std::move(out_tuple))),
     ...);
    return true;
  } else {
    return P::Parse(lexemes, consumed, matches...);
  }
}

// A parser that matches a lexeme exactly.
template <base::CompileTimeString S>
struct Match {
  using match_type = base::type_list<Lexeme>;
  static bool Parse(absl::Span<Lexeme const> &lexemes,
                    std::string_view &consumed, auto &&out) {
    if (lexemes.empty() or S != lexemes[0].content()) { return false; }
    out      = lexemes[0];
    consumed = internal_parser::ExtractRange(lexemes, lexemes.subspan(1));
    return true;
  }
};

// A parser that matches any one lexeme whose kind is `K`.
template <Lexeme::Kind K>
struct Kind {
  using match_type = base::type_list<std::string_view>;
  static bool Parse(absl::Span<Lexeme const> &lexemes,
                    std::string_view &consumed, auto &&out) {
    if (lexemes.empty() or lexemes[0].kind() != K) { return false; }
    out      = lexemes[0].content();
    consumed = internal_parser::ExtractRange(lexemes, lexemes.subspan(1));
    return true;
  }
};

// A parser that never matches.
struct Nothing {
  using match_type = base::type_list<>;
  static bool Parse(absl::Span<Lexeme const> &, std::string_view &) {
    return false;
  }
};

namespace internal_parser {

template <Parser P>
struct OptionalImpl {
  using match_type = base::transform_t<std::optional, typename MatchTypeImpl<P>::type>;

  static bool Parse(absl::Span<Lexeme const> &lexemes, std::string_view &consumed, auto &&out) {
    if (not core::Parse(P(), lexemes, consumed, out)) { out = std::nullopt; }
    return true;
  }
};

template <Parser L, Parser R>
struct Disjunction {
  static_assert(base::meta<MatchType<L>> == base::meta<MatchType<R>>);
  using match_type = MatchType<L>;
  static bool Parse(absl::Span<Lexeme const> &lexemes,
                    std::string_view &consumed, auto &&out) {
    absl::Span span = lexemes;
    bool result     = core::Parse(L(), span, consumed, out) or
                  core::Parse(R(), span, consumed, out);
    if (result) { consumed = internal_parser::ExtractRange(lexemes, span); }
    return result;
  }
};

template <Parser P, size_t... Ns>
bool CallWithIgnores(std::index_sequence<Ns...>,
                     absl::Span<Lexeme const> &lexemes,
                     std::string_view &consumed) {
  return ::core::Parse(P(), lexemes, consumed, (std::ignore = Ns)...);
}

template <Parser P>
struct Ignored {
  using match_type = base::type_list<>;

  static bool Parse(absl::Span<Lexeme const> &lexemes,
                    std::string_view &consumed) {
    constexpr size_t N = base::Length(MatchType<P>{});
    return CallWithIgnores<P>(std::make_index_sequence<N>{}, lexemes, consumed);
  }
};

template <Parser L, Parser R>
struct Sequenced {
  using match_type = base::type_list_cat<MatchType<L>, MatchType<R>>;

  static bool Parse(absl::Span<Lexeme const> &lexemes,
                    std::string_view &consumed, auto &&... outs) {
    auto [l_out, r_out] = base::SplitTuple<base::Length(MatchType<L>{}),
                                           base::Length(MatchType<R>{})>(
        std::forward_as_tuple(outs...));

    absl::Span span = lexemes;
    std::string_view ignore;
    bool parsed = std::apply(
        [&](auto &... outs) {
          return ::core::Parse(L(), span, ignore, outs...);
        },
        l_out);
    if (not parsed) { return false; }
    parsed = std::apply(
        [&](auto &... outs) {
          return ::core::Parse(R(), span, ignore, outs...);
        },
        r_out);
    if (not parsed) { return false; }

    consumed = internal_parser::ExtractRange(lexemes, span);
    return true;
  }
};

}  // namespace internal_parser

// A parser that attempts to parse `P`, binding if `P` parses successfully, but
// otherwise consumes no lexemes and is considered to have parsed successfully.
static constexpr auto Optional(Parser auto P) {
  return internal_parser::OptionalImpl<decltype(P)>();
}

template <typename T>
inline constexpr auto Construct = []<typename... Args>(Args &&... args)  //
    requires(std::constructible_from<T, Args &&...>) {
  return T(std::forward<Args>(args)...);
};

inline constexpr auto Identity = []<typename T>(std::string_view, T t) -> T {
  return std::move(t);
};

template <typename T, typename Out = T>
inline constexpr auto MakeUnique =
    []<typename... Args>(Args &&... args)
        -> std::unique_ptr<Out> requires(std::constructible_from<T, Args...>) {
  return std::make_unique<T>(std::forward<Args>(args)...);
};

template <typename T>
inline constexpr auto Vector = []<typename Arg>(Arg &&arg) {
  std::vector<T> v;
  v.push_back(std::forward<Arg>(arg));
  return v;
};

}  // namespace core

// Given two parsers `L` and `R` for the same type `T`, returns a parser for `T`
// which attempts to match `T` first by using `L`, and if that fails, then by
// using `R`.
template <core::Parser L, core::Parser R>
constexpr auto operator|(L, R) requires(base::meta<core::MatchType<L>> ==
                                        base::meta<core::MatchType<R>>) {
  return core::internal_parser::Disjunction<L, R>();
}

template <core::Parser P>
constexpr auto operator~(P) {
  return ::core::internal_parser::Ignored<P>();
}

// Given two parsers `L` and `R` returns a parser matching those lexeme streams
// which matching `L` and then `R`.`
template <core::Parser L, core::Parser R>
constexpr auto operator+(L, R) {
  return core::internal_parser::Sequenced<L, R>();
}

#endif  // ICARUS_CORE_PARSER_H

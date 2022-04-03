#ifndef ICARUS_CORE_PARSER_H
#define ICARUS_CORE_PARSER_H

#include <type_traits>

#include "absl/types/span.h"
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
    using type = decltype(P::bind(std::declval<Args>()...));
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
                                  std::declval<Ts &>()...))>>
    : std::bool_constant<base::meta<decltype(P::Parse(
                             std::declval<absl::Span<Lexeme const> &>(),
                             std::declval<Ts &>()...))> == base::meta<bool>> {};

template <typename P>
concept BasicParser = (requires { typename P::match_type; } and
                       BasicParseableWith<P, typename P::match_type>::value);

template <typename P, typename TypeList, typename = void>
struct BindableWith : std::false_type {};

template <typename P, typename... Ts>
struct BindableWith<P, base::type_list<Ts...>,
                    std::void_t<decltype(P::bind(std::declval<Ts>()...))>>
    : std::true_type {};

template <typename P>
concept ParserAdaptor = (requires {
  P::parser;
  P::bind;
} and BindableWith<P, typename decltype(P::parser)::match_type>::value);

}  // namespace internal_parser

template <typename P>
using MatchType = typename internal_parser::MatchTypeImpl<P>::type;

template <typename P>
concept Parser =
    internal_parser::BasicParser<P> or internal_parser::ParserAdaptor<P>;

}  // namespace core

#endif  // ICARUS_CORE_PARSER_H

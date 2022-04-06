#ifndef ICARUS_FRONTEND_PARSER_DSL_H
#define ICARUS_FRONTEND_PARSER_DSL_H

#include <concepts>
#include <type_traits>

#include "absl/cleanup/cleanup.h"
#include "absl/types/span.h"
#include "base/meta.h"
#include "core/lexeme.h"
#include "core/parser.h"

namespace frontend {

template <typename P>
concept Parser = core::Parser<P>;

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

template <core::Lexeme::Kind Separator, Parser P, size_t MinLength,
          template <typename...> typename Container = std::vector>
struct SeparatedList {
 private:
  static constexpr size_t kNumMatches = base::Length(core::MatchType<P>{});
  using element_type =
      std::conditional_t<kNumMatches == 1, base::head<core::MatchType<P>>,
                         base::reduce_t<std::tuple, core::MatchType<P>>>;

 public:
  using match_type = base::type_list<Container<element_type>>;

  static bool Parse(absl::Span<core::Lexeme const> &lexemes,
                    std::string_view &consumed, auto &&out) {
    Container<element_type> result;
    auto span = lexemes;
    element_type v;
    if (not core::Parse(P(), span, consumed, v)) { return MinLength == 0; }
    result.push_back(std::move(v));
    while (true) {
      if (result.size() >= MinLength and
          (span.empty() or span[0].kind() != Separator)) {
        consumed = core::internal_parser::ExtractRange(lexemes, span);
        out      = std::move(result);
        return true;
      }
      span.remove_prefix(1);
      if (core::Parse(P(), span, consumed, v)) {
        result.emplace_back(std::move(v));
      } else {
        return false;
      }
    }
  }
};

template <char C, typename P>
struct DelimitedBy {
  using match_type = core::MatchType<P>;
  static bool Parse(absl::Span<core::Lexeme const> &lexemes,
                    std::string_view &consumed, auto &&... out) {
    auto range = CheckBounds(lexemes);
    if (not range.data()) { return false; }
    bool result = core::Parse(P(), range, consumed, out...) and range.empty();
    if (result) {
      // TODO: Update `consumed`
      lexemes.remove_prefix(lexemes.front().match_offset() + 1);
    }
    return result;
  }

 private:
  static absl::Span<core::Lexeme const> CheckBounds(
      absl::Span<core::Lexeme const> &lexemes) {
    if (lexemes.empty() or lexemes.front().content().size() != 1 or
        lexemes.front().content()[0] != C) {
      return absl::Span<core::Lexeme const>(nullptr, 0);
    } else {
      size_t offset = lexemes.front().match_offset();
      return lexemes.subspan(1, offset - 1);
    }
  }
};

}  // namespace internal_parser_dsl

template <char C>
constexpr auto DelimitedBy(auto P) {
  return internal_parser_dsl::DelimitedBy<C, decltype(P)>();
}

constexpr auto Bracketed(Parser auto P) { return DelimitedBy<'['>(P); }
constexpr auto Parenthesized(Parser auto P) { return DelimitedBy<'('>(P); }
constexpr auto Braced(Parser auto P) { return DelimitedBy<'{'>(P); }

template <size_t MinLength = 0>
constexpr auto CommaSeparatedListOf(Parser auto P) {
  return internal_parser_dsl::SeparatedList<core::Lexeme::Kind::Comma,
                                            decltype(P), MinLength>();
}
constexpr auto NewlineSeparatedListOf(Parser auto P) {
  return internal_parser_dsl::SeparatedList<core::Lexeme::Kind::Newline,
                                            decltype(P), 0>();
}

}  // namespace frontend

#endif  // ICARUS_FRONTEND_PARSER_DSL_H

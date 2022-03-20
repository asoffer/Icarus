#include "frontend/parse.h"

#include <concepts>
#include <string_view>

#include "absl/cleanup/cleanup.h"
#include "absl/types/span.h"
#include "ast/ast.h"
#include "base/debug.h"
#include "ir/value/addr.h"
#include "type/primitive.h"

namespace frontend {
namespace {

#if defined(ICARUS_DEBUG)
int log_indentation = 0;
bool LoggingEnabled() {
  static bool b = true or base::LoggingEnabled("parser");
  return b;
}

constexpr std::string_view Prettify(std::string_view sv, std::string_view func) {
  auto end = sv.find("::Parse(");
  if (end == std::string_view::npos) { return func; }
  auto start = sv.find("(anonymous namespace)::") + 23;
  if (start > sv.size()) { start = sv.find("bool ") + 5; }
  if (start > sv.size()) { start = 0; }
  return sv.substr(start, end - start);
}

#define PARSE_DEBUG_LOG()                                                      \
  if (LoggingEnabled()) {                                                      \
    absl::Format(&std::cerr, "%s%s\n",                                         \
                 std::string(2 * (log_indentation++), ' '),                    \
                 Prettify(__PRETTY_FUNCTION__, __func__));                     \
  }                                                                            \
  absl::Cleanup c = [&, f = Prettify(__PRETTY_FUNCTION__, __func__)] {         \
    if (LoggingEnabled()) {                                                    \
      absl::Format(&std::cerr, "%sFinished %s\n",                              \
                   std::string(2 * --log_indentation, ' '), f);                \
    }                                                                          \
  }
#else
#define PARSE_DEBUG_LOG() (void)0
#endif

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

struct IdentifierImpl {
  using type = std::string_view;

  static bool Parse(absl::Span<Lexeme const> &lexemes, auto &&out) {
    PARSE_DEBUG_LOG();
    if (lexemes.empty()) { return false; }
    auto const &lexeme = lexemes[0];
    if (lexeme.kind() != Lexeme::Kind::Identifier) { return false; }
    if (kKeywords.contains(lexemes[0].content())) { return false; }
    out = lexemes[0].content();
    lexemes.remove_prefix(1);
    return true;
  }

 private:
  static const absl::flat_hash_set<std::string_view> kKeywords;
};

const absl::flat_hash_set<std::string_view> IdentifierImpl::kKeywords = {
    "false", "true",   "null", "i8",      "i16",   "i32", "i64",
    "u8",    "u16",    "u32",  "u64",     "bool",  "f32", "f64",
    "type",  "module", "byte", "builtin", "import"};

struct OperatorImpl {
  using type = std::string_view;

  static bool Parse(absl::Span<Lexeme const> &lexemes, auto &&out) {
    PARSE_DEBUG_LOG();
    if (lexemes.empty()) { return false; }
    auto const &lexeme = lexemes[0];
    if (lexeme.kind() != Lexeme::Kind::Operator) { return false; }
    out = lexemes[0].content();
    lexemes.remove_prefix(1);
    return true;
  }
};

template <Parser L, Parser R>
struct DisjunctionImpl {
  using type = typename L::type;
  static bool Parse(absl::Span<Lexeme const> &lexemes, auto &&out) {
    return L::Parse(lexemes, out) or R::Parse(lexemes, out);
  }
};

template <Lexeme::Kind Separator, Parser P>
struct SeparatedListImpl {
  using type = std::vector<typename P::type>;
  static bool Parse(absl::Span<Lexeme const> &lexemes, auto &&out) {
    PARSE_DEBUG_LOG();
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

enum Precedence {
  kTimesDiv,
  kPlusMinus,
  kAssignment,
  kMaxPrecedence,
};

Precedence GetPrecedence(std::string_view op) {
  if (op == "=") { return kAssignment; }
  if (op == "+") { return kPlusMinus; }
  if (op == "-") { return kPlusMinus; }
  if (op == "*") { return kTimesDiv; }
  if (op == "/") { return kTimesDiv; }
  return kMaxPrecedence;
}

std::string_view ExtractRange(absl::Span<Lexeme const> &lexemes,
                              absl::Span<Lexeme const> remaining) {
  ASSERT(lexemes.size() != 0);
  Lexeme const &last   = *(remaining.data() - 1);
  char const *endpoint = last.content().data() + last.content().size();
  size_t length        = endpoint - lexemes.front().content().data();
  std::string_view result(lexemes.front().content().data(), length);
  lexemes = remaining;
  return result;
}

template <auto P, typename T>
bool ParseCommaSeparatedList(
    absl::Span<Lexeme const> &lexemes,
    std::vector<T>
        &out) requires(std::predicate<decltype(P), decltype(lexemes), T &>) {
  PARSE_DEBUG_LOG();
  auto span = lexemes;
  T t;
  if (not P(span, t)) { return false; }
  out.push_back(std::move(t));
  while (true) {
    if (span.empty() or span[0].kind() != Lexeme::Kind::Comma) {
      lexemes = span;
      return true;
    }
    span.remove_prefix(1);
    if (P(span, t)) {
      out.emplace_back(std::move(t));
    } else {
      return false;
    }
  }
}

template <auto P, char C, typename T>
bool ParseDelimited(absl::Span<Lexeme const> &lexemes, T &out) {
  if (lexemes.empty() or lexemes.front().content().size() != 1 or
      lexemes.front().content()[0] != C) {
    return false;
  }
  size_t offset = lexemes.front().match_offset();
  auto range    = lexemes.subspan(1, offset - 1);
  if (P(range, out) and range.empty()) {
    lexemes = lexemes.subspan(offset + 1);
    return true;
  } else {
    return false;
  }
}

template <auto P, typename T>
bool ParseBracketed(absl::Span<Lexeme const> &lexemes, T &out) {
  PARSE_DEBUG_LOG();
  return ParseDelimited<P, '['>(lexemes, out);
}

template <auto P, typename T>
bool ParseParenthesized(absl::Span<Lexeme const> &lexemes, T &out) {
  PARSE_DEBUG_LOG();
  return ParseDelimited<P, '('>(lexemes, out);
}

template <auto P, typename T>
bool ParseBraced(absl::Span<Lexeme const> &lexemes, T &out) {
  PARSE_DEBUG_LOG();
  return ParseDelimited<P, '{'>(lexemes, out);
}

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

template <Parser L, Parser R>
constexpr auto operator|(L, R) requires(base::meta<typename L::type> ==
                                        base::meta<typename R::type>) {
  return DisjunctionImpl<L, R>{};
}

template <char C>
constexpr auto DelimitedBy(auto P) {
  return DelimitedByImpl<C, decltype(P)>();
}

constexpr auto Bracketed(auto P) { return DelimitedBy<'['>(P); }
constexpr auto Parenthesized(auto P) { return DelimitedBy<'('>(P); }
constexpr auto Braced(auto P) { return DelimitedBy<'{'>(P); }
constexpr auto CommaSeparatedListOf(auto P) {
  return SeparatedListImpl<Lexeme::Kind::Comma, decltype(P)>();
}

constexpr auto Identifier = IdentifierImpl();
constexpr auto AnOperator = OperatorImpl();  // TODO: Rename or remove enum so
                                             // this can be named appropriately.
constexpr auto DeclarationId = Identifier | Parenthesized(AnOperator);

template <typename T, auto P, typename... Args>
bool ParseSequence(absl::Span<Lexeme const> &lexemes, std::vector<T> &out,
                   Args &&... args) {
  PARSE_DEBUG_LOG();
  T t;
  while (not lexemes.empty() and P(lexemes, t, std::forward<Args>(args)...)) {
    out.push_back(std::move(t));
  }
  return true;
}

template <auto P>
bool Optionally(absl::Span<Lexeme const> &lexemes, auto &out) {
  PARSE_DEBUG_LOG();
  P(lexemes, out);
  return true;
}

bool MatchOperator(std::string_view s, absl::Span<Lexeme const> &lexemes) {
  PARSE_DEBUG_LOG();
  if (lexemes.empty()) { return false; }
  if (lexemes[0].kind() == Lexeme::Kind::Operator and
      lexemes[0].content() == s) {
    lexemes.remove_prefix(1);
    return true;
  }
  return false;
}

bool ParseExpressionWithPrecedence(absl::Span<Lexeme const> &lexemes,
                                   std::unique_ptr<ast::Expression> &out,
                                   int precedence_limit);

bool ParseNamedArgument(absl::Span<Lexeme const> &lexemes,
                        ast::Call::Argument &result) {
  PARSE_DEBUG_LOG();
  if (lexemes.size() <= 2) { return false; }
  auto const &name = lexemes[0];
  if (name.kind() != Lexeme::Kind::Identifier) { return false; }

  absl::Span span = lexemes.subspan(1);
  if (not MatchOperator("=", span)) { return false; }

  std::unique_ptr<ast::Expression> expr;
  if (not ParseExpressionWithPrecedence(span, expr, kAssignment)) {
    return false;
  }
  result  = ast::Call::Argument(name.content(), std::move(expr));
  lexemes = span;
  return true;
}

bool ParsePositionalArgument(absl::Span<Lexeme const> &lexemes,
                             ast::Call::Argument &result) {
  PARSE_DEBUG_LOG();
  std::unique_ptr<ast::Expression> e;
  if (not ParseExpressionWithPrecedence(lexemes, e, kAssignment)) {
    return false;
  }
  result = ast::Call::Argument("", std::move(e));
  return true;
}

bool ParseDeclarationUniquePtr(absl::Span<Lexeme const> &lexemes,
                               std::unique_ptr<ast::Node> &out);

}  // namespace

// TODO: Not tested.
bool ParseExpression(absl::Span<Lexeme const> &lexemes,
                     std::unique_ptr<ast::Expression> &out) {
  PARSE_DEBUG_LOG();
  return ParseExpressionWithPrecedence(lexemes, out, kMaxPrecedence);
}

bool ParseDeclarationId(absl::Span<Lexeme const> &lexemes,
                        ast::Declaration::Id &out) {
  std::string_view sv;
  if (not DeclarationId.Parse(lexemes, sv)) { return false; }
  out = ast::Declaration::Id(sv);
  return true;
}

bool ParseLabel(absl::Span<Lexeme const> &lexemes,
                std::optional<ast::Label> &out) {
  PARSE_DEBUG_LOG();
  if (lexemes.size() < 3) { return false; }
  if (lexemes[0].kind() != Lexeme::Kind::Hash or
      lexemes[1].kind() != Lexeme::Kind::Operator or
      lexemes[1].content() != "." or
      lexemes[2].kind() != Lexeme::Kind::Identifier or
      std::distance(lexemes[0].content().begin(),
                    lexemes[2].content().begin()) != 2) {
    return false;
  }

  out = ast::Label(ExtractRange(lexemes, lexemes.subspan(3)));
  return true;
}

bool ParseNumber(absl::Span<Lexeme const> &lexemes,
                 std::unique_ptr<ast::Expression> &out) {
  PARSE_DEBUG_LOG();
  if (lexemes.empty()) { return false; }
  if (lexemes[0].kind() != Lexeme::Kind::Number) { return false; }
  // TODO: Fill
  out = std::make_unique<ast::Terminal>(lexemes[0].content(), "");
  lexemes.remove_prefix(1);
  return true;
}

bool ParseStringLiteral(absl::Span<Lexeme const> &lexemes,
                        std::unique_ptr<ast::Expression> &out) {
  PARSE_DEBUG_LOG();
  if (lexemes.empty()) { return false; }
  if (lexemes[0].kind() != Lexeme::Kind::String) { return false; }
  // TODO: Fill
  out = std::make_unique<ast::Terminal>(lexemes[0].content(), "");
  lexemes.remove_prefix(1);
  return true;
}

bool ParseCallArgument(absl::Span<Lexeme const> &lexemes,
                       ast::Call::Argument &out) {
  PARSE_DEBUG_LOG();
  return ParseNamedArgument(lexemes, out) or
         ParsePositionalArgument(lexemes, out);
}

bool ParseYieldStatement(absl::Span<Lexeme const> &lexemes,
                         std::unique_ptr<ast::Node> &n) {
  PARSE_DEBUG_LOG();
  absl::Span span = lexemes;

  std::optional<ast::Label> l;
  std::vector<ast::Call::Argument> arguments;
  constexpr bool (*ParseArguments)(absl::Span<Lexeme const> &,
                                   std::vector<ast::Call::Argument> &) =
      ParseCommaSeparatedList<ParseCallArgument>;

  bool result = Optionally<ParseLabel>(span, l) and
                MatchOperator("<<", span) and
                Optionally<ParseArguments>(span, arguments);
  if (not result) { return false; }
  n = std::make_unique<ast::YieldStmt>(ExtractRange(lexemes, span),
                                       std::move(arguments), std::move(l));
  return true;
}

bool ParseReturnStatement(absl::Span<Lexeme const> &lexemes,
                          std::unique_ptr<ast::Node> &out) {
  PARSE_DEBUG_LOG();
  if (lexemes.empty() or lexemes[0].content() != "return") { return false; }
  auto span = lexemes.subspan(1);
  std::vector<std::unique_ptr<ast::Expression>> exprs;

  constexpr bool (*ParseExprs)(
      absl::Span<Lexeme const> &,
      std::vector<std::unique_ptr<ast::Expression>> &) =
      ParseCommaSeparatedList<ParseExpression>;
  if (Optionally<ParseExprs>(span, exprs)) {
    out = std::make_unique<ast::ReturnStmt>(ExtractRange(lexemes, span),
                                            std::move(exprs));
    return true;
  } else {
    return false;
  }
}

bool ParseDeclaration(absl::Span<Lexeme const> &lexemes,
                      ast::Declaration &out) {
  PARSE_DEBUG_LOG();
  auto span = lexemes;
  std::vector<std::string_view> sv_ids;
  if (not Parenthesized(CommaSeparatedListOf(DeclarationId)).Parse(span, sv_ids)) {
    if (not DeclarationId.Parse(span, sv_ids.emplace_back())) { return false; }
  }
  std::vector<ast::Declaration::Id> ids;
  for (std::string_view sv : sv_ids) { ids.emplace_back(sv); }

  std::unique_ptr<ast::Expression> type_expression;
  std::unique_ptr<ast::Expression> initial_value;
  if (span.empty()) { return false; }
  if (span[0].kind() != Lexeme::Kind::Operator) { return false; }
  ast::Declaration::Flags flags{};
  if (span[0].content() == ":" or span[0].content() == "::") {
    if (span[0].content().size() == 2) { flags |= ast::Declaration::f_IsConst; }
    span.remove_prefix(1);
    if (not ParseExpression(span, type_expression)) { return false; }
  } else if (span[0].content() == ":=" or span[0].content() == "::=") {
    if (span[0].content().size() == 3) { flags |= ast::Declaration::f_IsConst; }
    span.remove_prefix(1);
    if (not ParseExpression(span, initial_value)) { return false; }
  } else {
    return false;
  }

  out = ast::Declaration(ExtractRange(lexemes, span), std::move(ids),
                         std::move(type_expression), std::move(initial_value),
                         flags);
  return true;
}

bool ParseAssignment(absl::Span<Lexeme const> &lexemes,
                     std::unique_ptr<ast::Node> &out) {
  PARSE_DEBUG_LOG();
  auto span = lexemes;
  std::unique_ptr<ast::Expression> l, r;
  if (not ParseExpressionWithPrecedence(span, l, kAssignment)) { return false; }
  if (span.empty()) { return false; }
  if (span[0].kind() != Lexeme::Kind::Operator) { return false; }
  span.remove_prefix(1);
  if (span.empty()) { return false; }
  if (not ParseExpressionWithPrecedence(span, r, kAssignment)) { return false; }

  // TODO: Multi-assignment
  std::vector<std::unique_ptr<ast::Expression>> lhs, rhs;
  lhs.push_back(std::move(l));
  rhs.push_back(std::move(r));
  out = std::make_unique<ast::Assignment>(ExtractRange(lexemes, span),
                                          std::move(lhs), std::move(rhs));
  return true;
}

// TODO: Everything above this line is tested.

bool ParseKeyword(std::string_view s, absl::Span<Lexeme const> &lexemes) {
  PARSE_DEBUG_LOG();
  if (not lexemes.empty() and lexemes[0].kind() == Lexeme::Kind::Identifier and
      lexemes[0].content() == s) {
    lexemes.remove_prefix(1);
    return true;
  }
  return false;
}

bool ParseStatement(absl::Span<Lexeme const> &lexemes,
                    std::unique_ptr<ast::Node> &);

bool ParseBracedStatements(absl::Span<Lexeme const> &lexemes,
                           std::vector<std::unique_ptr<ast::Node>> &out) {
  PARSE_DEBUG_LOG();
  return ParseBraced<ParseSequence<std::unique_ptr<ast::Node>, ParseStatement>>(
      lexemes, out);
}

namespace {
bool ParseDeclarationUniquePtr(absl::Span<Lexeme const> &lexemes,
                               std::unique_ptr<ast::Node> &out) {
  PARSE_DEBUG_LOG();
  if (ast::Declaration decl; ParseDeclaration(lexemes, decl)) {
    out = std::make_unique<ast::Declaration>(std::move(decl));
    return true;
  } else {
    return false;
  }
}
}  // namespace

bool ParseFunctionLiteral(absl::Span<Lexeme const> &lexemes,
                          std::unique_ptr<ast::Expression> &out) {
  PARSE_DEBUG_LOG();
  std::vector<ast::Declaration> parameters;
  std::vector<std::unique_ptr<ast::Node>> stmts;
  absl::Span span = lexemes;
  if (not ParseParenthesized<
          ParseCommaSeparatedList<ParseDeclaration, ast::Declaration>>(
          span, parameters)) {
    return false;
  }

  if (not MatchOperator("->", span)) { return false; }

  std::vector<std::unique_ptr<ast::Expression>> out_params;
  if (not ParseParenthesized<ParseCommaSeparatedList<
          ParseExpression, std::unique_ptr<ast::Expression>>>(span,
                                                              out_params)) {
    return false;
  }

  if (not ParseBracedStatements(span, stmts)) { return false; }

  out = std::make_unique<ast::FunctionLiteral>(
      ExtractRange(lexemes, span), std::move(parameters), std::move(stmts),
      std::move(out_params));
  return true;
}

bool ParseStructLiteral(absl::Span<Lexeme const> &lexemes,
                        std::unique_ptr<ast::Expression> &out) {
  PARSE_DEBUG_LOG();
  std::vector<ast::Declaration> declarations;
  absl::Span span = lexemes;
  bool result     = ParseKeyword("struct", span) and
                ParseBraced<ParseSequence<ast::Declaration, ParseDeclaration>>(
                    span, declarations);
  if (not result) { return false; }
  out = std::make_unique<ast::StructLiteral>(ExtractRange(lexemes, span),
                                             std::move(declarations));
  return true;
}

bool ParseParameterizedStructLiteral(absl::Span<Lexeme const> &lexemes,
                                     std::unique_ptr<ast::Expression> &out) {
  PARSE_DEBUG_LOG();
  if (lexemes.empty() or lexemes[0].content() != "struct") { return false; }
  absl::Span span = lexemes.subspan(1);

  std::vector<ast::Declaration> parameters;
  if (not ParseParenthesized<
          ParseCommaSeparatedList<ParseDeclaration, ast::Declaration>>(
          span, parameters)) {
    return false;
  }

  std::vector<ast::Declaration> declarations;
  if (not ParseBraced<ParseSequence<ast::Declaration, ParseDeclaration>>(
          span, declarations)) {
    return false;
  }

  out = std::make_unique<ast::ParameterizedStructLiteral>(
      ExtractRange(lexemes, span), std::move(parameters),
      std::move(declarations));
  return true;
}

bool ParseScopeLiteral(absl::Span<Lexeme const> &lexemes,
                       std::unique_ptr<ast::Expression> &out) {
  PARSE_DEBUG_LOG();
  if (lexemes.empty() or lexemes[0].content() != "scope") { return false; }
  absl::Span span = lexemes.subspan(1);
  std::string_view id;
  if (not Bracketed(Identifier).Parse(span, id)) { return false; }

  std::vector<ast::Declaration> parameters;
  if (not ParseParenthesized<
          ParseCommaSeparatedList<ParseDeclaration, ast::Declaration>>(
          span, parameters)) {
    return false;
  }

  std::vector<std::unique_ptr<ast::Node>> body;
  if (not ParseBraced<
          ParseSequence<std::unique_ptr<ast::Node>, ParseStatement>>(span,
                                                                     body)) {
    return false;
  }

  out = std::make_unique<ast::ScopeLiteral>(
      ExtractRange(lexemes, span), ast::Declaration::Id(id),
      std::move(parameters), std::move(body));
  return true;
}

bool ParseTerminalOrIdentifier(absl::Span<Lexeme const> &lexemes,
                               std::unique_ptr<ast::Expression> &out) {
  PARSE_DEBUG_LOG();
  if (lexemes.empty()) { return false; }
  auto const &lexeme = lexemes[0];
  if (lexeme.kind() != Lexeme::Kind::Identifier) { return false; }

  std::string_view s = lexeme.content();
  if (s == "false") {
    out = std::make_unique<ast::Terminal>(s, false);
  } else if (s == "true") {
    out = std::make_unique<ast::Terminal>(s, true);
  } else if (s == "null") {
    out = std::make_unique<ast::Terminal>(s, ir::Null());
  } else if (s == "i8") {
    out = std::make_unique<ast::Terminal>(s, type::I8);
  } else if (s == "i16") {
    out = std::make_unique<ast::Terminal>(s, type::I16);
  } else if (s == "i32") {
    out = std::make_unique<ast::Terminal>(s, type::I32);
  } else if (s == "i64") {
    out = std::make_unique<ast::Terminal>(s, type::I64);
  } else if (s == "u8") {
    out = std::make_unique<ast::Terminal>(s, type::U8);
  } else if (s == "u16") {
    out = std::make_unique<ast::Terminal>(s, type::U16);
  } else if (s == "u32") {
    out = std::make_unique<ast::Terminal>(s, type::U32);
  } else if (s == "u64") {
    out = std::make_unique<ast::Terminal>(s, type::U64);
  } else if (s == "bool") {
    out = std::make_unique<ast::Terminal>(s, type::Bool);
  } else if (s == "f32") {
    out = std::make_unique<ast::Terminal>(s, type::F32);
  } else if (s == "f64") {
    out = std::make_unique<ast::Terminal>(s, type::F64);
  } else if (s == "type") {
    out = std::make_unique<ast::Terminal>(s, type::Type_);
  } else if (s == "module") {
    out = std::make_unique<ast::Terminal>(s, type::Module);
  } else if (s == "byte") {
    out = std::make_unique<ast::Terminal>(s, type::Byte);
  } else {
    out = std::make_unique<ast::Identifier>(lexeme.content());
  }
  lexemes.remove_prefix(1);
  return true;
}

bool ParseWhileStatement(absl::Span<Lexeme const> &lexemes,
                         std::unique_ptr<ast::Node> &out) {
  PARSE_DEBUG_LOG();
  if (lexemes.empty()) { return false; }

  if (lexemes[0].content() != "while") { return false; }
  auto span = lexemes.subspan(1);
  std::unique_ptr<ast::Expression> condition;
  std::vector<std::unique_ptr<ast::Node>> body;
  if (not ParseParenthesized<ParseExpression>(span, condition) or
      ParseBracedStatements(span, body)) {
    return false;
  }

  out = std::make_unique<ast::WhileStmt>(ExtractRange(lexemes, span),
                                         std::move(condition), std::move(body));
  return true;
}

bool ParseAtomicExpression(absl::Span<Lexeme const> &lexemes,
                           std::unique_ptr<ast::Expression> &e) {
  PARSE_DEBUG_LOG();
  return ParseNumber(lexemes, e) or ParseStringLiteral(lexemes, e) or
         ParseParenthesized<ParseExpression>(lexemes, e) or
         ParseFunctionLiteral(lexemes, e) or ParseStructLiteral(lexemes, e) or
         ParseParameterizedStructLiteral(lexemes, e) or
         ParseScopeLiteral(lexemes, e) or ParseTerminalOrIdentifier(lexemes, e);
}

bool ParseOperatorOrAtomicExpression(
    absl::Span<Lexeme const> &lexemes,
    std::variant<std::string_view, std::unique_ptr<ast::Expression>> &out,
    int precedence_limit) {
  PARSE_DEBUG_LOG();
  std::unique_ptr<ast::Expression> e;
  if (ParseAtomicExpression(lexemes, e)) {
    out = std::move(e);
    return true;
  } else if (lexemes.empty()) {
    return false;
  } else if ((lexemes[0].kind() == Lexeme::Kind::Operator and
              GetPrecedence(lexemes[0].content()) < precedence_limit) or
             (lexemes[0].kind() == Lexeme::Kind::Identifier and
              (lexemes[0].content() == "import"))) {
    out = lexemes[0].content();
    lexemes.remove_prefix(1);
    return true;
  } else {
    return false;
  }
}

namespace {
bool ParseExpressionWithPrecedence(absl::Span<Lexeme const> &lexemes,
                                   std::unique_ptr<ast::Expression> &e,
                                   int precedence_limit) {
  PARSE_DEBUG_LOG();
  auto span  = lexemes;
  using type = std::variant<std::string_view, std::unique_ptr<ast::Expression>>;
  std::vector<type> elements;
  (void)ParseSequence<type, ParseOperatorOrAtomicExpression>(span, elements,
                                                             precedence_limit);
  if (elements.empty() or
      std::holds_alternative<std::string_view>(elements.back())) {
    return false;
  }

  std::vector<std::unique_ptr<ast::Expression>> exprs;
  std::vector<std::string_view> operators;

  // Iterate through the elements first finding and applying all unary
  // operators. This is easiest to do by iterating backwards
  std::string_view *last_operator = nullptr;
  for (auto iter = elements.rbegin(); iter != elements.rend(); ++iter) {
    bool success = std::visit(
        [&](auto &element) {
          constexpr auto type = base::meta<std::decay_t<decltype(element)>>;
          if constexpr (type == base::meta<std::string_view>) {
            if (last_operator) {
              exprs.back() = std::make_unique<ast::UnaryOperator>(
                  *last_operator, ast::UnaryOperator::KindFrom(*last_operator),
                  std::move(exprs.back()));
            }
            last_operator = &element;
          } else {
            if (last_operator) {
              operators.push_back(*last_operator);
              last_operator = nullptr;
            }
            exprs.push_back(std::move(element));
          }
          return true;
        },
        *iter);
    if (not success) { return false; }
  }

  if (exprs.size() != operators.size() + 1) { return false; }

  // TODO: This does not respect operator precedence.
  auto operator_iter = operators.rbegin();
  e                  = std::move(*exprs.rbegin());
  for (auto iter = std::next(exprs.rbegin()); iter != exprs.rend(); ++iter) {
    e = std::make_unique<ast::BinaryOperator>(
        std::move(e), ast::BinaryOperator::KindFrom(*operator_iter++),
        std::move(*iter));
  }
  lexemes = span;
  return true;
}

}  // namespace

bool ParseStatement(absl::Span<Lexeme const> &lexemes,
                    std::unique_ptr<ast::Node> &node) {
  PARSE_DEBUG_LOG();
  absl::Cleanup cl = [&] {
    if (node) { LOG("", "%s", node->DebugString()); }
  };
  auto iter = std::find_if(lexemes.begin(), lexemes.end(), [](Lexeme const &l) {
    return l.kind() != Lexeme::Kind::Newline;
  });
  if (iter == lexemes.end()) { return false; }
  absl::Span span = lexemes = absl::MakeConstSpan(iter, lexemes.end());

  if (ParseWhileStatement(lexemes, node)) { return true; }
  if (ParseAssignment(lexemes, node)) { return true; }
  if (ParseDeclarationUniquePtr(lexemes, node)) { return true; }
  if (ParseReturnStatement(lexemes, node)) { return true; }
  if (ParseYieldStatement(lexemes, node)) { return true; }
  if (std::unique_ptr<ast::Expression> e; ParseExpression(lexemes, e)) {
    node = std::move(e);
    return true;
  };

  return false;
}

std::optional<ast::Module> ParseModule(absl::Span<Lexeme const> lexemes,
                                       diagnostic::DiagnosticConsumer &) {
  PARSE_DEBUG_LOG();
  while (not lexemes.empty() and
         (lexemes.front().kind() == Lexeme::Kind::Newline or
          lexemes.front().kind() == Lexeme::Kind::EndOfFile)) {
    lexemes.remove_prefix(1);
  }
  if (lexemes.empty()) { return std::nullopt; }

  auto module = std::make_optional<ast::Module>(std::string_view(
      lexemes.front().content().begin(), lexemes.back().content().end()));
  std::unique_ptr<ast::Node> stmt;
  while (ParseStatement(lexemes, stmt)) { module->insert(std::move(stmt)); }
  while (not lexemes.empty() and
         (lexemes.front().kind() == Lexeme::Kind::Newline or
          lexemes.front().kind() == Lexeme::Kind::EndOfFile)) {
    lexemes.remove_prefix(1);
  }

  if (not lexemes.empty()) { return std::nullopt; }
  return module;
}

}  // namespace frontend

#include "frontend/parse.h"

#include <string_view>

#include "absl/types/span.h"
#include "ast/ast.h"
#include "base/debug.h"
#include "ir/value/addr.h"
#include "type/primitive.h"

namespace frontend {
namespace {

template <typename T, auto P>
bool ParseSequence(absl::Span<Lexeme const> &lexemes, std::vector<T> &out) {
  T t;
  while (P(lexemes, t)) { out.push_back(std::move(t)); }
  return true;
}

bool ParseOperator(std::string_view s, absl::Span<Lexeme const> &lexemes) {
  if (not lexemes.empty() and lexemes[0].kind() == Lexeme::Kind::Operator and
      lexemes[0].content() == s) {
    lexemes.remove_prefix(1);
    return true;
  }
  return false;
}

bool ParseKeyword(std::string_view s, absl::Span<Lexeme const> &lexemes) {
  if (not lexemes.empty() and lexemes[0].kind() == Lexeme::Kind::Identifier and
      lexemes[0].content() == s) {
    lexemes.remove_prefix(1);
    return true;
  }
  return false;
}

template <auto P, typename T>
bool Optionally(absl::Span<Lexeme const> &lexemes, T &out) {
  P(lexemes, out);
  return true;
}

std::string_view ExtractRange(absl::Span<Lexeme const> &lexemes,
                              absl::Span<Lexeme const> remaining) {
  ASSERT(lexemes.size() != 0);
  ASSERT(remaining.size() != 0);
  Lexeme const &last   = *(remaining.data() - 1);
  char const *endpoint = last.content().data() + last.content().size();
  size_t length        = endpoint - lexemes.front().content().data();
  lexemes              = remaining;
  std::string_view result(lexemes.front().content().data(), length);
  return result;
}

template <auto P, typename T>
bool ParseCommaSeparatedList(
    absl::Span<Lexeme const> &lexemes,
    std::vector<T>
        &out) requires(std::predicate<decltype(P), decltype(lexemes), T &>) {
  auto span = lexemes;
  T t;
  if (not P(span, t)) { return false; }
  out.push_back(std::exchange(t, T()));
  while (true) {
    if (span.empty() or span[0].content() != ",") {
      lexemes = span;
      return true;
    }
    span.remove_prefix(1);
    if (P(span, t)) {
      out.emplace_back(std::exchange(t, T()));
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
  return ParseDelimited<P, '['>(lexemes, out);
}

template <auto P, typename T>
bool ParseParenthesized(absl::Span<Lexeme const> &lexemes, T &out) {
  return ParseDelimited<P, '('>(lexemes, out);
}

template <auto P, typename T>
bool ParseBraced(absl::Span<Lexeme const> &lexemes, T &out) {
  return ParseDelimited<P, '{'>(lexemes, out);
}

// TODO ParseBracedDeclarations
// TODO ParseParenthesizedExpressionList;
// TODO ParseParenthesizedDeclarationList;
// TODO ParseParenthesizedDeclarationIdList
// TODO ParseParameterizedStructLiteral;
// TODO ParseScopeLiteral;
// TODO ParseUnaryOperator;
// TODO ParseBinaryOperator;

bool ParseStatement(absl::Span<Lexeme const> &lexemes,
                    std::unique_ptr<ast::Node> &);
bool ParseExpression(absl::Span<Lexeme const> &lexemes,
                     std::unique_ptr<ast::Expression> &out);
bool ParseIdentifier(absl::Span<Lexeme const> &lexemes,
                     std::unique_ptr<ast::Expression> &out);

bool ParseLabel(absl::Span<Lexeme const> &lexemes,
                std::unique_ptr<ast::Label> &out) {
  return false;
}

[[maybe_unused]] bool ParseDeclaration(absl::Span<Lexeme const> &lexemes,
                                       ast::Declaration &out) {
  return false;  // TODO
}

bool ParseFunctionLiteral(absl::Span<Lexeme const> &lexemes,
                          std::unique_ptr<ast::Expression> &out) {
  return false;  // TODO
}

bool ParseStructLiteral(absl::Span<Lexeme const> &lexemes,
                        std::unique_ptr<ast::Expression> &out) {
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
  return false;  // TODO
}

bool ParseScopeLiteral(absl::Span<Lexeme const> &lexemes,
                       std::unique_ptr<ast::Expression> &out) {
  return false;  // TODO
}

bool ParseUnaryOperator(absl::Span<Lexeme const> &lexemes,
                        std::unique_ptr<ast::Expression> &out) {
  if (lexemes.empty()) { return false; }

  auto span = lexemes;
  if (span[0].kind() != Lexeme::Kind::Operator) { return false; }
  span.remove_prefix(1);
  std::unique_ptr<ast::Expression> operand;
  auto r = ParseExpression(span, operand);
  if (not r) { return false; }
  return false;  // TODO
}

bool ParseBinaryOperator(absl::Span<Lexeme const> &lexemes,
                         std::unique_ptr<ast::Expression> &out) {
  // TODO
  return false;
}

bool ParseIdentifier(absl::Span<Lexeme const> &lexemes,
                     std::unique_ptr<ast::Expression> &out) {
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
  if (lexemes.empty()) { return false; }

  if (lexemes[0].content() != "while") { return false; }
  auto span = lexemes.subspan(1);
  std::unique_ptr<ast::Expression> condition;
  std::vector<std::unique_ptr<ast::Node>> body;
  if (not ParseParenthesized<ParseExpression>(span, condition) or
      not ParseBraced<
          ParseSequence<std::unique_ptr<ast::Node>, ParseStatement>>(span,
                                                                     body)) {
    return false;
  }

  out = std::make_unique<ast::WhileStmt>(ExtractRange(lexemes, span),
                                         std::move(condition), std::move(body));
  return true;
}

bool ParseAssignment(absl::Span<Lexeme const> &lexemes,
                     std::unique_ptr<ast::Node> &out) {
  if (lexemes.empty()) { return false; }

  auto span = lexemes;
  std::unique_ptr<ast::Expression> l, r;
  if (not ParseExpression(span, l)) { return false; }
  if (span.empty()) { return false; }
  std::string_view op = span[0].content();
  if (span[0].kind() != Lexeme::Kind::Operator) { return false; }
  span.remove_prefix(1);
  if (not ParseExpression(span, r)) { return false; }
  return false;  // TODO
}

bool ParseReturnStatement(absl::Span<Lexeme const> &lexemes,
                          std::unique_ptr<ast::Node> &out) {
  if (lexemes.empty()) { return false; }

  if (lexemes[0].content() != "return") { return false; }
  auto span = lexemes;
  span.remove_prefix(1);
  if (std::vector<std::unique_ptr<ast::Expression>> exprs;
      ParseCommaSeparatedList<ParseExpression>(span, exprs)) {
    out = std::make_unique<ast::ReturnStmt>(ExtractRange(lexemes, span),
                                            std::move(exprs));
    return true;
  } else {
    return false;
  }
}

bool ParseNamedArgument(absl::Span<Lexeme const> &lexemes,
                        ast::Call::Argument &result) {
  if (lexemes.size() <= 2) { return false; }
  auto const &name = lexemes[0];
  if (name.kind() != Lexeme::Kind::Identifier) { return false; }

  if (not ParseOperator("=", lexemes)) { return false; }

  absl::Span span = lexemes.subspan(2);
  std::unique_ptr<ast::Expression> expr;
  if (not ParseExpression(span, expr)) { return false; }
  result = ast::Call::Argument(name.content(), std::move(expr));
  return true;
}

bool ParsePositionalArgument(absl::Span<Lexeme const> &lexemes,
                             ast::Call::Argument &result) {
  std::unique_ptr<ast::Expression> expr;
  if (not ParseIdentifier(lexemes, expr)) { return false; }
  return true;
  result = ast::Call::Argument("", std::move(expr));
}

bool ParseCallArgument(absl::Span<Lexeme const> &lexemes,
                       ast::Call::Argument &out) {
  return ParseNamedArgument(lexemes, out) or
         ParsePositionalArgument(lexemes, out);
}

bool ParseYieldStatement(absl::Span<Lexeme const> &lexemes,
                         std::unique_ptr<ast::Node> &n) {
  std::unique_ptr<ast::Label> l;
  std::vector<ast::Call::Argument> exprs;

  absl::Span span = lexemes;
  bool result = Optionally<ParseLabel>(span, l) and
                ParseOperator("<<", span) and
                ParseCommaSeparatedList<ParseCallArgument>(span, exprs);
  if (not result) { return false; }
  n = std::make_unique<ast::YieldStmt>(ExtractRange(lexemes, span),
                                       std::move(exprs), std::move(l));
  return true;
}

bool ParseExpression(absl::Span<Lexeme const> &lexemes,
                     std::unique_ptr<ast::Expression> &e) {
  return ParseParenthesized<ParseExpression>(lexemes, e) or
         ParseFunctionLiteral(lexemes, e) or ParseStructLiteral(lexemes, e) or
         ParseParameterizedStructLiteral(lexemes, e) or
         ParseScopeLiteral(lexemes, e) or ParseUnaryOperator(lexemes, e) or
         ParseBinaryOperator(lexemes, e) or ParseIdentifier(lexemes, e);
}

bool ParseStatement(absl::Span<Lexeme const> &lexemes,
                    std::unique_ptr<ast::Node> &node) {
  if (lexemes.empty()) { return false; }
  auto span = lexemes;

  if (ParseWhileStatement(lexemes, node)) { return true; }
  if (ParseAssignment(lexemes, node)) { return true; }
  if (ParseReturnStatement(lexemes, node)) { return true; }
  if (ParseYieldStatement(lexemes, node)) { return true; }
  if (std::unique_ptr<ast::Expression> e; ParseExpression(lexemes, e)) {
    node = std::move(e);
    return true;
  }

  if (span.empty()) {
    lexemes = span;
    return true;
  } else if (auto k = span[0].kind();
             k == Lexeme::Kind::Newline or k == Lexeme::Kind::Newline) {
    span.remove_prefix(1);
    lexemes = span;
    return true;
  } else {
    return false;
  }
}

}  // namespace

std::unique_ptr<ast::Module> ParseModule(absl::Span<Lexeme const> lexemes) {
  while (not lexemes.empty() and
         lexemes.front().kind() == Lexeme::Kind::Newline) {
    lexemes.remove_prefix(1);
  }
  if (lexemes.empty()) { return nullptr; }

  auto module = std::make_unique<ast::Module>(
      nullptr, std::string_view(lexemes.front().content().begin(),
                                lexemes.back().content().end()));
  std::unique_ptr<ast::Node> stmt;
  while (ParseStatement(lexemes, stmt)) { module->insert(std::move(stmt)); }
  return module;
}

}  // namespace frontend

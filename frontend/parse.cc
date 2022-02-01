#include "frontend/parse.h"

#include <string_view>

#include "absl/types/span.h"
#include "ast/ast.h"
#include "base/debug.h"
#include "ir/value/addr.h"
#include "type/primitive.h"

namespace frontend {
namespace {

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

template <std::invocable<absl::Span<Lexeme const> &> auto P>
std::optional<
    std::vector<decltype(P(std::declval<absl::Span<Lexeme const> &>()))>>
ParseCommaSeparatedList(absl::Span<Lexeme const> &lexemes) {
  auto span = lexemes;

  std::vector<decltype(P(std::declval<absl::Span<Lexeme const> &>()))> result;
  auto n = P(span);
  if (not n) { return std::nullopt; }
  result.push_back(std::move(n));
  while (true) {
    if (span.empty() or span[0].content() != ",") {
      lexemes = span;
      return result;
    }
    span.remove_prefix(1);
    if (not result.emplace_back(P(span))) { return std::nullopt; }
  }
}

std::unique_ptr<ast::Expression> ParseParenthesizedExpression(
    absl::Span<Lexeme const> &lexemes);

// TODO ParseBracedDeclarations
// TODO ParseParenthesizedExpressionList;
// TODO ParseParenthesizedDeclarationList;
// TODO ParseParenthesizedDeclarationIdList
// TODO ParseParameterizedStructLiteral;
// TODO ParseScopeLiteral;
// TODO ParseUnaryOperator;
// TODO ParseBinaryOperator;

std::unique_ptr<ast::Node> ParseStatement(absl::Span<Lexeme const> &lexemes);
std::unique_ptr<ast::Expression> ParseExpression(
    absl::Span<Lexeme const> &lexemes);

std::optional<std::vector<std::unique_ptr<ast::Node>>> ParseBracedStatements(
    absl::Span<Lexeme const> &lexemes) {
  if (lexemes.empty()) { return std::nullopt; }

  if (lexemes.front().content() != "{") { return std::nullopt; }
  size_t offset = lexemes.front().match_offset();
  auto range    = lexemes.subspan(1, offset - 1);
  std::vector<std::unique_ptr<ast::Node>> nodes;
  while (auto stmt = ParseStatement(range)) {
    nodes.push_back(std::move(stmt));
  }
  if (range.empty()) { return nodes; }
  return std::nullopt;
}

std::unique_ptr<ast::Label> ParseLabel(absl::Span<Lexeme const> &lexemes) { return nullptr; }

std::unique_ptr<ast::Expression> ParseParenthesizedExpression(
    absl::Span<Lexeme const> &lexemes) {
  if (lexemes.empty()) { return nullptr; }

  if (lexemes.front().content() != "(") { return nullptr; }
  size_t offset = lexemes.front().match_offset();
  auto range = lexemes.subspan(1, offset - 1);
  if (auto e = ParseExpression(range)) {
    lexemes = lexemes.subspan(offset + 1);
    return e;
  } else {
    return nullptr;
  }
}

std::unique_ptr<ast::Expression> ParseFunctionLiteral(
    absl::Span<Lexeme const> &lexemes) { return nullptr; }
std::unique_ptr<ast::Expression> ParseStructLiteral(
    absl::Span<Lexeme const> &lexemes){ return nullptr; }
std::unique_ptr<ast::Expression> ParseParameterizedStructLiteral(
    absl::Span<Lexeme const> &lexemes){ return nullptr; }

std::unique_ptr<ast::Expression> ParseScopeLiteral(
    absl::Span<Lexeme const> &lexemes){ return nullptr; }

std::unique_ptr<ast::Expression> ParseUnaryOperator(
    absl::Span<Lexeme const> &lexemes) {
  if (lexemes.empty()) { return nullptr; }

  auto span = lexemes;
  if (span[0].kind() != Lexeme::Kind::Operator) { return nullptr; }
  span.remove_prefix(1);
  auto r = ParseExpression(span);
  if (not r) { return nullptr; }
  return nullptr;  // TODO
}

std::unique_ptr<ast::Expression> ParseBinaryOperator(
    absl::Span<Lexeme const> &lexemes) {
  // TODO
  return nullptr;
}

std::unique_ptr<ast::Expression> ParseIdentifier(
    absl::Span<Lexeme const> &lexemes) {
  if (lexemes.empty()) { return nullptr; }
  auto const &lexeme = lexemes[0];
  if (lexeme.kind() != Lexeme::Kind::Identifier) { return nullptr; }

  std::string_view s = lexeme.content();
  if (s == "false") { return std::make_unique<ast::Terminal>(s, false); }
  if (s == "true") { return std::make_unique<ast::Terminal>(s, true); }
  if (s == "null") { return std::make_unique<ast::Terminal>(s, ir::Null()); }
  if (s == "i8") { return std::make_unique<ast::Terminal>(s, type::I8); }
  if (s == "i16") { return std::make_unique<ast::Terminal>(s, type::I16); }
  if (s == "i32") { return std::make_unique<ast::Terminal>(s, type::I32); }
  if (s == "i64") { return std::make_unique<ast::Terminal>(s, type::I64); }
  if (s == "u8") { return std::make_unique<ast::Terminal>(s, type::U8); }
  if (s == "u16") { return std::make_unique<ast::Terminal>(s, type::U16); }
  if (s == "u32") { return std::make_unique<ast::Terminal>(s, type::U32); }
  if (s == "u64") { return std::make_unique<ast::Terminal>(s, type::U64); }
  if (s == "bool") { return std::make_unique<ast::Terminal>(s, type::Bool); }
  if (s == "f32") { return std::make_unique<ast::Terminal>(s, type::F32); }
  if (s == "f64") { return std::make_unique<ast::Terminal>(s, type::F64); }
  if (s == "type") { return std::make_unique<ast::Terminal>(s, type::Type_); }
  if (s == "module") {
    return std::make_unique<ast::Terminal>(s, type::Module);
  }
  if (s == "byte") { return std::make_unique<ast::Terminal>(s, type::Byte); }

  return std::make_unique<ast::Identifier>(lexeme.content());
}

std::unique_ptr<ast::WhileStmt> ParseWhileStatement(
    absl::Span<Lexeme const> &lexemes) {
  if (lexemes.empty()) { return nullptr; }

  if (lexemes[0].content() != "while") { return nullptr; }
  auto span      = lexemes.subspan(1);
  auto condition = ParseParenthesizedExpression(span);
  if (not condition) { return nullptr; }
  auto body = ParseBracedStatements(span);
  if (not body) { return nullptr; }

  return std::make_unique<ast::WhileStmt>(
      ExtractRange(lexemes, span), std::move(condition), *std::move(body));
}

std::unique_ptr<ast::Node> ParseAssignment(absl::Span<Lexeme const> &lexemes) {
  if (lexemes.empty()) { return nullptr; }

  auto span = lexemes;
  auto l    = ParseExpression(span);
  if (not l) { return nullptr; }
  if (span.empty()) { return nullptr; }
  if (span[0].kind() != Lexeme::Kind::Operator) { return nullptr; }
  span.remove_prefix(1);
  auto r = ParseExpression(span);
  if (not r) { return nullptr; }
  return nullptr;  // TODO
}

std::unique_ptr<ast::Node> ParseDeclaration(absl::Span<Lexeme const> &lexemes) {
  return nullptr;  // TODO
}

std::unique_ptr<ast::Node> ParseReturnStatement(
    absl::Span<Lexeme const> &lexemes) {
  if (lexemes.empty()) { return nullptr; }

  if (lexemes[0].content() != "return") { return nullptr; }
  auto span = lexemes;
  span.remove_prefix(1);
  if (auto exprs = ParseCommaSeparatedList<ParseExpression>(span)) {
    return std::make_unique<ast::ReturnStmt>(ExtractRange(lexemes, span),
                                             *std::move(exprs));
  } else {
    return nullptr;
  }
}

std::unique_ptr<ast::Node> ParseYieldStatement(
    absl::Span<Lexeme const> &lexemes) {
  auto span = lexemes;
  auto l    = ParseLabel(span);
  if (span.empty()) { return nullptr; }
  if (span[0].content() != "<<") { return nullptr; }

  auto exprs = ParseCommaSeparatedList<ParseExpression>(span);
  // TODO: Parse CallArguments
  return nullptr;
  // return std::make_unique<ast::YieldStmt>(ExtractRange(lexemes, span),
  //                                         std::move(exprs), std::move(l));
}

std::unique_ptr<ast::Expression> ParseExpression(
    absl::Span<Lexeme const> &lexemes) {
  std::unique_ptr<ast::Expression> e;
  ((e = ParseParenthesizedExpression(lexemes)) or
   (e = ParseFunctionLiteral(lexemes)) or (e = ParseStructLiteral(lexemes)) or
   (e = ParseParameterizedStructLiteral(lexemes)) or
   (e = ParseScopeLiteral(lexemes)) or (e = ParseUnaryOperator(lexemes)) or
   (e = ParseBinaryOperator(lexemes)) or (e = ParseIdentifier(lexemes)));
  return e;
}

std::unique_ptr<ast::Node> ParseStatement(absl::Span<Lexeme const> &lexemes) {
  auto span = lexemes;

  std::unique_ptr<ast::Node> e;
  ((e = ParseWhileStatement(span)) or (e = ParseAssignment(span)) or
   (e = ParseDeclaration(span)) or (e = ParseReturnStatement(span)) or
   (e = ParseYieldStatement(span)) or (e = ParseExpression(span)));

  if (span.empty()) {
    lexemes = span;
    return e;
  } else if (auto k = span[0].kind();
             k == Lexeme::Kind::Newline or k == Lexeme::Kind::Newline) {
    span.remove_prefix(1);
    lexemes = span;
    return e;
  } else {
    return nullptr;
  }
}

}  // namespace

std::unique_ptr<ast::Module> ParseModule(absl::Span<Lexeme const> lexemes) {
  auto module = std::make_unique<ast::Module>(
      nullptr, std::string_view(lexemes.front().content().begin(),
                                lexemes.back().content().end()));
  while (auto stmt = ParseStatement(lexemes)) {
    module->insert(std::move(stmt));
  }
  return module;
}

}  // namespace frontend

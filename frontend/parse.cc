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

// TODO: Out of sheer laziness, I haven't wired this through yet.
SourceBuffer const *src;

template <typename NodeType, auto... Ps>
auto FirstOf(absl::Span<Lexeme const> &lexemes) {
  std::unique_ptr<NodeType> e;
  absl::Span<Lexeme const> range;
  if (((e = Ps(range = lexemes)) or ...)) { lexemes = range; }
  return e;
}

template <std::invocable<absl::Span<Lexeme const> &> P>
std::optional<
    std::vector<decltype(P(std::declval<absl::Span<Lexeme const> &>()))>>
CommaSeparatedListOf(absl::Span<Lexeme const> &lexemes) {
  auto range = lexemes;

  std::vector<decltype(P(std::declval<absl::Span<Lexeme const> &>()))> result;
  auto n = P(range);
  if (not n) { return std::nullopt; }
  result.push_back(std::move(n));
  while () {}
}

// TODO TryParseBracedDeclarations
// TODO TryParseParenthesizedExpressionList;
// TODO TryParseParenthesizedDeclarationList;
// TODO TryParseParenthesizedDeclarationIdList
// TODO TryParseParameterizedStructLiteral;
// TODO TryParseScopeLiteral;
// TODO TryParseUnaryOperator;
// TODO TryParseBinaryOperator;

std::unique_ptr<ast::Node> TryParseStatement(absl::Span<Lexeme const> &lexemes);
std::unique_ptr<ast::Expression> TryParseExpression(
    absl::Span<Lexeme const> &lexemes);

std::unique_ptr<ast::Expression> TryParseIdentifier(
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

  // TODO: Reserved keywords should return nullptr.

  return std::make_unique<ast::Identifier>(lexeme.content());
}

std::unique_ptr<ast::Expression> TryParseDeclaration(
    absl::Span<Lexeme const> &lexemes);

std::unique_ptr<ast::Expression> TryParseParenthesizedExpression(
    absl::Span<Lexeme const> &lexemes) {
  if (lexemes.empty()) { return nullptr; }

  if (lexemes.front().content() != "(") { return nullptr; }
  size_t offset = lexemes.front().match_offset();
  auto range = lexemes.subspan(1, offset - 1);
  if (auto e = TryParseExpression(range)) {
    lexemes = lexemes.subspan(offset + 1);
    return e;
  } else {
    return nullptr;
  }
}

std::optional<std::vector<std::unique_ptr<ast::Node>>> TryParseBracedStatements(
    absl::Span<Lexeme const> &lexemes) {
  if (lexemes.empty()) { return std::nullopt; }

  if (lexemes.front().content() != "{") { return std::nullopt; }
  size_t offset = lexemes.front().match_offset();
  auto range    = lexemes.subspan(1, offset - 1);
  std::vector<std::unique_ptr<ast::Node>> nodes;
  while (auto stmt = TryParseStatement(range)) {
    nodes.push_back(std::move(stmt));
  }
  if (range.empty()) { return nodes; }
  return std::nullopt;
}

#if 0
std::unique_ptr<ast::FunctionLiteral> TryParseFunctionLiteral(
    absl::Span<Lexeme const> &lexemes) {
  auto range = lexemes;
  auto decls = TryParseParenthesizedDeclarationList(range);
  if (range.front().content() != "->") { return nullptr; }
  range.consume_prefix(1);

  std::vector<std::unique_ptr<ast::Expression>> outs;
  if (outs = TryParseParenthesizedExpressionList()) {
  } else if (auto e = TryParseExpression()) {
    outs.push_back(std::move(e));
  }

  auto body = TryParseBracedStatements(range);
  if (not body) { return nullptr; }

  return std::make_unique<ast::FunctionLiteral>(
      ExtractRange(lexemes, range), std::move(decls), std::move(body),
      std::move(outs));
}

std::unique_ptr<ast::StructLiteral> TryParseStructLiteral(
    absl::Span<Lexeme const> &lexemes) {
  if (lexemes[0].content() != "struct") { return nullptr; }
  auto range = lexemes.subspan(1);
  auto decls = TryParseBracedDeclarations(range);
  if (not decls) { return nullptr; }
  return std::make_unique<ast::StructLiteral>(ExtractRange(lexemes, range),
                                              *std::move(decls));
}
#endif

std::unique_ptr<ast::IfStmt> TryParseIfStatement(
    absl::Span<Lexeme const> &lexemes) {
  if (lexemes.empty()) { return nullptr; }

  if (lexemes[0].content() != "if") { return nullptr; }
  auto range     = lexemes.subspan(1);
  auto condition = TryParseParenthesizedExpression(range);
  if (not condition) { return nullptr; }
  auto true_body = TryParseBracedStatements(range);
  if (not true_body) { return nullptr; }

  if (range[0].content() == "else") {
    auto else_range = range.subspan(1);
    auto false_body = TryParseBracedStatements(else_range);
    if (not false_body) { return nullptr; }
    return std::make_unique<ast::IfStmt>(
        ExtractRange(lexemes, else_range), std::move(condition),
        *std::move(true_body), *std::move(false_body));
  } else {
    return std::make_unique<ast::IfStmt>(ExtractRange(lexemes, range),
                                         std::move(condition),
                                         *std::move(true_body));
  }
}

std::unique_ptr<ast::WhileStmt> TryParseWhileStatement(
    absl::Span<Lexeme const> &lexemes) {
  if (lexemes.empty()) { return nullptr; }

  if (lexemes[0].content() != "while") { return nullptr; }
  auto range     = lexemes.subspan(1);
  auto condition = TryParseParenthesizedExpression(range);
  if (not condition) { return nullptr; }
  auto body = TryParseBracedStatements(range);
  if (not body) { return nullptr; }

  return std::make_unique<ast::WhileStmt>(
      ExtractRange(lexemes, range), std::move(condition), *std::move(body));
}

std::unique_ptr<ast::Expression> TryParseExpression(
    absl::Span<Lexeme const> &lexemes) {
  return FirstOf<ast::Expression,
                 TryParseParenthesizedExpression, /* TryParseFunctionLiteral,
 TryParseStructLiteral, TryParseParameterizedStructLiteral,
 TryParseScopeLiteral, TryParseUnaryOperator,
 TryParseBinaryOperator, */
                 TryParseIdentifier>(lexemes);
}

std::unique_ptr<ast::Node> TryParseStatement(
    absl::Span<Lexeme const> &lexemes) {
  return FirstOf<ast::Node, TryParseIfStatement, TryParseWhileStatement>(
      lexemes);
}

}  // namespace

std::unique_ptr<ast::Module> ParseModule(absl::Span<Lexeme const> lexemes) {
  auto module = std::make_unique<ast::Module>(nullptr);
  while (auto stmt = TryParseStatement(lexemes)) {
    module->insert(std::move(stmt));
  }
  return module;
}

}  // namespace frontend

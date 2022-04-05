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

template <typename T, auto P, typename... Args>
bool ParseSequence(absl::Span<core::Lexeme const> &lexemes, std::vector<T> &out,
                   Args &&... args) {
  PARSE_DEBUG_LOG();
  T t;
  while (not lexemes.empty() and P(lexemes, t, std::forward<Args>(args)...)) {
    out.push_back(std::move(t));
  }
  return true;
}

enum Precedence {
  kTimesDiv,
  kPlusMinus,
  kMaxPrecedence,
};

[[maybe_unused]] Precedence GetPrecedence(std::string_view op) {
  if (op == "+") { return kPlusMinus; }
  if (op == "-") { return kPlusMinus; }
  if (op == "*") { return kTimesDiv; }
  if (op == "/") { return kTimesDiv; }
  return kMaxPrecedence;
}

}  // namespace

bool TerminalOrIdentifier::Parse(absl::Span<core::Lexeme const> &lexemes,
                                 std::string_view &consumed,
                                 std::unique_ptr<ast::Expression> &out) {
  PARSE_DEBUG_LOG();
  if (lexemes.empty()) { return false; }
  auto const &lexeme = lexemes[0];
  if (lexeme.kind() != core::Lexeme::Kind::Identifier and
      lexeme.kind() != core::Lexeme::Kind::Reserved) {
    return false;
  }

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

namespace {
bool ParseOperatorOrAtomicExpression(
    absl::Span<core::Lexeme const> &lexemes,
    std::variant<std::string_view, std::unique_ptr<ast::Expression>> &out) {
  PARSE_DEBUG_LOG();
  std::unique_ptr<ast::Expression> e;
  absl::Span span = lexemes;
  std::string_view consumed;
  if (core::Parse(AtomicExpression(), span, consumed, e)) {
    lexemes = span;
    out     = std::move(e);
    return true;
  } else if (lexemes.empty()) {
    return false;
  } else if ((lexemes[0].kind() == core::Lexeme::Kind::Operator and
              lexemes[0].content() != "<<") or
             (lexemes[0].kind() == core::Lexeme::Kind::Identifier and
              (lexemes[0].content() == "import"))) {
    out = lexemes[0].content();
    lexemes.remove_prefix(1);
    return true;
  } else {
    return false;
  }
}
}  // namespace

bool Expression::Parse(absl::Span<core::Lexeme const> &lexemes,
                       std::string_view &consumed,
                       std::unique_ptr<ast::Expression> &e) {
  PARSE_DEBUG_LOG();
  auto span  = lexemes;
  using type = std::variant<std::string_view, std::unique_ptr<ast::Expression>>;
  std::vector<type> elements;
  (void)ParseSequence<type, ParseOperatorOrAtomicExpression>(span, elements);
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

// bool Statement::Impl::Parse(absl::Span<core::Lexeme const> &lexemes,
//                             std::string_view &consumed,
//                             std::unique_ptr<ast::Node> &node) {
//   PARSE_DEBUG_LOG();
//   absl::Cleanup cl = [&] {
//     if (node) { LOG("", "%s", node->DebugString()); }
//   };
//   auto iter = std::find_if(lexemes.begin(), lexemes.end(), [](core::Lexeme const &l) {
//     return l.kind() != core::Lexeme::Kind::Newline;
//   });
//   if (iter == lexemes.end()) { return false; }
//   absl::Span span = lexemes = absl::MakeConstSpan(iter, lexemes.end());
// 
//   constexpr auto Statement = (                                          //
//       WhileStatement                                                    //
//       | Assignment                                                      //
//       | (Declaration << Bind(MakeUnique<ast::Declaration, ast::Node>))  //
//       | ReturnStatement                                                 //
//       | YieldStatement                                                  //
//       | (Expression << Bind([](std::unique_ptr<ast::Expression> e)
//                                 -> std::unique_ptr<ast::Node> { return e; })));
//   return Statement.Parse(lexemes, consumed, node);
// }

std::optional<ast::Module> ParseModule(absl::Span<core::Lexeme const> lexemes,
                                       diagnostic::DiagnosticConsumer &) {
  // PARSE_DEBUG_LOG();
  // while (not lexemes.empty() and
  //        (lexemes.front().kind() == core::Lexeme::Kind::Newline or
  //         lexemes.front().kind() == core::Lexeme::Kind::EndOfFile)) {
  //   lexemes.remove_prefix(1);
  // }
  // if (lexemes.empty()) { return std::nullopt; }

  // auto module = std::make_optional<ast::Module>(std::string_view(
  //     lexemes.front().content().begin(), lexemes.back().content().end()));
  // std::unique_ptr<ast::Node> stmt;
  // while (ParseStatement(lexemes, stmt)) { module->insert(std::move(stmt)); }
  // while (not lexemes.empty() and
  //        (lexemes.front().kind() == core::Lexeme::Kind::Newline or
  //         lexemes.front().kind() == core::Lexeme::Kind::EndOfFile)) {
  //   lexemes.remove_prefix(1);
  // }

  // if (not lexemes.empty()) { return std::nullopt; }
  // return module;
  return std::nullopt;
}

}  // namespace frontend

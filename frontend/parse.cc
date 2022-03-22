#include "frontend/parse.h"

#include <concepts>
#include <string_view>

#include "absl/cleanup/cleanup.h"
#include "absl/types/span.h"
#include "ast/ast.h"
#include "base/debug.h"
#include "frontend/parser_dsl.h"
#include "ir/value/addr.h"
#include "type/primitive.h"

namespace frontend {
bool ParseTerminalOrIdentifier(absl::Span<Lexeme const> &lexemes,
                               std::unique_ptr<ast::Expression> &out);
bool ParseDeclaration(absl::Span<Lexeme const> &lexemes, ast::Declaration &out);
bool ParseStatement(absl::Span<Lexeme const> &lexemes,
                    std::unique_ptr<ast::Node> &);
bool ParseExpression(absl::Span<Lexeme const> &lexemes,
                     std::unique_ptr<ast::Expression> &out);

namespace {

template <typename T>
constexpr auto Construct = []<typename... Args>(Args &&... args) {
  return T(std::forward<Args>(args)...);
};
template <typename T, typename Out = T>
constexpr auto MakeUnique =
    []<typename... Args>(Args &&... args)
        -> std::unique_ptr<Out> requires(std::constructible_from<T, Args...>) {
  return std::make_unique<T>(std::forward<Args>(args)...);
};
template <typename T>
constexpr auto Vector = []<typename Arg>(Arg &&arg) {
  std::vector<T> v;
  v.push_back(std::forward<Arg>(arg));
  return v;
};

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

template <typename T, auto P>
struct Wrap {
  using match_type = base::type_list<T>;

  static bool Parse(absl::Span<Lexeme const> &lexemes,
                    std::string_view &consumed, auto &&out) {
    return P(lexemes, out);
  }
};

constexpr auto Number        = Kind<Lexeme::Kind::Number>;
constexpr auto StringLiteral = Kind<Lexeme::Kind::String>;
constexpr auto AnOperator    = Kind<Lexeme::Kind::Operator>;
constexpr auto Identifier    = Kind<Lexeme::Kind::Identifier>;

constexpr auto DeclarationId = (Identifier | Parenthesized(AnOperator))
                               << Bind(Construct<ast::Declaration::Id>);

constexpr auto Label = (--Kind<Lexeme::Kind::Hash> + Match<"."> + Identifier)
                       << Bind(Construct<std::optional<ast::Label>>);

constexpr auto Expression =
    Wrap<std::unique_ptr<ast::Expression>, ParseExpression>{};

constexpr auto NamedArgument = (Identifier + Match<"="> + Expression)
                               << Bind(Construct<ast::Call::Argument>);

constexpr auto PositionalArgument =
    Expression << Bind([](std::unique_ptr<ast::Expression> e) {
      return ast::Call::Argument("", std::move(e));
    });
constexpr auto CallArgument = NamedArgument | PositionalArgument;

constexpr auto Statement   = Wrap<std::unique_ptr<ast::Node>, ParseStatement>{};
constexpr auto Declaration = Wrap<ast::Declaration, ParseDeclaration>{};

constexpr auto ReturnStatement =
    (Match<"return"> + CommaSeparatedListOf(Expression))
    << BindWithRange(MakeUnique<ast::ReturnStmt, ast::Node>);

constexpr auto YieldStatement =
    (Optional(Label) + Match<"<<"> + CommaSeparatedListOf(CallArgument))
    << BindWithRange(MakeUnique<ast::YieldStmt, ast::Node>);

constexpr auto WhileStatement =
    Match<"while"> + Parenthesized(Expression) +
        Braced(NewlineSeparatedListOf(Statement))
    << BindWithRange(MakeUnique<ast::WhileStmt, ast::Node>);

constexpr auto ExpressionOrExpressionList =
    (Parenthesized(CommaSeparatedListOf(Expression)) |
     Expression << Bind(Vector<std::unique_ptr<ast::Expression>>));
constexpr auto Assignment =
    (ExpressionOrExpressionList + --Kind<Lexeme::Kind::Assignment> +
     ExpressionOrExpressionList)
    << BindWithRange(MakeUnique<ast::Assignment, ast::Node>);

constexpr auto StructImpl =
    Braced(NewlineSeparatedListOf(Declaration))
    << BindWithRange(MakeUnique<ast::StructLiteral, ast::Expression>);
constexpr auto ParameterizedStructImpl =
    (Parenthesized(CommaSeparatedListOf(Declaration)) +
     Braced(NewlineSeparatedListOf(Declaration)))
    << BindWithRange(
           MakeUnique<ast::ParameterizedStructLiteral, ast::Expression>);
constexpr auto StructLiteral =
    Match<"struct"> + (StructImpl | ParameterizedStructImpl);

constexpr auto ScopeLiteral =
    (Match<"scope"> +
     Bracketed(Identifier << Bind(Construct<ast::Declaration::Id>)) +
     Parenthesized(CommaSeparatedListOf(Declaration)) +
     Braced(NewlineSeparatedListOf(Statement)))
    << BindWithRange(MakeUnique<ast::ScopeLiteral, ast::Expression>);

constexpr auto FunctionLiteral =
    (Parenthesized(CommaSeparatedListOf(Declaration)) + Match<"->"> +
     Optional(Parenthesized(CommaSeparatedListOf(Expression))) +
     Braced(NewlineSeparatedListOf(Statement)))
    << BindWithRange(MakeUnique<ast::FunctionLiteral, ast::Expression>);

constexpr auto TerminalOrIdentifier =
    Wrap<std::unique_ptr<ast::Expression>, ParseTerminalOrIdentifier>{};

constexpr auto AsExpression =
    [](std::string_view content) -> std::unique_ptr<ast::Expression> {
  return std::make_unique<ast::Terminal>(content, "");
};

constexpr auto AtomicExpression =
    ((Number | StringLiteral) << Bind(AsExpression))  //
    | Parenthesized(Expression)                       //
    | FunctionLiteral                                 //
    | StructLiteral                                   //
    | ScopeLiteral                                    //
    | TerminalOrIdentifier;                           //

bool ParseOperatorOrAtomicExpression(
    absl::Span<Lexeme const> &lexemes,
    std::variant<std::string_view, std::unique_ptr<ast::Expression>> &out);

}  // namespace

// TODO: Not tested.
bool ParseExpression(absl::Span<Lexeme const> &lexemes,
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

bool ParseDeclarationId(absl::Span<Lexeme const> &lexemes,
                        ast::Declaration::Id &out) {
  std::string_view consumed;
  return DeclarationId.Parse(lexemes, consumed, out);
}

bool ParseLabel(absl::Span<Lexeme const> &lexemes,
                std::optional<ast::Label> &out) {
  std::string_view consumed;
  return Label.Parse(lexemes, consumed, out);
}

bool ParseStringLiteral(absl::Span<Lexeme const> &lexemes,
                        std::unique_ptr<ast::Expression> &out) {
  std::string_view consumed;
  return (StringLiteral << Bind(AsExpression)).Parse(lexemes, consumed, out);
}

bool ParseCallArgument(absl::Span<Lexeme const> &lexemes,
                       ast::Call::Argument &out) {
  std::string_view consumed;
  return CallArgument.Parse(lexemes, consumed, out);
}

bool ParseDeclaration(absl::Span<Lexeme const> &lexemes,
                      ast::Declaration &out) {
  PARSE_DEBUG_LOG();
  auto span = lexemes;
  std::vector<ast::Declaration::Id> ids;
  std::string_view consumed;

  constexpr auto DeclarationStart =
      (Parenthesized(CommaSeparatedListOf(DeclarationId)) |
       DeclarationId << Bind(Vector<ast::Declaration::Id>));
  if (not DeclarationStart.Parse(span, consumed, ids)) { return false; }

  std::unique_ptr<ast::Expression> type_expression;
  std::unique_ptr<ast::Expression> initial_value;
  if (span.empty()) { return false; }
  if (span[0].kind() != Lexeme::Kind::Operator) { return false; }
  ast::Declaration::Flags flags{};
  if (span[0].content() == ":" or span[0].content() == "::") {
    if (span[0].content().size() == 2) { flags |= ast::Declaration::f_IsConst; }
    span.remove_prefix(1);
    if (not Expression.Parse(span, consumed, type_expression)) { return false; }
    std::ignore = Match<"=">.Parse(span, consumed) and
                  Expression.Parse(span, consumed, initial_value);
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
  std::string_view consumed;
  return Assignment.Parse(lexemes, consumed, out);
}

// TODO: Everything above this line is tested.

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

bool ParseAtomicExpression(absl::Span<Lexeme const> &lexemes,
                           std::unique_ptr<ast::Expression> &e) {
  std::string_view consumed;
  return AtomicExpression.Parse(lexemes, consumed, e);
}

namespace {
bool ParseOperatorOrAtomicExpression(
    absl::Span<Lexeme const> &lexemes,
    std::variant<std::string_view, std::unique_ptr<ast::Expression>> &out) {
  PARSE_DEBUG_LOG();
  std::unique_ptr<ast::Expression> e;
  absl::Span span = lexemes;
  std::string_view consumed;
  if (AtomicExpression.Parse(span, consumed, e)) {
    lexemes = span;
    out     = std::move(e);
    return true;
  } else if (lexemes.empty()) {
    return false;
  } else if ((lexemes[0].kind() == Lexeme::Kind::Operator and
              lexemes[0].content() != "<<") or
             (lexemes[0].kind() == Lexeme::Kind::Identifier and
              (lexemes[0].content() == "import"))) {
    out = lexemes[0].content();
    lexemes.remove_prefix(1);
    return true;
  } else {
    return false;
  }
}
}  // namespace

bool ParseReturnStatement(absl::Span<Lexeme const> &lexemes,
                          std::unique_ptr<ast::Node> &out) {
  std::string_view consumed;
  return ReturnStatement.Parse(lexemes, consumed, out);
}
bool ParseYieldStatement(absl::Span<Lexeme const> &lexemes,
                         std::unique_ptr<ast::Node> &out) {
  std::string_view consumed;
  return YieldStatement.Parse(lexemes, consumed, out);
}

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

  constexpr auto Statement = (                                          //
      WhileStatement                                                    //
      | Assignment                                                      //
      | (Declaration << Bind(MakeUnique<ast::Declaration, ast::Node>))  //
      | ReturnStatement                                                 //
      | YieldStatement                                                  //
      | (Expression << Bind([](std::unique_ptr<ast::Expression> e)
                                -> std::unique_ptr<ast::Node> { return e; })));
  std::string_view consumed;
  return Statement.Parse(lexemes, consumed, node);
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

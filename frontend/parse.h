#ifndef ICARUS_FRONTEND_PARSE_H
#define ICARUS_FRONTEND_PARSE_H

#include <memory>

#include "absl/types/span.h"
#include "ast/ast.h"
#include "ast/module.h"
#include "diagnostic/consumer/consumer.h"
#include "frontend/lexeme.h"
#include "frontend/parser_dsl.h"

namespace frontend {

bool ParseTerminalOrIdentifier(absl::Span<Lexeme const> &lexemes,
                               std::unique_ptr<ast::Expression> &out);
bool ParseStatement(absl::Span<Lexeme const> &lexemes,
                    std::unique_ptr<ast::Node> &);
bool ParseExpression(absl::Span<Lexeme const> &lexemes,
                     std::unique_ptr<ast::Expression> &out);

template <typename T>
inline constexpr auto Construct = []<typename... Args>(Args &&... args) {
  return T(std::forward<Args>(args)...);
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


template <typename T, auto P>
struct Wrap {
  using match_type = base::type_list<T>;

  static bool Parse(absl::Span<Lexeme const> &lexemes,
                    std::string_view &consumed, auto &&out) {
    return P(lexemes, out);
  }
};

inline constexpr auto Number        = Kind<Lexeme::Kind::Number>;
inline constexpr auto StringLiteral = Kind<Lexeme::Kind::String>;
inline constexpr auto AnOperator    = Kind<Lexeme::Kind::Operator>;
inline constexpr auto Identifier    = Kind<Lexeme::Kind::Identifier>;

inline constexpr auto DeclarationId = (Identifier | Parenthesized(AnOperator))
                                      << Bind(Construct<ast::Declaration::Id>);

inline constexpr auto Label =
    (--Kind<Lexeme::Kind::Hash> + --Match<"."> + Identifier)
    << Bind(Construct<std::optional<ast::Label>>);

inline constexpr auto Expression =
    Wrap<std::unique_ptr<ast::Expression>, ParseExpression>{};

inline constexpr auto PositionalArgument =
    Expression << Bind([](std::unique_ptr<ast::Expression> e) {
      return ast::Call::Argument("", std::move(e));
    });
inline constexpr auto NamedArgument = (Identifier + --Match<"="> + Expression)
                                      << Bind(Construct<ast::Call::Argument>);

inline constexpr auto CallArgument = NamedArgument | PositionalArgument;

inline constexpr auto Statement =
    Wrap<std::unique_ptr<ast::Node>, ParseStatement>{};

inline constexpr auto DeclarationStart =
    (Parenthesized(CommaSeparatedListOf(DeclarationId)) |
     DeclarationId << Bind(Vector<ast::Declaration::Id>));
inline constexpr auto NonDeducedDeclaration =
    (DeclarationStart + NonDeducedDeclarationMarker + Expression +
     Optional(--Match<"="> + Expression))
    << BindWithRange([](std::string_view range, ast::Declaration::Flags flags,
                        std::vector<ast::Declaration::id> ids,
                        std::unique_ptr<ast::Expression> type_expression,
                        std::unique_ptr<ast::Expression> initial_value) {
         // TODO: Make this Construct.
         return ast::Declaration(range, std::move(ids),
                                 std::move(type_expression),
                                 std::move(initial_value), flags);
       });
inline constexpr auto DeducedDeclaration =
    (DeclarationStart + DeducedDeclarationMarker + Expression)
    << BindWithRange([](std::string_view range, ast::Declaration::Flags flags,
                        std::vector<ast::Declaration::id> ids,
                        std::unique_ptr<ast::Expression> initial_value) {
         // TODO: Make this Construct.
         return ast::Declaration(range, std::move(ids), nullptr,
                                 std::move(initial_value), flags);
       });

inline constexpr auto Declaration = NonDeducedDeclaration | DeducedDeclaration;
inline constexpr auto ReturnStatement =
    (--Match<"return"> + CommaSeparatedListOf(Expression))
    << BindWithRange(MakeUnique<ast::ReturnStmt, ast::Node>);

inline constexpr auto YieldStatement =
    (Optional(Label) + --Match<"<<"> + CommaSeparatedListOf(CallArgument))
    << BindWithRange(MakeUnique<ast::YieldStmt, ast::Node>);

inline constexpr auto WhileStatement =
    --Match<"while"> + Parenthesized(Expression) +
        Braced(NewlineSeparatedListOf(Statement))
    << BindWithRange(MakeUnique<ast::WhileStmt, ast::Node>);

inline constexpr auto ExpressionOrExpressionList =
    (Parenthesized(CommaSeparatedListOf(Expression)) |
     Expression << Bind(Vector<std::unique_ptr<ast::Expression>>));
inline constexpr auto Assignment =
    (ExpressionOrExpressionList + --Kind<Lexeme::Kind::Assignment> +
     ExpressionOrExpressionList)
    << BindWithRange(MakeUnique<ast::Assignment, ast::Node>);

inline constexpr auto StructImpl =
    Braced(NewlineSeparatedListOf(Declaration))
    << BindWithRange(MakeUnique<ast::StructLiteral, ast::Expression>);
inline constexpr auto ParameterizedStructImpl =
    (Parenthesized(CommaSeparatedListOf(Declaration)) +
     Braced(NewlineSeparatedListOf(Declaration)))
    << BindWithRange(
           MakeUnique<ast::ParameterizedStructLiteral, ast::Expression>);
inline constexpr auto StructLiteral =
    --Match<"struct"> + (StructImpl | ParameterizedStructImpl);

inline constexpr auto ScopeLiteral =
    (--Match<"scope"> +
     Bracketed(Identifier << Bind(Construct<ast::Declaration::Id>)) +
     Parenthesized(CommaSeparatedListOf(Declaration)) +
     Braced(NewlineSeparatedListOf(Statement)))
    << BindWithRange(MakeUnique<ast::ScopeLiteral, ast::Expression>);

inline constexpr auto FunctionLiteral =
    (Parenthesized(CommaSeparatedListOf(Declaration)) + --Match<"->"> +
     Optional(Parenthesized(CommaSeparatedListOf(Expression))) +
     Braced(NewlineSeparatedListOf(Statement)))
    << BindWithRange(MakeUnique<ast::FunctionLiteral, ast::Expression>);

inline constexpr auto TerminalOrIdentifier =
    Wrap<std::unique_ptr<ast::Expression>, ParseTerminalOrIdentifier>{};

inline constexpr auto AsExpression =
    [](std::string_view content) -> std::unique_ptr<ast::Expression> {
  return std::make_unique<ast::Terminal>(content, "");
};

inline constexpr auto AtomicExpression =
    ((Number | StringLiteral) << Bind(AsExpression))  //
    | Parenthesized(Expression)                       //
    | FunctionLiteral                                 //
    | StructLiteral                                   //
    | ScopeLiteral                                    //
    | TerminalOrIdentifier;                           //

std::optional<ast::Module> ParseModule(
    absl::Span<Lexeme const> lexemes, diagnostic::DiagnosticConsumer& consumer);

}  // namespace frontend

#endif  // ICARUS_FRONTEND_PARSE_H

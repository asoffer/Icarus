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

struct Expression {
 private:
  struct Impl {
    using match_type = base::type_list<std::unique_ptr<ast::Expression>>;

    static bool Parse(absl::Span<Lexeme const> &lexemes,
                      std::string_view &consumed, auto &&out);
  };

 public:
  static constexpr auto parser = Impl();
};

struct Statement {
 private:
  struct Impl {
    using match_type = base::type_list<std::unique_ptr<ast::Expression>>;

    static bool Parse(absl::Span<Lexeme const> &lexemes,
                      std::string_view &consumed, auto &&out);
  };

 public:
  static constexpr auto parser = Impl();
};

struct Number : Kind<Lexeme::Kind::Number> {};
struct StringLiteral : Kind<Lexeme::Kind::String> {};
struct AnOperator : Kind<Lexeme::Kind::Operator> {};
struct Identifier : Kind<Lexeme::Kind::Identifier> {};

struct ArrayLiteral {
  static constexpr auto parser =
      Bracketed(CommaSeparatedListOf(Expression()))
      << BindWithRange(MakeUnique<ast::ArrayLiteral, ast::Expression>);
};

struct ArrayType {
  static constexpr auto parser =
      Bracketed(CommaSeparatedListOf<1>(Expression()) +
                ~Kind<Lexeme::Kind::Semicolon>() + Expression())
      << BindWithRange(MakeUnique<ast::ArrayType, ast::Expression>);
};

struct DeclarationId {
  static constexpr auto parser = (Identifier() | Parenthesized(AnOperator()))
                                 << Bind(Construct<ast::Declaration::Id>);
};

struct Label {
  static constexpr auto parser =
      (~Kind<Lexeme::Kind::Hash>() + ~Match<".">() + Identifier())
      << Bind(Construct<std::optional<ast::Label>>);
};

struct CallArgument {
 private:
  struct Positional {
    static constexpr auto parser =
        Expression() << Bind([](std::unique_ptr<ast::Expression> e) {
          return ast::Call::Argument("", std::move(e));
        });
  };
  struct Named {
    static constexpr auto parser = (Identifier() + ~Match<"=">() + Expression())
                                   << Bind(Construct<ast::Call::Argument>);
  };

 public:
  static constexpr auto parser = Named() | Positional();
};

struct Declaration {
 private:
  struct Start {
    static constexpr auto parser =
        (Parenthesized(CommaSeparatedListOf(DeclarationId()) | DeclarationId())
         << Bind(Vector<ast::Declaration::Id>));
  };

  struct InferredType {
   private:
    struct InferenceMarker {
      static constexpr auto parser =
          (Match<":=">() | Match<"::=">())
          << Bind([](Lexeme const &l) -> ast::Declaration::Flags {
               if (l.content().size() == 3) {
                 return ast::Declaration::f_IsConst;
               }
               return ast::Declaration::Flags{};
             });
    };

   public:
    static constexpr auto parser =
        (Start() + InferenceMarker() + Expression() +
         Optional(~Match<"=">() + Expression()))
        << BindWithRange(Construct<ast::Declaration>);
  };

  struct ExplicitType {
   private:
    struct Marker {
      static constexpr auto parser =
          (Match<":">() | Match<"::">())
          << Bind([](Lexeme const &l) -> ast::Declaration::Flags {
               if (l.content().size() == 2) {
                 return ast::Declaration::f_IsConst;
               }
               return ast::Declaration::Flags{};
             });
    };

   public:
    static constexpr auto parser =
        (Start() + Marker() + Expression())
        << BindWithRange(Construct<ast::Declaration>);
  };

 public:
  static constexpr auto parser = InferredType() | ExplicitType();
};

struct ReturnStatement {
  static constexpr auto parser =
      (~Match<"return">() + CommaSeparatedListOf(Expression()))
      << BindWithRange(MakeUnique<ast::ReturnStmt, ast::Node>);
};

struct YieldStatement {
  static constexpr auto parser =
      (Optional(Label) + ~Match<"<<">() + CommaSeparatedListOf(CallArgument()))
      << BindWithRange(MakeUnique<ast::YieldStmt, ast::Node>);
};

struct WhileStatement {
  static constexpr auto parser =
      ~Match<"while">() + Parenthesized(Expression()) +
          Braced(NewlineSeparatedListOf(Statement()))
      << BindWithRange(MakeUnique<ast::WhileStmt, ast::Node>);
};

struct ExpressionOrExpressionList {
  static constexpr auto parser =
      (Parenthesized(CommaSeparatedListOf(Expression())) |
       Expression << Bind(Vector<std::unique_ptr<ast::Expression>>));
};
struct Assignment {
  static constexpr auto parser =
      (ExpressionOrExpressionList() + ~Kind<Lexeme::Kind::Assignment>() +
       ExpressionOrExpressionList())
      << BindWithRange(MakeUnique<ast::Assignment, ast::Node>);
};

struct StructLiteral {
  static constexpr auto parser =
      ~Match<"struct">() + (Impl() | ParameterizedImpl());

 private:
  struct Impl {
    static constexpr auto parser =
        Braced(NewlineSeparatedListOf(Declaration()))
        << BindWithRange(MakeUnique<ast::StructLiteral, ast::Expression>);
  };

  struct ParameterizedImpl {
    static constexpr auto parser =
        (Parenthesized(CommaSeparatedListOf(Declaration())) +
         Braced(NewlineSeparatedListOf(Declaration())))
        << BindWithRange(
               MakeUnique<ast::ParameterizedStructLiteral, ast::Expression>);
  };
};

struct ScopeLiteral {
  static constexpr auto parser =
      (~Match<"scope">() +
       Bracketed(Identifier() << Bind(Construct<ast::Declaration::Id>)) +
       Parenthesized(CommaSeparatedListOf(Declaration())) +
       Braced(NewlineSeparatedListOf(Statement())))
      << BindWithRange(MakeUnique<ast::ScopeLiteral, ast::Expression>);
};

struct FunctionLiteral {
  static constexpr auto parser =
      (Parenthesized(CommaSeparatedListOf(Declaration())) + ~Match<"->">() +
       Optional(Parenthesized(CommaSeparatedListOf(Expression()))) +
       Braced(NewlineSeparatedListOf(Statement())))
      << BindWithRange(MakeUnique<ast::FunctionLiteral, ast::Expression>);
};

struct TerminalOrIdentifier {
  static constexpr auto parser = Impl();

 private:
  struct Impl {
    static bool Parse(absl::Span<Lexeme const> &lexemes,
                      std::string_view &consumed, auto &&out);
  };
};

inline constexpr auto AsExpression =
    [](std::string_view content) -> std::unique_ptr<ast::Expression> {
  return std::make_unique<ast::Terminal>(content, "");
};

struct AtomicExpression {
  static constexpr auto parser =
      ((Number() | StringLiteral()) << Bind(AsExpression))  //
      | Parenthesized(Expression)                           //
      | FunctionLiteral()                                   //
      | StructLiteral()                                     //
      | ScopeLiteral()                                      //
      | ArrayLiteral()                                      //
      | ArrayType()                                         //
      | TerminalOrIdentifier();
};

std::optional<ast::Module> ParseModule(
    absl::Span<Lexeme const> lexemes, diagnostic::DiagnosticConsumer &consumer);

}  // namespace frontend

#endif  // ICARUS_FRONTEND_PARSE_H

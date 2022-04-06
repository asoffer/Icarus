#ifndef ICARUS_FRONTEND_PARSE_H
#define ICARUS_FRONTEND_PARSE_H

#include <memory>

#include "absl/types/span.h"
#include "ast/ast.h"
#include "ast/module.h"
#include "core/lexeme.h"
#include "diagnostic/consumer/consumer.h"
#include "frontend/parser_dsl.h"

namespace frontend {

struct Expression {
  using match_type = base::type_list<std::unique_ptr<ast::Expression>>;

  static bool Parse(absl::Span<core::Lexeme const> &lexemes,
                    std::string_view &consumed,
                    std::unique_ptr<ast::Expression> &e);
};

struct Statement {
  using match_type = base::type_list<std::unique_ptr<ast::Node>>;

  static bool Parse(absl::Span<core::Lexeme const> &lexemes,
                    std::string_view &consumed,
                    std::unique_ptr<ast::Node> &out);
};

struct Number : core::Kind<core::Lexeme::Kind::Number> {};
struct StringLiteral : core::Kind<core::Lexeme::Kind::String> {};
struct AnOperator : core::Kind<core::Lexeme::Kind::Operator> {};
struct Identifier : core::Kind<core::Lexeme::Kind::Identifier> {};

struct DeclarationId {
  static constexpr auto parser = ~(Identifier() | Parenthesized(AnOperator()));
  static constexpr auto bind   = [](std::string_view range) {
    return ast::Declaration::Id(range);
  };
};

struct CallArgument {
 private:
  struct Positional {
    static constexpr auto parser = Expression();
    static ast::Call::Argument bind(std::string_view,
                                    std::unique_ptr<ast::Expression> e) {
      return ast::Call::Argument("", std::move(e));
    }
  };

  struct Named {
    static constexpr auto parser =
        Identifier() + ~core::Match<"=">() + Expression();
    static constexpr auto bind = [](std::string_view, std::string_view name,
                                    std::unique_ptr<ast::Expression> expr) {
      return ast::Call::Argument(name, std::move(expr));
    };
  };

 public:
  static constexpr auto parser = Named() | Positional();
  static constexpr auto bind   = core::Identity;
};

struct ExpressionOrExpressionList {
 private:
  struct OneExpression {
    static constexpr auto parser = Expression();
    static constexpr auto bind = core::Vector<std::unique_ptr<ast::Expression>>;
  };
 public:
  static constexpr auto parser =
      Parenthesized(CommaSeparatedListOf(Expression())) | OneExpression();
  static constexpr auto bind = core::Identity;
};

struct TerminalOrIdentifier {
  using match_type = base::type_list<std::unique_ptr<ast::Expression>>;

  static bool Parse(absl::Span<core::Lexeme const> &lexemes,
                    std::string_view &consumed,
                    std::unique_ptr<ast::Expression> &out);
};

inline constexpr auto AsExpression =
    [](std::string_view,
       std::string_view content) -> std::unique_ptr<ast::Expression> {
  return std::make_unique<ast::Terminal>(content, "");
};

std::optional<ast::Module> ParseModule(
    absl::Span<core::Lexeme const> lexemes,
    diagnostic::DiagnosticConsumer &consumer);

// /// -----------------------------------------------------------------------------------
// struct Access {
//   static constexpr auto parser = Expression() + Identifier();
//   static constexpr auto bind = 
// std::string_view range, size_t length,
//                   std::unique_ptr<Expression> operand
// };
// 
// struct ArgumentType {
//   static constexpr auto parser = ~core::Match<"$">() + Identifier();
// };
// 
struct ArrayLiteral {
  static constexpr auto parser =
      Bracketed(CommaSeparatedListOf(Expression()));
  static constexpr auto bind =
      core::MakeUnique<ast::ArrayLiteral, ast::Expression>;
};

struct ArrayType {
  static constexpr auto parser =
      Bracketed(CommaSeparatedListOf<1>(Expression()) +
                ~core::Kind<core::Lexeme::Kind::Semicolon>() + Expression());
  static constexpr auto bind =
      core::MakeUnique<ast::ArrayType, ast::Expression>;
};

struct Assignment {
  static constexpr auto parser = ExpressionOrExpressionList() +
                                 ~core::Kind<core::Lexeme::Kind::Assignment>() +
                                 ExpressionOrExpressionList();
  static constexpr auto bind = core::MakeUnique<ast::Assignment>;
};

// struct BlockNode {
//   static constexpr auto parser =
//       Identifier() + Optional(Bracketed(CommaSeparatedListOf(Declaration()))) +
//       Braced(NewlineSeparatedListOf(Statement()));
// };

struct Builtin {
  static constexpr auto parser = ~core::Match<"builtin">();
  static constexpr auto bind = core::MakeUnique<ast::Builtin, ast::Expression>;
};

struct Declaration {
 private:
  struct DeclarationStart {
   private:
    struct OneDeclarationId {
      static constexpr auto parser = DeclarationId();
      static constexpr auto bind   = core::Vector<ast::Declaration::Id>;
    };

   public:
    static constexpr auto parser =
        OneDeclarationId() |
        Parenthesized(CommaSeparatedListOf(DeclarationId()));
    static constexpr auto bind = core::Identity;
  };

  struct InferredType {
   private:
    struct InferenceMarker {
      static constexpr auto parser = core::Match<":=">() | core::Match<"::=">();

      static constexpr auto bind =
          [](std::string_view,
             core::Lexeme const &l) -> ast::Declaration::Flags {
        if (l.content().size() == 3) { return ast::Declaration::f_IsConst; }
        return ast::Declaration::Flags{};
      };
    };

   public:
    static constexpr auto parser = DeclarationStart() + InferenceMarker() +
                                   Expression() +
                                   Optional(~core::Match<"=">() + Expression());
    static constexpr auto bind = core::Construct<ast::Declaration>;
  };

  struct ExplicitType {
   private:
    struct Marker {
      static constexpr auto parser = core::Match<":">() | core::Match<"::">();
      static constexpr auto bind =
          [](std::string_view,
             core::Lexeme const &l) -> ast::Declaration::Flags {
        if (l.content().size() == 2) { return ast::Declaration::f_IsConst; }
        return ast::Declaration::Flags{};
      };
    };

   public:
    static constexpr auto parser = DeclarationStart() + Marker() + Expression();
    static constexpr auto bind   = core::Construct<ast::Declaration>;
  };

 public:
  static constexpr auto parser = InferredType() | ExplicitType();
  static constexpr auto bind   = core::Identity;
};

struct DesignatedInitializer {
  static constexpr auto parser = Expression() + ~core::Match<".">() +
                                 Braced(NewlineSeparatedListOf(Assignment()));
  static constexpr auto bind =
      core::MakeUnique<ast::DesignatedInitializer, ast::Expression>;
};

// struct EnumLiteral {
//   static constexpr auto parser =
//       ~core::Match<"enum">() + Braced(NewlineSeparatedListOf(Assignment()));
// };
// 
// struct FunctionLiteral {
//   static constexpr auto parser =
//       (Parenthesized(CommaSeparatedListOf(Declaration())) + ~core::Match<"->">() +
//        Optional(Parenthesized(CommaSeparatedListOf(Expression()))) +
//        Braced(NewlineSeparatedListOf(Statement())))
//       << BindWithRange(MakeUnique<ast::FunctionLiteral, ast::Expression>);
// };
// 
// struct FunctionType {
//   static constexpr auto parser =
//       Parenthesized(CommaSeparatedListOf(Declaration())) + ~core::Match<"->">() +
//       (Expression() | Parenthesized(CommaSeparatedListOf(Expression())));
// };
// 
// struct Import {
//   static constexpr auto parser = ~core::Match<"import">() + Expression();
// };
// 
// struct Index {
//   static constexpr auto parser = Expression() + Bracketed(Expression());
// };
// 
// struct Index {
//   static constexpr auto parser = Expression() + Bracketed(Expression());
// };

struct Label {
  static constexpr auto parser = ~(core::Kind<core::Lexeme::Kind::Hash>() +
                                   core::Match<".">() + Identifier());
  static constexpr auto bind   = [](std::string_view range) {
    return std::optional<ast::Label>(range);
  };
};

struct StructLiteral {
 private:
  struct Impl {
    static constexpr auto parser =
        Braced(NewlineSeparatedListOf(Declaration()));
    static constexpr auto bind =
        core::MakeUnique<ast::StructLiteral, ast::Expression>;
  };

  struct ParameterizedImpl {
    static constexpr auto parser =
        (Parenthesized(CommaSeparatedListOf(Declaration())) +
         Braced(NewlineSeparatedListOf(Declaration())));
    static constexpr auto bind =
        core::MakeUnique<ast::ParameterizedStructLiteral, ast::Expression>;
  };

 public:
  static constexpr auto parser =
      ~core::Match<"struct">() + (Impl() | ParameterizedImpl());
  static constexpr auto bind = core::Identity;
};

struct ProgramArguments {
  static constexpr auto parser = ~core::Match<"arguments">();
  static constexpr auto bind =
      core::MakeUnique<ast::ProgramArguments, ast::Expression>;
};

struct ReturnStatement {
  static constexpr auto parser =
      ~core::Match<"return">() + CommaSeparatedListOf(Expression());
  static constexpr auto bind = core::MakeUnique<ast::ReturnStmt, ast::Node>;
};

// struct ScopeLiteral {
//   static constexpr auto parser =
//       (~core::Match<"scope">() +
//        Bracketed(Identifier() << Bind(Construct<ast::Declaration::Id>)) +
//        Parenthesized(CommaSeparatedListOf(Declaration())) +
//        Braced(NewlineSeparatedListOf(Statement())))
//       << BindWithRange(MakeUnique<ast::ScopeLiteral, ast::Expression>);
// };

struct SliceType {
  static constexpr auto parser =
      ~Bracketed(core::Nothing()) + Expression();
  static constexpr auto bind =
      core::MakeUnique<ast::SliceType, ast::Expression>;
};

// struct ShortFunctionLiteral {
//   static constexpr auto parser =
//       (Parenthesized(CommaSeparatedListOf(Declaration())) +
//       ~core::Match<"=>">() +
//        Expression())
//       << BindWithRange(MakeUnique<ast::ShortFunctionLiteral,
//       ast::Expression>);
// };

struct YieldStatement {
  static constexpr auto parser = core::Optional(Label()) +
                                 ~core::Match<"<<">() +
                                 CommaSeparatedListOf(CallArgument());
  static constexpr auto bind = core::MakeUnique<ast::YieldStmt, ast::Node>;
};

struct WhileStatement {
  static constexpr auto parser = ~core::Match<"while">() +
                                 Parenthesized(Expression()) +
                                 Braced(NewlineSeparatedListOf(Statement()));
  static constexpr auto bind = core::MakeUnique<ast::WhileStmt, ast::Node>;
};

// ICARUS_AST_NODE_X(BinaryAssignmentOperator)
// ICARUS_AST_NODE_X(BinaryOperator)
// ICARUS_AST_NODE_X(BindingDeclaration)
// ICARUS_AST_NODE_X(BuiltinFn)
// ICARUS_AST_NODE_X(Call)
// ICARUS_AST_NODE_X(Cast)
// ICARUS_AST_NODE_X(ComparisonOperator)
// ICARUS_AST_NODE_X(Identifier)
// ICARUS_AST_NODE_X(InterfaceLiteral)
// ICARUS_AST_NODE_X(PatternMatch)
// ICARUS_AST_NODE_X(ScopeNode)
// ICARUS_AST_NODE_X(Terminal)
// ICARUS_AST_NODE_X(UnaryOperator)
// ICARUS_AST_NODE_X(IfStmt)

struct AtomicExpression {
 private:
  // TODO: Add a mechanism to core that allows us to spell this type inline.
  struct NumberOrStringLiteral {
    static constexpr auto parser = Number() | StringLiteral();
    static constexpr auto bind   = AsExpression;
  };

 public:
  static constexpr auto parser = NumberOrStringLiteral()        //
                                 | Builtin()                    //
                                 | ProgramArguments()           //
                                 | Parenthesized(Expression())  //
                                 | StructLiteral()              //
                                 // | FunctionLiteral()         //
                                 // | ScopeLiteral()            //
                                 // | DesignatedInitializer()  //
                                 | ArrayLiteral()  //
                                 | ArrayType()     //
                                 | SliceType()     //
                                 | TerminalOrIdentifier();

  static constexpr auto bind = core::Identity;
};

}  // namespace frontend

#endif  // ICARUS_FRONTEND_PARSE_H

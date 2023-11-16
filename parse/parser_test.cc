#include "parse/parser.h"

#include <stack>
#include <variant>

#include "common/resources.h"
#include "diagnostics/consumer/null.h"
#include "lexer/lexer.h"
#include "nth/meta/type.h"
#include "nth/test/test.h"

namespace ic {

struct TreeNodeRef {
  ParseTree const &tree;
  ParseNodeIndex index;
  auto children() const { return tree.child_indices(index); }

  friend void NthPrint(auto &p, auto &f, TreeNodeRef const &t) {
    struct Indent {
      int n;
    };
    using Action = std::variant<ParseNodeIndex, Indent>;
    std::stack<Action> actions;
    actions.push(t.index);
    p.write("\n");
    int indentation = 4;
    while (not actions.empty()) {
      auto action = std::move(actions.top());
      actions.pop();
      std::visit(
          [&](auto a) {
            constexpr auto type = nth::type<decltype(a)>;
            if constexpr (type == nth::type<ParseNodeIndex>) {
              p.write(indentation, ' ');
              f(p, t.tree[a].kind);
              p.write("\n");
              actions.push(Indent{.n = -2});
              for (auto index : t.tree.child_indices(a)) {
                actions.push(index);
              }
              actions.push(Indent{.n = 2});
            } else {
              indentation += a.n;
            }
          },
          action);
    }
  }
};

TreeNodeRef FromRoot(ParseTree const &tree NTH_ATTRIBUTE(lifetimebound)) {
  return {
      .tree  = tree,
      .index = ParseNodeIndex{tree.size() - 1},
  };
}

#define IC_XMACRO_PARSE_NODE(node)                                             \
  inline constexpr auto node = nth::debug::MakeProperty<#node>(                \
      [](auto const &expr, auto const &...children) {                          \
        using std::begin;                                                      \
        using std::end;                                                        \
        auto &&value = nth::debug::EvaluateTraced(expr);                       \
        auto v       = value.children();                                       \
        std::vector<ParseNodeIndex> child_vector;                              \
        for (auto const &element : v) { child_vector.push_back(element); }     \
        if (value.tree[value.index].kind != ParseNode::Kind::node) {           \
          return false;                                                        \
        }                                                                      \
        if (sizeof...(children) != child_vector.size()) { return false; }      \
        std::reverse(child_vector.begin(), child_vector.end());                \
        auto iter = child_vector.begin();                                      \
        auto e    = child_vector.end();                                        \
        return (nth::debug::Matches(                                           \
                    children,                                                  \
                    TreeNodeRef{.tree = value.tree, .index = *iter++}) and     \
                ...);                                                          \
      });
#include "parse/node.xmacro.h"

NTH_TEST("parser/empty", std::string_view content) {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(content, d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(FromRoot(tree) >>=
             Module(ModuleStart(), StatementSequence(ScopeStart())));
}

NTH_INVOKE_TEST("parser/empty") {
  co_yield "";
  co_yield "\n";
  co_yield "\n\n";
  co_yield "\n   \n";
}

NTH_TEST("parser/declaration/integer") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex("let x ::= 3", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(FromRoot(tree) >>= Module(
                 ModuleStart(),
                 StatementSequence(ScopeStart(),
                                   Statement(StatementStart(),
                                             Declaration(DeclarationStart(),
                                                         DeclaredIdentifier(),
                                                         IntegerLiteral())))));
}

NTH_TEST("parser/declaration/bool") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex("let x := true", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(FromRoot(tree) >>= Module(
                 ModuleStart(),
                 StatementSequence(ScopeStart(),
                                   Statement(StatementStart(),
                                             Declaration(DeclarationStart(),
                                                         DeclaredIdentifier(),
                                                         BooleanLiteral())))));
}

NTH_TEST("parser/declaration/with-type-and-initializer") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex("let x: T = true", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>=
      Module(ModuleStart(),
             StatementSequence(
                 ScopeStart(),
                 Statement(StatementStart(),
                           Declaration(DeclarationStart(), DeclaredIdentifier(),
                                       Identifier(), BooleanLiteral())))));
}

NTH_TEST("parser/declaration/with-type-but-no-initializer") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex("let x: T", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(FromRoot(tree) >>= Module(
                 ModuleStart(),
                 StatementSequence(ScopeStart(),
                                   Statement(StatementStart(),
                                             Declaration(DeclarationStart(),
                                                         DeclaredIdentifier(),
                                                         Identifier())))));
}

NTH_TEST("parser/comment") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex("let x ::= true  // comment!", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(FromRoot(tree) >>= Module(
                 ModuleStart(),
                 StatementSequence(ScopeStart(),
                                   Statement(StatementStart(),
                                             Declaration(DeclarationStart(),
                                                         DeclaredIdentifier(),
                                                         BooleanLiteral())))));
}

NTH_TEST("parser/multiple-declarations-with-newlines") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(
  let x ::= 3
  var y ::= 4
  )",
                                d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>=
      Module(ModuleStart(),
             StatementSequence(
                 ScopeStart(),
                 Statement(StatementStart(),
                           Declaration(DeclarationStart(), DeclaredIdentifier(),
                                       IntegerLiteral())),
                 Statement(StatementStart(),
                           Declaration(DeclarationStart(), DeclaredIdentifier(),
                                       IntegerLiteral())))));
}

NTH_TEST("parser/operator-precedence/plus-times") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(x + y * z)", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>= Module(
          ModuleStart(),
          StatementSequence(
              ScopeStart(),
              Statement(StatementStart(), ExpressionPrecedenceGroup(
                                              Identifier(), InfixOperator(),
                                              ExpressionPrecedenceGroup(
                                                  Identifier(), InfixOperator(),
                                                  Identifier()))))));
}

NTH_TEST("parser/operator-precedence/times-plus") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(x * y + z)", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>=
      Module(ModuleStart(),
             StatementSequence(
                 ScopeStart(),
                 Statement(StatementStart(),
                           ExpressionPrecedenceGroup(
                               ExpressionPrecedenceGroup(
                                   Identifier(), InfixOperator(), Identifier()),
                               InfixOperator(), Identifier())))));
}

NTH_TEST("parser/operator-precedence/multiple-levels") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(a + b * c == d)", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>= Module(
          ModuleStart(),
          StatementSequence(ScopeStart(),
                            Statement(StatementStart(),
                                      ExpressionPrecedenceGroup(
                                          ExpressionPrecedenceGroup(
                                              Identifier(), InfixOperator(),
                                              ExpressionPrecedenceGroup(
                                                  Identifier(), InfixOperator(),
                                                  Identifier())),
                                          InfixOperator(), Identifier())))));
}

NTH_TEST("parser/operator-precedence/multiple-levels-balanced") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(a + b == c * d)", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>=
      Module(ModuleStart(),
             StatementSequence(
                 ScopeStart(),
                 Statement(
                     StatementStart(),
                     ExpressionPrecedenceGroup(
                         ExpressionPrecedenceGroup(
                             Identifier(), InfixOperator(), Identifier()),
                         InfixOperator(),
                         ExpressionPrecedenceGroup(
                             Identifier(), InfixOperator(), Identifier()))))));
}

NTH_TEST("parser/operator-precedence/plus-plus") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(x + y + z)", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(FromRoot(tree) >>= Module(
                 ModuleStart(),
                 StatementSequence(
                     ScopeStart(),
                     Statement(StatementStart(),
                               ExpressionPrecedenceGroup(
                                   Identifier(), InfixOperator(), Identifier(),
                                   InfixOperator(), Identifier())))));
}

NTH_TEST("parser/access/basic") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(a.b)", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(FromRoot(tree) >>= Module(
                 ModuleStart(),
                 StatementSequence(ScopeStart(),
                                   Statement(StatementStart(),
                                             MemberExpression(Identifier())))));
}

NTH_TEST("parser/access/nested") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(a.b.c)", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>=
      Module(ModuleStart(),
             StatementSequence(
                 ScopeStart(),
                 Statement(StatementStart(),
                           MemberExpression(MemberExpression(Identifier()))))));
}

NTH_TEST("parser/access/precedence") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(a.b * c.d)", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>=
      Module(ModuleStart(),
             StatementSequence(
                 ScopeStart(),
                 Statement(StatementStart(),
                           ExpressionPrecedenceGroup(
                               MemberExpression(Identifier()), InfixOperator(),
                               MemberExpression(Identifier()))))));
}

NTH_TEST("parser/invoke/empty") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(f())", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(FromRoot(tree) >>=
             Module(ModuleStart(),
                    StatementSequence(
                        ScopeStart(),
                        Statement(StatementStart(),
                                  CallExpression(Identifier(),
                                                 InvocationArgumentStart())))));
}

NTH_TEST("parser/invoke/empty-member-call") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(a.b())", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(FromRoot(tree) >>=
             Module(ModuleStart(),
                    StatementSequence(
                        ScopeStart(),
                        Statement(StatementStart(),
                                  CallExpression(MemberExpression(Identifier()),
                                                 InvocationArgumentStart())))));
}

NTH_TEST("parser/invoke/double-call") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(a.b()())", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>= Module(
          ModuleStart(),
          StatementSequence(
              ScopeStart(),
              Statement(
                  StatementStart(),
                  CallExpression(CallExpression(MemberExpression(Identifier()),
                                                InvocationArgumentStart()),
                                 InvocationArgumentStart())))));
}

NTH_TEST("parser/invoke/one-positional") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(a(b))", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>= Module(
          ModuleStart(),
          StatementSequence(
              ScopeStart(),
              Statement(StatementStart(),
                        CallExpression(Identifier(), InvocationArgumentStart(),
                                       Identifier())))));
}

NTH_TEST("parser/invoke/one-positional-newline") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(a(
  b
  ))",
                                d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>= Module(
          ModuleStart(),
          StatementSequence(
              ScopeStart(),
              Statement(StatementStart(),
                        CallExpression(Identifier(), InvocationArgumentStart(),
                                       Identifier())))));
}

NTH_TEST("parser/invoke/multiple-positional") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(a(b, c, d))", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>= Module(
          ModuleStart(),
          StatementSequence(
              ScopeStart(),
              Statement(
                  StatementStart(),
                  CallExpression(Identifier(), InvocationArgumentStart(),
                                 Identifier(), Identifier(), Identifier())))));
}

NTH_TEST("parser/invoke/multiple-positional-newline") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(
  a(b, c,
    d))",
                                d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>= Module(
          ModuleStart(),
          StatementSequence(
              ScopeStart(),
              Statement(
                  StatementStart(),
                  CallExpression(Identifier(), InvocationArgumentStart(),
                                 Identifier(), Identifier(), Identifier())))));
}

NTH_TEST("parser/invoke/access-call") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(a.b(c.d))", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>=
      Module(ModuleStart(),
             StatementSequence(
                 ScopeStart(),
                 Statement(StatementStart(),
                           CallExpression(MemberExpression(Identifier()),
                                          InvocationArgumentStart(),
                                          MemberExpression(Identifier()))))));
}

NTH_TEST("parser/pointer") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(*a)", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>= Module(
          ModuleStart(),
          StatementSequence(ScopeStart(),
                            Statement(StatementStart(),
                                      Pointer(PointerStart(), Identifier())))));
}

NTH_TEST("parser/pointer/access") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(*a.b)", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(FromRoot(tree) >>=
             Module(ModuleStart(),
                    StatementSequence(
                        ScopeStart(),
                        Statement(StatementStart(),
                                  Pointer(PointerStart(),
                                          MemberExpression(Identifier()))))));
}

NTH_TEST("parser/pointer/function") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(*a -> b)", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>= Module(
          ModuleStart(),
          StatementSequence(ScopeStart(),
                            Statement(StatementStart(),
                                      ExpressionPrecedenceGroup(
                                          Pointer(PointerStart(), Identifier()),
                                          InfixOperator(), Identifier())))));
}

NTH_TEST("parser/buffer-pointer") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"([*]a)", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(FromRoot(tree) >>= Module(
                 ModuleStart(),
                 StatementSequence(ScopeStart(),
                                   Statement(StatementStart(),
                                             BufferPointer(BufferPointerStart(),
                                                           Identifier())))));
}

NTH_TEST("parser/buffer-pointer/access") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"([*]a.b)", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>=
      Module(ModuleStart(),
             StatementSequence(
                 ScopeStart(),
                 Statement(StatementStart(),
                           BufferPointer(BufferPointerStart(),
                                         MemberExpression(Identifier()))))));
}

NTH_TEST("parser/buffer-pointer/function") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"([*]a -> b)", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>= Module(
          ModuleStart(),
          StatementSequence(
              ScopeStart(),
              Statement(StatementStart(),
                        ExpressionPrecedenceGroup(
                            BufferPointer(BufferPointerStart(), Identifier()),
                            InfixOperator(), Identifier())))));
}

NTH_TEST("parser/if-statement/empty") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(
  if (condition) {}
  )",
                                d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>= Module(
          ModuleStart(),
          StatementSequence(
              ScopeStart(),
              Statement(StatementStart(),
                        IfStatement(Identifier(), IfStatementTrueBranchStart(),
                                    StatementSequence(ScopeStart()))))));
}

NTH_TEST("parser/if-statement/empty-newlines") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(
  if (condition) {
  }
  )",
                                d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>= Module(
          ModuleStart(),
          StatementSequence(
              ScopeStart(),
              Statement(StatementStart(),
                        IfStatement(Identifier(), IfStatementTrueBranchStart(),
                                    StatementSequence(ScopeStart()))))));
}

NTH_TEST("parser/if-statement/with-one-line-body") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(
  if (condition) {
    body
  }
  )",
                                d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>= Module(
          ModuleStart(),
          StatementSequence(
              ScopeStart(),
              Statement(
                  StatementStart(),
                  IfStatement(Identifier(), IfStatementTrueBranchStart(),
                              StatementSequence(ScopeStart(),
                                                Statement(StatementStart(),
                                                          Identifier())))))));
}

NTH_TEST("parser/if-statement/with-body") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(
  if (condition) {
    body
    body
  }
  )",
                                d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>= Module(
          ModuleStart(),
          StatementSequence(
              ScopeStart(),
              Statement(StatementStart(),
                        IfStatement(
                            Identifier(), IfStatementTrueBranchStart(),
                            StatementSequence(
                                ScopeStart(),
                                Statement(StatementStart(), Identifier()),
                                Statement(StatementStart(), Identifier())))))));
}

NTH_TEST("parser/if-else-statement/empty") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(
  if (condition) {
  } else {
  }
  )",
                                d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>= Module(
          ModuleStart(),
          StatementSequence(
              ScopeStart(),
              Statement(StatementStart(),
                        IfStatement(Identifier(), IfStatementTrueBranchStart(),
                                    StatementSequence(ScopeStart()),
                                    IfStatementFalseBranchStart(),
                                    StatementSequence(ScopeStart()))))));
}

NTH_TEST("parser/if-else-statement/with-else-body") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(
  if (condition) {
  } else {
    body
  }
  )",
                                d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>= Module(
          ModuleStart(),
          StatementSequence(
              ScopeStart(),
              Statement(
                  StatementStart(),
                  IfStatement(Identifier(), IfStatementTrueBranchStart(),
                              StatementSequence(ScopeStart()),
                              IfStatementFalseBranchStart(),
                              StatementSequence(ScopeStart(),
                                                Statement(StatementStart(),
                                                          Identifier())))))));
}

NTH_TEST("parser/if-else-statement/with-bodies") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(
  if (condition) {
    body
  } else {
    body
  }
  )",
                                d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>= Module(
          ModuleStart(),
          StatementSequence(
              ScopeStart(),
              Statement(
                  StatementStart(),
                  IfStatement(Identifier(), IfStatementTrueBranchStart(),
                              StatementSequence(
                                  ScopeStart(),
                                  Statement(StatementStart(), Identifier())),
                              IfStatementFalseBranchStart(),
                              StatementSequence(ScopeStart(),
                                                Statement(StatementStart(),
                                                          Identifier())))))));
}

NTH_TEST("parser/if-else-statement/one-line-with-bodies") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(if (condition) { body } else { body })", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>= Module(
          ModuleStart(),
          StatementSequence(
              ScopeStart(),
              Statement(
                  StatementStart(),
                  IfStatement(Identifier(), IfStatementTrueBranchStart(),
                              StatementSequence(
                                  ScopeStart(),
                                  Statement(StatementStart(), Identifier())),
                              IfStatementFalseBranchStart(),
                              StatementSequence(ScopeStart(),
                                                Statement(StatementStart(),
                                                          Identifier())))))));
}

NTH_TEST("parser/if-statement/else-if") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(
  if (condition) {} else if (condition) {}
  )",
                                d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>=
      Module(ModuleStart(),
             StatementSequence(
                 ScopeStart(),
                 Statement(
                     StatementStart(),
                     IfStatement(
                         Identifier(), IfStatementTrueBranchStart(),
                         StatementSequence(ScopeStart()),
                         IfStatementFalseBranchStart(),
                         StatementSequence(
                             ScopeStart(),
                             Statement(
                                 StatementStart(),
                                 IfStatement(
                                     Identifier(), IfStatementTrueBranchStart(),
                                     StatementSequence(ScopeStart())))))))));
}


NTH_TEST("parser/assignment") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(a = b)", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(FromRoot(tree) >>=
             Module(ModuleStart(),
                    StatementSequence(
                        ScopeStart(),
                        Statement(StatementStart(),
                                  Assignment(Identifier(), AssignedValueStart(),
                                             Identifier())))));
}

NTH_TEST("parser/assignment/with-operators") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(a + b = c + d)", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(FromRoot(tree) >>=
             Module(ModuleStart(),
                    StatementSequence(
                        ScopeStart(),
                        Statement(StatementStart(),
                                  Assignment(ExpressionPrecedenceGroup(
                                                 Identifier(), InfixOperator(),
                                                 Identifier()),
                                             AssignedValueStart(),
                                             ExpressionPrecedenceGroup(
                                                 Identifier(), InfixOperator(),
                                                 Identifier()))))));
}

NTH_TEST("parser/assignment/complex") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(a.b = c.d + e)", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>=
      Module(ModuleStart(),
             StatementSequence(
                 ScopeStart(),
                 Statement(StatementStart(),
                           Assignment(MemberExpression(Identifier()),
                                      AssignedValueStart(),
                                      ExpressionPrecedenceGroup(
                                          MemberExpression(Identifier()),
                                          InfixOperator(), Identifier()))))));
}

NTH_TEST("parser/function-literal/empty") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(fn() -> x {})", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>= Module(
          ModuleStart(),
          StatementSequence(
              ScopeStart(),
              Statement(StatementStart(),
                        FunctionLiteral(FunctionLiteralStart(),
                                        FunctionLiteralSignature(Identifier()),
                                        StatementSequence(ScopeStart()))))));
}

NTH_TEST("parser/function-literal/one-parameter") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(fn(let a: b) -> x {})", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>= Module(
          ModuleStart(),
          StatementSequence(
              ScopeStart(),
              Statement(StatementStart(),
                        FunctionLiteral(
                            FunctionLiteralStart(),
                            FunctionLiteralSignature(
                                Declaration(DeclarationStart(),
                                            DeclaredIdentifier(), Identifier()),
                                Identifier()),
                            StatementSequence(ScopeStart()))))));
}

NTH_TEST("parser/function-literal/multiple-parameters") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(
    fn(let a: b,
       let c: d) -> x {})",
                                d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>= Module(
          ModuleStart(),
          StatementSequence(
              ScopeStart(),
              Statement(StatementStart(),
                        FunctionLiteral(
                            FunctionLiteralStart(),
                            FunctionLiteralSignature(
                                Declaration(DeclarationStart(),
                                            DeclaredIdentifier(), Identifier()),
                                Declaration(DeclarationStart(),
                                            DeclaredIdentifier(), Identifier()),
                                Identifier()),
                            StatementSequence(ScopeStart()))))));
}

NTH_TEST("parser/function-literal/body") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(
    fn(let a: b) -> x {
      y
    })",
                                d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>= Module(
          ModuleStart(),
          StatementSequence(
              ScopeStart(),
              Statement(StatementStart(),
                        FunctionLiteral(
                            FunctionLiteralStart(),
                            FunctionLiteralSignature(
                                Declaration(DeclarationStart(),
                                            DeclaredIdentifier(), Identifier()),
                                Identifier()),
                            StatementSequence(
                                ScopeStart(),
                                Statement(StatementStart(), Identifier())))))));
}

NTH_TEST("parser/function-literal/no-returns-with-newlines") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(
  fn() -> (
    // No returns here!
  ) {})",
                                d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>= Module(
          ModuleStart(),
          StatementSequence(
              ScopeStart(),
              Statement(StatementStart(),
                        FunctionLiteral(FunctionLiteralStart(),
                                        FunctionLiteralSignature(NoReturns()),
                                        StatementSequence(ScopeStart()))))));
}

NTH_TEST("parser/function-literal/no-returns") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(fn() -> () {})", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>= Module(
          ModuleStart(),
          StatementSequence(
              ScopeStart(),
              Statement(StatementStart(),
                        FunctionLiteral(FunctionLiteralStart(),
                                        FunctionLiteralSignature(NoReturns()),
                                        StatementSequence(ScopeStart()))))));
}

NTH_TEST("parser/return-expression") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(return a)", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>=
      Module(ModuleStart(),
             StatementSequence(ScopeStart(), Statement(StatementStart(),
                                                       Return(Identifier())))));
}


NTH_TEST("parser/function-literal/return-expression") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(
    fn(let a: b) -> x {
      return c
    })",
                                d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>= Module(
          ModuleStart(),
          StatementSequence(
              ScopeStart(),
              Statement(
                  StatementStart(),
                  FunctionLiteral(
                      FunctionLiteralStart(),
                      FunctionLiteralSignature(
                          Declaration(DeclarationStart(), DeclaredIdentifier(),
                                      Identifier()),
                          Identifier()),
                      StatementSequence(ScopeStart(),
                                        Statement(StatementStart(),
                                                  Return(Identifier()))))))));
}

NTH_TEST("parser/slice/once") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(\i32)", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>=
      Module(ModuleStart(),
             StatementSequence(ScopeStart(),
                               Statement(StatementStart(),
                                         Slice(SliceStart(), TypeLiteral())))));
}

NTH_TEST("parser/slice/multiple") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(\\\i32)", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>=
      Module(ModuleStart(),
             StatementSequence(
                 ScopeStart(),
                 Statement(StatementStart(),
                           Slice(SliceStart(),
                                 Slice(SliceStart(),
                                       Slice(SliceStart(), TypeLiteral())))))));
}

NTH_TEST("parser/parenthesized") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"((a))", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(FromRoot(tree) >>= Module(
                 ModuleStart(),
                 StatementSequence(ScopeStart(),
                                   Statement(StatementStart(), Identifier()))));
}

NTH_TEST("parser/parenthesized/newlines") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"((
  a))",
                                d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(FromRoot(tree) >>= Module(
                 ModuleStart(),
                 StatementSequence(ScopeStart(),
                                   Statement(StatementStart(), Identifier()))));
}

NTH_TEST("parser/while/empty") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(while (condition) {})", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(FromRoot(tree) >>= Module(
                 ModuleStart(),
                 StatementSequence(
                     ScopeStart(),
                     Statement(StatementStart(),
                               WhileLoop(WhileLoopStart(), Identifier(),
                                         WhileLoopBodyStart(),
                                         StatementSequence(ScopeStart()))))));
}

NTH_TEST("parser/while/statements") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(while (condition) {
    a
    b
  })",
                                d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>= Module(
          ModuleStart(),
          StatementSequence(
              ScopeStart(),
              Statement(
                  StatementStart(),
                  WhileLoop(WhileLoopStart(), Identifier(), WhileLoopBodyStart(),
                            StatementSequence(
                                ScopeStart(),
                                Statement(StatementStart(), Identifier()),
                                Statement(StatementStart(), Identifier())))))));
}

NTH_TEST("parser/address") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(&n + 0)", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(FromRoot(tree) >>=
             Module(ModuleStart(),
                    StatementSequence(
                        ScopeStart(),
                        Statement(StatementStart(),
                                  ExpressionPrecedenceGroup(
                                      Address(AddressStart(), Identifier()),
                                      InfixOperator(), IntegerLiteral())))));
}

NTH_TEST("parser/deref") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(@n + 0)", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(FromRoot(tree) >>=
             Module(ModuleStart(),
                    StatementSequence(
                        ScopeStart(),
                        Statement(StatementStart(),
                                  ExpressionPrecedenceGroup(
                                      Deref(DerefStart(), Identifier()),
                                      InfixOperator(), IntegerLiteral())))));
}

}  // namespace ic

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

  friend void NthPrint(auto& p, auto& f, TreeNodeRef const &t) {
    struct Indent {
      int n;
    };
    using Action = std::variant<ParseNodeIndex, Indent>;
    std::stack<Action> actions ;
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
  NTH_EXPECT(FromRoot(tree) >>= FunctionStart());
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
  NTH_EXPECT(
      FromRoot(tree) >>= StatementSequence(Statement(
          StatementStart(), Declaration(Let(), DeclaredIdentifier(),
                                        ColonColonEqual(), IntegerLiteral()))));
}

NTH_TEST("parser/declaration/bool") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex("let x := true", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>= StatementSequence(Statement(
          StatementStart(), Declaration(Let(), DeclaredIdentifier(),
                                        ColonEqual(), BooleanLiteral()))));
}

NTH_TEST("parser/comment") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex("let x ::= true  // comment!", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>= StatementSequence(Statement(
          StatementStart(), Declaration(Let(), DeclaredIdentifier(),
                                        ColonColonEqual(), BooleanLiteral()))));
}

NTH_TEST("parser/multiple-declarations-with-newlines") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(
  let x ::= 3
  var y ::= 4
  )",
                                d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(FromRoot(tree) >>= StatementSequence(
                 Statement(StatementStart(),
                           Declaration(Let(), DeclaredIdentifier(),
                                       ColonColonEqual(), IntegerLiteral())),
                 Statement(StatementStart(),
                           Declaration(Var(), DeclaredIdentifier(),
                                       ColonColonEqual(), IntegerLiteral()))));
}

NTH_TEST("parser/operator-precedence/plus-times") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(x + y * z)", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(FromRoot(tree) >>= StatementSequence(Statement(
                 StatementStart(),
                 ExpressionPrecedenceGroup(
                     Identifier(), InfixOperator(),
                     ExpressionPrecedenceGroup(Identifier(), InfixOperator(),
                                               Identifier())))));
}

NTH_TEST("parser/operator-precedence/times-plus") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(x * y + z)", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(FromRoot(tree) >>= StatementSequence(
                 Statement(StatementStart(),
                           ExpressionPrecedenceGroup(
                               ExpressionPrecedenceGroup(
                                   Identifier(), InfixOperator(), Identifier()),
                               InfixOperator(), Identifier()))));
}

NTH_TEST("parser/operator-precedence/plus-plus") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(x + y + z)", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>= StatementSequence(Statement(
          StatementStart(),
          ExpressionPrecedenceGroup(Identifier(), InfixOperator(), Identifier(),
                                    InfixOperator(), Identifier()))));
}

NTH_TEST("parser/access/basic") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(a.b)", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(FromRoot(tree) >>= StatementSequence(
                 Statement(StatementStart(), MemberExpression(Identifier()))));
}

NTH_TEST("parser/access/nested") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(a.b.c)", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>= StatementSequence(Statement(
          StatementStart(), MemberExpression(MemberExpression(Identifier())))));
}

NTH_TEST("parser/access/precedence") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(a.b * c.d)", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>= StatementSequence(Statement(
          StatementStart(), ExpressionPrecedenceGroup(
                                MemberExpression(Identifier()), InfixOperator(),
                                MemberExpression(Identifier())))));
}

NTH_TEST("parser/invoke/empty") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(f())", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(FromRoot(tree) >>= StatementSequence(Statement(
                 StatementStart(),
                 CallExpression(Identifier(), InvocationArgumentStart()))));
}

NTH_TEST("parser/invoke/empty-member-call") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(a.b())", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>= StatementSequence(Statement(
          StatementStart(), CallExpression(MemberExpression(Identifier()),
                                           InvocationArgumentStart()))));
}

NTH_TEST("parser/invoke/double-call") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(a.b()())", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(FromRoot(tree) >>= StatementSequence(Statement(
                 StatementStart(),
                 CallExpression(CallExpression(MemberExpression(Identifier()),
                                               InvocationArgumentStart()),
                                InvocationArgumentStart()))));
}

NTH_TEST("parser/invoke/one-positional") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(a(b))", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(FromRoot(tree) >>= StatementSequence(Statement(
                 StatementStart(),
                 CallExpression(Identifier(), InvocationArgumentStart(),
                                Identifier()))));
}

NTH_TEST("parser/invoke/one-positional-newline") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(a(
  b
  ))",
                                d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(FromRoot(tree) >>= StatementSequence(Statement(
                 StatementStart(),
                 CallExpression(Identifier(), InvocationArgumentStart(),
                                Identifier()))));
}

NTH_TEST("parser/invoke/multiple-positional") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(a(b, c, d))", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(FromRoot(tree) >>= StatementSequence(Statement(
                 StatementStart(),
                 CallExpression(Identifier(), InvocationArgumentStart(),
                                Identifier(), Identifier(), Identifier()))));
}

NTH_TEST("parser/invoke/multiple-positional-newline") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(
  a(b, c,
    d))",
                                d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(FromRoot(tree) >>= StatementSequence(Statement(
                 StatementStart(),
                 CallExpression(Identifier(), InvocationArgumentStart(),
                                Identifier(), Identifier(), Identifier()))));
}

NTH_TEST("parser/invoke/access-call") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(a.b(c.d))", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>= StatementSequence(Statement(
          StatementStart(), CallExpression(MemberExpression(Identifier()),
                                           InvocationArgumentStart(),
                                           MemberExpression(Identifier())))));
}

NTH_TEST("parser/invoke/pointer") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(*a)", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(FromRoot(tree) >>= StatementSequence(
                 Statement(StatementStart(), Pointer(Identifier()))));
}

NTH_TEST("parser/invoke/pointer-access") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(*a.b)", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(FromRoot(tree) >>= StatementSequence(Statement(
                 StatementStart(), Pointer(MemberExpression(Identifier())))));
}

NTH_TEST("parser/invoke/pointer-function") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(*a -> b)", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(FromRoot(tree) >>= StatementSequence(Statement(
                 StatementStart(),
                 ExpressionPrecedenceGroup(Pointer(Identifier()),
                                           InfixOperator(), Identifier()))));
}

NTH_TEST("parser/invoke/buffer-pointer") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"([*]a)", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(FromRoot(tree) >>= StatementSequence(
                 Statement(StatementStart(), BufferPointer(Identifier()))));
}

NTH_TEST("parser/invoke/buffer-pointer-access") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"([*]a.b)", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>= StatementSequence(Statement(
          StatementStart(), BufferPointer(MemberExpression(Identifier())))));
}

NTH_TEST("parser/invoke/buffer-pointer-function") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"([*]a -> b)", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(FromRoot(tree) >>= StatementSequence(Statement(
                 StatementStart(),
                 ExpressionPrecedenceGroup(BufferPointer(Identifier()),
                                           InfixOperator(), Identifier()))));
}

NTH_TEST("parser/invoke/if-statement-empty") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(
  if (condition) {}
  )", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(FromRoot(tree) >>= StatementSequence(Statement(
                 StatementStart(),
                 IfStatement(Identifier(), IfStatementTrueBranchStart()))));
}

NTH_TEST("parser/invoke/if-statement-empty-newlines") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(
  if (condition) {
  }
  )",
                                d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(FromRoot(tree) >>= StatementSequence(Statement(
                 StatementStart(),
                 IfStatement(Identifier(), IfStatementTrueBranchStart()))));
}

NTH_TEST("parser/invoke/if-statement-with-one-line-body") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(
  if (condition) {
    body
  }
  )",
                                d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>= StatementSequence(Statement(
          StatementStart(),
          IfStatement(
              Identifier(), IfStatementTrueBranchStart(), ScopeStart(),
              StatementSequence(Statement(StatementStart(), Identifier()))))));
}

NTH_TEST("parser/invoke/if-statement-with-body") {
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
      FromRoot(tree) >>= StatementSequence(Statement(
          StatementStart(),
          IfStatement(
              Identifier(), IfStatementTrueBranchStart(), ScopeStart(),
              StatementSequence(Statement(StatementStart(), Identifier()),
                                Statement(StatementStart(), Identifier()))))));
}

NTH_TEST("parser/invoke/simple-assignment") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(a = b)", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>= StatementSequence(Statement(
          StatementStart(),
          Assignment(Identifier(), AssignedValueStart(), Identifier()))));
}

NTH_TEST("parser/invoke/complex-assignment") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(a.b = c.d + e)", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>= StatementSequence(Statement(
          StatementStart(),
          Assignment(
              MemberExpression(Identifier()), AssignedValueStart(),
              ExpressionPrecedenceGroup(MemberExpression(Identifier()),
                                        InfixOperator(), Identifier())))));
}

}  // namespace ic

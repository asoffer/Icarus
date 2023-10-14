#include "parser/parser.h"

#include <concepts>
#include <cstring>
#include <span>

#include "diagnostics/consumer/consumer.h"
#include "nth/debug/debug.h"
#include "parser/parse_tree.h"
#include "parser/precedence.h"

namespace ic {
namespace {

struct Parser {
  explicit Parser(TokenBuffer const& token_buffer,
                  diag::DiagnosticConsumer& diagnostic_consumer)
      : iterator_(token_buffer.begin()),
        token_buffer_(token_buffer),
        diagnostic_consumer_(diagnostic_consumer) {}

  Token current_token() const {
    NTH_REQUIRE((v.debug), iterator_ != token_buffer_.end());
    return *iterator_;
  }

  struct State {
    enum class Kind {
#define IC_XMACRO_PARSER_STATE(state) state,
#include "parser/parse_state.xmacro.h"
    };

    friend void NthPrint(auto& p, auto&, Kind const& k) {
      switch (k) {
#define IC_XMACRO_PARSER_STATE(kind)                                           \
  case Kind::kind:                                                             \
    p.write(#kind);                                                            \
    break;
#include "parser/parse_state.xmacro.h"
      }
    }

    Kind kind;
    Precedence ambient_precedence = Precedence::Loosest();
    Token token;
    uint32_t subtree_start = -1;

    friend void NthPrint(auto& p, auto& f, State const& s) {
      nth::Interpolate<"[{} {}]">(p, f, s.kind, s.subtree_start);
    }
  };
#define IC_XMACRO_PARSER_STATE(state) void Handle##state(ParseTree& tree);
#include "parser/parse_state.xmacro.h"

  std::span<State const> state() const { return state_; }
  std::span<State> state() { return state_; }

  void pop_and_discard_state() {
    NTH_REQUIRE((v.debug), not state_.empty());
    state_.pop_back();
  }

  void push_state(State state) { state_.push_back(state); }

  State pop_state() {
    NTH_REQUIRE((v.debug), not state_.empty());
    auto state = state_.back();
    pop_and_discard_state();
    return state;
  }

  void IgnoreAnyNewlines() {
    while (current_token().kind() != Token::Kind::Eof and
           current_token().kind() == Token::Kind::Newline) {
      ++iterator_;
    }
  }

  void ExpandState(auto... states) {
    auto state = pop_state();
    int dummy;
    (dummy = ... = (ExpandStateImpl(state, states), 0));
  }

 private:
  void ExpandStateImpl(State prototype, State::Kind kind) {
    prototype.kind = kind;
    ExpandStateImpl(prototype, prototype);
  }
  void ExpandStateImpl(State, State state) { state_.push_back(state); }

  std::vector<State> state_ = {
      {.kind = State::Kind::Module, .subtree_start = 0},
      {.kind = State::Kind::Newlines, .subtree_start = 0},
  };

  TokenBuffer::const_iterator iterator_;

  TokenBuffer const& token_buffer_;
  diag::DiagnosticConsumer& diagnostic_consumer_;
};

}  // namespace

ParseTree Parse(TokenBuffer const& token_buffer,
                diag::DiagnosticConsumer& diagnostic_consumer) {
  ParseTree tree;
  Parser p(token_buffer, diagnostic_consumer);

  while (not p.state().empty()) {
    if (tree.size() != 0) {
      NTH_REQUIRE((v.debug), tree.back().subtree_size > 0);
    }
    NTH_LOG((v.when(false)), "\n{} {}:\t{}") <<=
        {p.current_token(), p.state().back().subtree_start, p.state()};
    switch (p.state().back().kind) {
#define IC_XMACRO_PARSER_STATE(state)                                          \
  case Parser::State::Kind::state:                                             \
    p.Handle##state(tree);                                                     \
    break;
#include "parser/parse_state.xmacro.h"
    }
  }
  return tree;
}

#define IC_XMACRO_PARSER_STATE(kind)
#define IC_XMACRO_PARSER_STATE_SEQUENCE(state_kind, separator)                   \
  void Parser::Handle##state_kind##Sequence(ParseTree& tree) {                   \
    if (current_token().kind() == Token::Kind::Eof) {                            \
      pop_and_discard_state();                                                   \
      return;                                                                    \
    }                                                                            \
                                                                                 \
    ExpandState(State::Kind::state_kind, State::Kind::separator,                 \
                State::Kind::Subsequent##state_kind##Sequence,                   \
                State{                                                           \
                    .kind          = State::Kind::Resolve##state_kind##Sequence, \
                    .subtree_start = tree.size(),                                \
                });                                                              \
  }                                                                              \
                                                                                 \
  void Parser::HandleSubsequent##state_kind##Sequence(ParseTree& tree) {         \
    if (current_token().kind() == Token::Kind::Eof) {                            \
      pop_and_discard_state();                                                   \
      return;                                                                    \
    }                                                                            \
                                                                                 \
    ExpandState(State::Kind::state_kind, State::Kind::separator,                 \
                State::Kind::Subsequent##state_kind##Sequence);                  \
  }                                                                              \
                                                                                 \
  void Parser::HandleResolve##state_kind##Sequence(ParseTree& tree) {            \
    State state = pop_state();                                                   \
    tree.append(ParseTree::Node::Kind::state_kind##Sequence, Token::Invalid(),   \
                state.subtree_start);                                            \
  }
#include "parser/parse_state.xmacro.h"

void Parser::HandleNewlines(ParseTree& tree) {
  pop_and_discard_state();
  IgnoreAnyNewlines();
}

void Parser::HandleModule(ParseTree& tree) {
  ExpandState(State::Kind::StatementSequence);
}

void Parser::HandleStatement(ParseTree& tree) {
  ParseTree::Node::Kind k;
  switch (current_token().kind()) {
    case Token::Kind::Let: k = ParseTree::Node::Kind::Let; break;
    case Token::Kind::Var: k = ParseTree::Node::Kind::Var; break;
    default: ExpandState(State::Kind::Expression); return;
  }

  ExpandState(
      State::Kind::DeclaredSymbol,
      State{.kind = State::Kind::Declaration, .subtree_start = tree.size()});
  tree.append_leaf(k, *++iterator_);
}

void Parser::HandleDeclaration(ParseTree& tree) {
  ParseTree::Node::Kind node_kind;
  State::Kind state_kind;
  switch (current_token().kind()) {
    case Token::Kind::ColonColonEqual:
      node_kind  = ParseTree::Node::Kind::ColonColonEqual;
      state_kind = State::Kind::ResolveInferredTypeDeclaration;
      break;
    case Token::Kind::ColonEqual:
      node_kind  = ParseTree::Node::Kind::ColonEqual;
      state_kind = State::Kind::ResolveInferredTypeDeclaration;
      break;
    case Token::Kind::ColonColon:
      node_kind  = ParseTree::Node::Kind::ColonColon;
      state_kind = State::Kind::ResolveUninferredTypeDeclaration;
      break;
    case Token::Kind::Colon:
      node_kind  = ParseTree::Node::Kind::Colon;
      state_kind = State::Kind::ResolveUninferredTypeDeclaration;
      break;
    default: NTH_UNIMPLEMENTED("{}") <<= {current_token().kind()};
  }

  tree.append_leaf(node_kind, *iterator_++);
  ExpandState(
      State{
          .kind               = State::Kind::Expression,
          .ambient_precedence = Precedence::Loosest(),
          .subtree_start      = tree.size(),
      },
      State{
          .kind          = state_kind,
          .subtree_start = state().back().subtree_start,
      });
}

void Parser::HandleResolveInferredTypeDeclaration(ParseTree& tree) {
  State state = pop_state();
  tree.append(ParseTree::Node::Kind::Declaration, state.token,
              state.subtree_start);
}

void Parser::HandleResolveUninferredTypeDeclaration(ParseTree& tree) {
  switch (current_token().kind()) {
    case Token::Kind::Equal:
      ++iterator_;
      ExpandState(
          State{
              .kind               = State::Kind::Expression,
              .ambient_precedence = Precedence::Loosest(),
              .subtree_start      = tree.size(),
          },
          State{
              .kind          = State::Kind::ResolveInferredTypeDeclaration,
              .subtree_start = tree.size(),
          });
      break;
    default:
      ++iterator_;
      ExpandState(
          State{
              .kind               = State::Kind::Expression,
              .ambient_precedence = Precedence::Loosest(),
              .subtree_start      = tree.size(),
          },
          State{
              .kind          = State::Kind::ResolveDefaultedDeclaration,
              .subtree_start = tree.size(),
          });
  }
}

void Parser::HandleResolveDefaultedDeclaration(ParseTree&) {
  NTH_UNIMPLEMENTED();
}

void Parser::HandleDeclaredSymbol(ParseTree& tree) {
  NTH_REQUIRE((v.debug), current_token().kind() == Token::Kind::Identifier);
  state()[state().size() - 4].token = current_token();
  tree.append_leaf(ParseTree::Node::Kind::DeclaredIdentifier, *iterator_++);
  pop_and_discard_state();
}

void Parser::HandleExpression(ParseTree& tree) {
  ExpandState(State::Kind::AtomicTerm, State::Kind::ExpressionSuffix);
}

void Parser::HandleExpressionClosing(ParseTree& tree) {
  NTH_REQUIRE(current_token().kind() == Token::Kind::RightParen or
              current_token().kind() == Token::Kind::RightBracket or
              current_token().kind() == Token::Kind::RightBrace);
  State state = pop_state();
  tree.append(
      ParseTree::Node::Kind::ExpressionGroup, *iterator_++,
      state.subtree_start - tree.nodes()[state.subtree_start].subtree_size - 1);
}

void Parser::HandleInvocationArgumentSequence(ParseTree& tree) {
  IgnoreAnyNewlines();
  if (current_token().kind() == Token::Kind::RightParen) {
    ExpandState(State::Kind::ResolveInvocationArgumentSequence);
    return;
  } else if (current_token().kind() == Token::Kind::Comma) {
    ++iterator_;
    IgnoreAnyNewlines();
    ExpandState(State::Kind::Expression,
                State::Kind::InvocationArgumentSequence);
  } else {
    NTH_UNREACHABLE("{}") <<= {current_token().kind()};
  }
}

void Parser::HandleResolveMemberTerm(ParseTree& tree) {
  if (current_token().kind() != Token::Kind::Identifier) {
    NTH_UNIMPLEMENTED("{}") <<= {current_token()};
  }
  tree.append(ParseTree::Node::Kind::MemberExpression, current_token(),
              state().back().subtree_start);
  ++iterator_;
  pop_and_discard_state();
}

void Parser::HandleAtomicTerm(ParseTree& tree) {
  ParseTree::Node::Kind k;
  switch (current_token().kind()) {
    case Token::Kind::Builtin: k = ParseTree::Node::Kind::BuiltinLiteral; break;
    case Token::Kind::True:
    case Token::Kind::False: k = ParseTree::Node::Kind::BooleanLiteral; break;
    case Token::Kind::StringLiteral:
      k = ParseTree::Node::Kind::StringLiteral;
      break;
    case Token::Kind::IntegerLiteral:
      k = ParseTree::Node::Kind::IntegerLiteral;
      break;
    case Token::Kind::Identifier: k = ParseTree::Node::Kind::Identifier; break;

#define IC_XMACRO_PRIMITIVE_TYPE(kind, symbol, spelling)              \
  case Token::Kind::kind:
#include "common/language/primitive_types.xmacro.h"
      k = ParseTree::Node::Kind::TypeLiteral;
      break;
    default: NTH_UNIMPLEMENTED("Token: {}") <<= {current_token()};
  }

  tree.append_leaf(k, *iterator_++);
  pop_and_discard_state();
}

void Parser::HandleResolveInvocationArgumentSequence(ParseTree& tree) {
  NTH_REQUIRE(current_token().kind() == Token::Kind::RightParen);
  tree.append(ParseTree::Node::Kind::CallExpression, current_token(),
              state().back().subtree_start);
  tree.set_back_child_count();
  ++iterator_;
  pop_and_discard_state();
}

void Parser::HandleExpressionSuffix(ParseTree& tree) {
  Precedence p = Precedence::Loosest();
  switch (current_token().kind()) {
    case Token::Kind::Newline:
    case Token::Kind::Eof: pop_and_discard_state(); return;

#define IC_XMACRO_TOKEN_KIND_BINARY_ONLY_OPERATOR(kind, symbol,                \
                                                  precedence_group)            \
  case Token::Kind::kind:                                                      \
    p = Precedence::precedence_group();                                        \
    break;

#include "lexer/token_kind.xmacro.h"
    case Token::Kind::Star: p = Precedence::MultiplyDivide(); break;
    case Token::Kind::LeftParen: {
      ++iterator_;
      IgnoreAnyNewlines();
      if (current_token().kind() == Token::Kind::RightParen) {
        ExpandState(State::Kind::ResolveInvocationArgumentSequence,
                    State::Kind::ExpressionSuffix);
      } else {
        ExpandState(State::Kind::Expression,
                    State::Kind::InvocationArgumentSequence,
                    State::Kind::ExpressionSuffix);
      }
      tree.append(ParseTree::Node::Kind::InvocationArgumentStart,
                  current_token(), state().back().subtree_start);
      return;
    } break;
    case Token::Kind::Period: {
      ++iterator_;
      ExpandState(
          State{.kind          = State::Kind::ResolveMemberTerm,
                .subtree_start = tree.size() - tree.back().subtree_size},
          State{.kind          = State::Kind::ExpressionSuffix,
                .subtree_start = tree.size() - tree.back().subtree_size});
      return;
    } break;
    default: pop_and_discard_state(); return;
  }
  uint32_t subtree_start;
  State state = pop_state();
  switch (Precedence::Priority(state.ambient_precedence, p)) {
    case Priority::Left:
      NTH_REQUIRE(state_.back().kind == State::Kind::ResolveExpressionGroup);
      tree.append(ParseTree::Node::Kind::ExpressionPrecedenceGroup,
                  Token::Invalid(), state_.back().subtree_start);
      tree.set_back_child_count();
      subtree_start = state.subtree_start;
      break;
    case Priority::Same:
      NTH_REQUIRE(state_.back().kind == State::Kind::ResolveExpressionGroup);
      subtree_start = state.subtree_start;
      break;
    case Priority::Right:
      push_state({.kind          = State::Kind::ResolveExpressionGroup,
                  .token         = Token::Invalid(),
                  .subtree_start = state.subtree_start});
      // Plus one because of the infix operator we're going to append.
      subtree_start = tree.size() + 1;
      break;
    case Priority::Ambiguous:
      NTH_UNIMPLEMENTED(
          "Ambiguous precedences Precedence::{} vs Precedence::{}") <<=
          {state.ambient_precedence, p};
      break;
  }

  tree.append_leaf(ParseTree::Node::Kind::InfixOperator, *iterator_++);
  push_state(State{
      .kind               = State::Kind::Expression,
      .ambient_precedence = p,
      .subtree_start      = subtree_start,
  });
}

void Parser::HandleResolveExpressionGroup(ParseTree& tree) {
  auto state = pop_state();
  tree.append(ParseTree::Node::Kind::ExpressionPrecedenceGroup,
              Token::Invalid(), state.subtree_start);
  tree.set_back_child_count();
}

void Parser::HandleResolveUnaryExpression(ParseTree& tree) {
  auto t = pop_state().token;
  NTH_LOG((v.debug), "Token: {}") <<= {t};
}

}  // namespace ic

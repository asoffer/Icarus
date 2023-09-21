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

  // TODO: Also handle ends due to group closing.
  bool AtChunkEnd() const { return iterator_->kind() == Token::Kind::Eof; }

  Token current_token() const {
    NTH_ASSERT((v.debug), iterator_ != token_buffer_.end());
    return *iterator_;
  }

  struct State {
    enum class Kind {
#define IC_XMACRO_PARSER_STATE_KIND(state) state,
#include "parser/parse_state.xmacro.h"
    };

    friend void NthPrint(auto& p, auto&, Kind const& k) {
      switch (k) {
#define IC_XMACRO_PARSER_STATE_KIND(kind)                                      \
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
#define IC_XMACRO_PARSER_STATE_KIND(state) void Handle##state(ParseTree& tree);
#include "parser/parse_state.xmacro.h"

  std::span<State const> state() const { return state_; }
  std::span<State> state() { return state_; }

  void pop_and_discard_state() {
    NTH_ASSERT((v.debug), not state_.empty());
    state_.pop_back();
  }

  void push_state(State state) { state_.push_back(state); }

  State pop_state() {
    NTH_ASSERT((v.debug), not state_.empty());
    auto state = state_.back();
    pop_and_discard_state();
    return state;
  }

  void ExpandState(auto... states) {
    auto state = pop_state();
    int dummy;
    (dummy = ... = ExpandStateImpl(state, states));
  }

 private:
  int ExpandStateImpl(State prototype, State::Kind kind) {
    prototype.kind = kind;
    return ExpandStateImpl(prototype, prototype);
  }
  int ExpandStateImpl(State , State state) {
    state_.push_back(state);
    return 0;
  }
  std::vector<State> state_ = {
      {.kind = State::Kind::StatementSequence, .subtree_start = 0},
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
    NTH_LOG((v.debug), "\n{}:\t{}") <<= {p.current_token(), p.state()};
    switch (p.state().back().kind) {
#define IC_XMACRO_PARSER_STATE_KIND(state)                                     \
  case Parser::State::Kind::state:                                             \
    p.Handle##state(tree);                                                     \
    break;
#include "parser/parse_state.xmacro.h"
    }
  }
  return tree;
}

void Parser::HandleStatementSequence(ParseTree& tree) {
  // Ignore leading newlines.
  while (not AtChunkEnd() and current_token().kind() == Token::Kind::Newline) {
    ++iterator_;
  }

  if (AtChunkEnd()) {
    pop_and_discard_state();
    return;
  }

  ExpandState(State::Kind::Statement, State::Kind::SubsequentStatementSequence,
              State{
                  .kind          = State::Kind::ResolveStatementSequence,
                  .subtree_start = tree.size(),
              });
}

void Parser::HandleResolveStatementSequence(ParseTree& tree) {
  State state = pop_state();
  tree.append(ParseTree::Node::Kind::StatementSequence, Token::Invalid(),
              state.subtree_start);
}

void Parser::HandleSubsequentStatementSequence(ParseTree& tree) {
  if (AtChunkEnd()) {
    pop_and_discard_state();
    return;
  }
  ExpandState(State::Kind::NewlinesBetweenStatements,
              State::Kind::SubsequentStatementSequence);
}

void Parser::HandleStatement(ParseTree& tree) {
  switch (current_token().kind()) {
    case Token::Kind::Let:
    case Token::Kind::Var:
      ExpandState(State::Kind::DeclarationIdentifier,
                  State::Kind::ColonColonEqual,
                  State{
                      .kind               = State::Kind::Expression,
                      .ambient_precedence = Precedence::Loosest(),
                      .subtree_start      = tree.size(),
                  },
                  State{
                      .kind          = State::Kind::ResolveDeclaration,
                      .subtree_start = tree.size(),
                  });
      ++iterator_;
      break;
    default: ExpandState(State::Kind::Expression);
  }
}

void Parser::HandleResolveDeclaration(ParseTree& tree) {
  State state = pop_state();
  tree.append(ParseTree::Node::Kind::Declaration, state.token,
              state.subtree_start);
}

void Parser::HandleNewlinesBetweenStatements(ParseTree& tree) {
  NTH_ASSERT((v.debug), current_token().kind() == Token::Kind::Newline);
  while (not AtChunkEnd() and current_token().kind() == Token::Kind::Newline) {
    ++iterator_;
  }

  if (AtChunkEnd()) {
    pop_and_discard_state();
    return;
  } else {
    ExpandState(State::Kind::Statement);
  }
}

void Parser::HandleDeclarationIdentifier(ParseTree& tree) {
  NTH_ASSERT((v.debug), current_token().kind() == Token::Kind::Identifier);
  state()[state().size() - 4].token = current_token();
  ++iterator_;
  pop_and_discard_state();
}

void Parser::HandleColonColonEqual(ParseTree& tree) {
  NTH_ASSERT((v.debug), current_token().kind() == Token::Kind::ColonColonEqual);
  pop_and_discard_state();
  ++iterator_;
}

void Parser::HandleExpression(ParseTree& tree) {
    switch (current_token().kind()) {
      case Token::Kind::LeftParen:
      case Token::Kind::LeftBracket:
      case Token::Kind::LeftBrace:
        ExpandState(state().back(),
                    State{
                        .kind               = State::Kind::ExpressionClosing,
                        .ambient_precedence = Precedence::Loosest(),
                        .subtree_start      = tree.size(),
                    });
        ++iterator_;
        return;
      case Token::Kind::RightParen:
      case Token::Kind::RightBracket:
      case Token::Kind::RightBrace:
        NTH_UNIMPLEMENTED();
      default: break;
    }
  ExpandState(State::Kind::Term, State::Kind::ExpressionSuffix);
}

void Parser::HandleExpressionClosing(ParseTree& tree) {
  NTH_ASSERT(current_token().kind() == Token::Kind::RightParen or
             current_token().kind() == Token::Kind::RightBracket or
             current_token().kind() == Token::Kind::RightBrace);
  State state = pop_state();
  tree.append(
      ParseTree::Node::Kind::ExpressionGroup, *iterator_++,
      state.subtree_start - tree.nodes()[state.subtree_start].subtree_size - 1);
  tree.set_back_child_count();
}

void Parser::HandleTerm(ParseTree& tree) {
  ParseTree::Node::Kind k;
  switch (current_token().kind()) {
    case Token::Kind::Builtin: k = ParseTree::Node::Kind::Builtin; break;
    case Token::Kind::True:
    case Token::Kind::False: k = ParseTree::Node::Kind::BooleanLiteral; break;
    case Token::Kind::IntegerLiteral:
      k = ParseTree::Node::Kind::IntegerLiteral;
      break;
    case Token::Kind::Identifier: k = ParseTree::Node::Kind::Identifier; break;

#define IC_XMACRO_TOKEN_KIND_BUILTIN_TYPE(kind, symbol, spelling)              \
  case Token::Kind::kind:
#include "lexer/token_kind.xmacro.h"
      k = ParseTree::Node::Kind::TypeLiteral;
      break;
    default: NTH_UNIMPLEMENTED("Token: {}") <<= {current_token()};
  }

  tree.append_leaf(k, *iterator_++);
  pop_and_discard_state();
}

void Parser::HandleCallExpression(ParseTree& tree) {
  pop_and_discard_state();
  push_state({.kind          = State::Kind::ResolveCallExpression,
              .token         = Token::Invalid(),
              .subtree_start = tree.size() - 1});
  tree.append_leaf(ParseTree::Node::Kind::CallArgumentsStart, *iterator_++);
}

void Parser::HandleResolveCallExpression(ParseTree& tree) {
  NTH_ASSERT(state().back().kind == State::Kind::ResolveCallExpression);
  NTH_ASSERT(current_token().kind() == Token::Kind::RightParen);
  tree.append(ParseTree::Node::Kind::CallExpression, current_token(),
              state().back().subtree_start);
  ++iterator_;
  pop_and_discard_state();
}

void Parser::HandleExpressionSuffix(ParseTree& tree) {
  Precedence p = Precedence::Loosest();
  switch (current_token().kind()) {
    case Token::Kind::Newline:
    case Token::Kind::Eof: pop_and_discard_state(); return;
    case Token::Kind::LeftParen: {
      state().back().kind = State::Kind::CallExpression;
      return;
    }

#define IC_XMACRO_TOKEN_KIND_BINARY_ONLY_OPERATOR(kind, symbol,                \
                                                  precedence_group)            \
  case Token::Kind::kind:                                                      \
    p = Precedence::precedence_group();                                        \
    break;

#include "lexer/token_kind.xmacro.h"
    case Token::Kind::Star: p = Precedence::MultiplyDivide(); break;
    default: NTH_UNIMPLEMENTED("Token: {}") <<= {current_token()};
  }

  State state = pop_state();
  switch (Precedence::Priority(state.ambient_precedence, p)) {
    case Priority::Left:
      tree.append(ParseTree::Node::Kind::ExpressionPrecedenceGroup,
                  Token::Invalid(),
                  state.subtree_start -
                      tree.nodes()[state.subtree_start].subtree_size - 1);
      tree.set_back_child_count();
      break;
    case Priority::Same:
      NTH_ASSERT(state_.back().kind == State::Kind::ResolveExpressionGroup);
      break;
    case Priority::Right:
      push_state({.kind          = State::Kind::ResolveExpressionGroup,
                  .token         = Token::Invalid(),
                  .subtree_start = tree.size() - 1});
      break;
    case Priority::Ambiguous: NTH_UNIMPLEMENTED(); break;
  }

  tree.append(ParseTree::Node::Kind::InfixOperator, current_token(),
              state.subtree_start);
  ++iterator_;
  push_state(State{
      .kind               = State::Kind::Expression,
      .ambient_precedence = p,
      .subtree_start      = tree.size(),
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

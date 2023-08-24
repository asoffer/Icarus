#include "parser/parser.h"

#include <concepts>
#include <cstring>
#include <span>

#include "diagnostics/consumer/consumer.h"
#include "nth/debug/debug.h"
#include "parser/parse_tree.h"

namespace ic {
namespace {

struct Parser {
  explicit Parser(TokenBuffer const& token_buffer,
                  diag::DiagnosticConsumer& diagnostic_consumer)
      : iterator_(token_buffer.begin()),
        token_buffer_(token_buffer),
        diagnostic_consumer_(diagnostic_consumer) {}

  // TODO: Also handle ends due to group closing.
  bool AtChunkEnd() const { return iterator_ == token_buffer_.end(); }

  Token current_token() const {
    NTH_ASSERT((v.debug), iterator_ != token_buffer_.end());
    return *iterator_;
  }

  struct State {
    enum class Kind {
#define IC_XMACRO_PARSER_STATE_KIND(state) state,
#include "parser/parse_state.xmacro.h"
    };

    friend void NthPrint(auto& p, Kind const& k) {
      switch (k) {
#define IC_XMACRO_PARSER_STATE_KIND(kind)                                      \
  case Kind::kind:                                                             \
    p.write(#kind);                                                            \
    break;
#include "parser/parse_state.xmacro.h"
      }
    }

    Kind kind;
    Token token;
    uint32_t subtree_start = -1;

    friend void NthPrint(auto& p, State const& s) {
      nth::universal_formatter f({
          .depth    = 3,
          .fallback = "...",
      });
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

  void push_state(Token token, uint32_t current_size) {
    state_.push_back({.token = token, .subtree_start = current_size});
  }

  State pop_state() {
    NTH_ASSERT((v.debug), not state_.empty());
    auto state = state_.back();
    pop_and_discard_state();
    return state;
  }

  void ExpandState(auto... states) {
    state_.pop_back();
    int dummy;
    (dummy = ... = ExpandStateImpl(states));
  }

 private:
  int ExpandStateImpl(State::Kind kind) {
    return ExpandStateImpl(State{.kind = kind});
  }
  int ExpandStateImpl(State state) {
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
    NTH_LOG((v.debug), "{}") <<= {p.state()};
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
                  State::Kind::ColonColonEqual, State::Kind::Expression,
                  State{.kind          = State::Kind::ResolveDeclaration,
                        .subtree_start = tree.size()});
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
    case Token::Kind::True:
    case Token::Kind::False:
      tree.append_leaf(ParseTree::Node::Kind::BooleanLiteral, *iterator_++);
      break;
    case Token::Kind::Integer:
      tree.append_leaf(ParseTree::Node::Kind::IntegerLiteral, *iterator_++);
      break;
    default: NTH_UNIMPLEMENTED("Token: {}") <<= {current_token()};
  }
  pop_and_discard_state();
}

}  // namespace ic

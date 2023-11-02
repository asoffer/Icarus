#include "parse/parser.h"

#include <concepts>
#include <cstring>
#include <span>

#include "common/debug.h"
#include "diagnostics/consumer/consumer.h"
#include "nth/debug/debug.h"
#include "parse/precedence.h"
#include "parse/tree.h"
#include "parse/node_index.h"

namespace ic {
namespace {

struct Parser {
  explicit Parser(TokenBuffer const& token_buffer, ScopeTree& scope_tree,
                  diag::DiagnosticConsumer& diagnostic_consumer)
      : iterator_(token_buffer.begin()),
        token_buffer_(token_buffer),
        scope_tree_(scope_tree),
        diagnostic_consumer_(diagnostic_consumer) {}

  Token current_token() const {
    NTH_REQUIRE((v.debug), iterator_ != token_buffer_.end());
    return *iterator_;
  }

  struct State {
    enum class Kind {
#define IC_XMACRO_PARSER_STATE(state) state,
#include "parse/state.xmacro.h"
    };

    friend void NthPrint(auto& p, auto&, Kind const& k) {
      switch (k) {
#define IC_XMACRO_PARSER_STATE(kind)                                           \
  case Kind::kind:                                                             \
    p.write(#kind);                                                            \
    break;
#include "parse/state.xmacro.h"
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
#include "parse/state.xmacro.h"

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
  void HandleParenthesizedCommaSeparatedSequenceOf(State::Kind one_state,
                                                   State::Kind repeat_state,
                                                   ParseTree& tree) {
    IgnoreAnyNewlines();
    if (current_token().kind() == Token::Kind::RightParen) {
      pop_and_discard_state();
      return;
    } else if (current_token().kind() == Token::Kind::Comma) {
      ++iterator_;
      IgnoreAnyNewlines();
      ExpandState(State{.kind = one_state, .subtree_start = tree.size()},
                  repeat_state);
    } else {
      NTH_UNREACHABLE("{}") <<= {current_token().kind()};
    }
  }

  Scope::Index PushScope() {
    NTH_REQUIRE(not scope_indices_.empty());
    return scope_indices_.emplace_back(
        scope_tree_.insert_child(scope_indices_.back()));
  }

  void PopScope() {
    NTH_REQUIRE(not scope_indices_.empty());
    scope_indices_.pop_back();
  }

  void ExpandStateImpl(State prototype, State::Kind kind) {
    prototype.kind = kind;
    ExpandStateImpl(prototype, prototype);
  }
  void ExpandStateImpl(State const&, State state) { state_.push_back(state); }

  std::vector<State> state_ = {
      {.kind = State::Kind::Module, .subtree_start = 0},
      {.kind = State::Kind::Newlines, .subtree_start = 0},
  };

  TokenBuffer::const_iterator iterator_;

  TokenBuffer const& token_buffer_;
  ScopeTree& scope_tree_;
  std::vector<Scope::Index> scope_indices_ = {Scope::Index::Root()};
  diag::DiagnosticConsumer& diagnostic_consumer_;
};

struct DebugView {
  friend void NthPrint(auto& p, auto& f, DebugView const view) {
    for (auto iter = view.state.rbegin(); iter != view.state.rend(); ++iter) {
      nth::Interpolate<"    {}\n">(p, f, *iter);
    }
  }
  std::span<Parser::State const> state;
};

}  // namespace

ParseResult Parse(TokenBuffer const& token_buffer,
                  diag::DiagnosticConsumer& diagnostic_consumer) {
  ParseResult result;
  Parser p(token_buffer, result.scope_tree, diagnostic_consumer);

  while (not p.state().empty()) {
    if (result.parse_tree.size() != 0) {
      NTH_REQUIRE((v.debug), result.parse_tree.back().subtree_size > 0);
    }
    NTH_LOG((v.when(debug::parser)), "{} {}:\n{}") <<=
        {p.current_token(), p.state().back().subtree_start,
         DebugView{.state = p.state()}};
    switch (p.state().back().kind) {
#define IC_XMACRO_PARSER_STATE(state)                                          \
  case Parser::State::Kind::state:                                             \
    p.Handle##state(result.parse_tree);                                        \
    break;
#include "parse/state.xmacro.h"
    }
  }
  return result;
}

void Parser::HandleNewlines(ParseTree& tree) {
  pop_and_discard_state();
  IgnoreAnyNewlines();
}

void Parser::HandleModule(ParseTree& tree) {
  tree.append_leaf(ParseNode::Kind::FunctionStart, Token::Invalid());
  ExpandState(State{
      .kind               = State::Kind::StatementSequence,
      .ambient_precedence = Precedence::Loosest(),
      .token              = *iterator_,
      .subtree_start      = tree.size(),
  });
}

void Parser::HandleStatement(ParseTree& tree) {
  ParseNode::Kind k;
  switch (current_token().kind()) {
    case Token::Kind::Let: k = ParseNode::Kind::Let; break;
    case Token::Kind::Var: k = ParseNode::Kind::Var; break;
    case Token::Kind::If:
      ExpandState(State::Kind::ParenthesizedExpression,
                  State::Kind::BeginIfStatementTrueBranch,
                  State::Kind::BracedStatementSequence,
                  State{
                      .kind               = State::Kind::ResolveIfStatement,
                      .ambient_precedence = Precedence::Loosest(),
                      .token              = *iterator_,
                      .subtree_start      = tree.size(),
                  });
      ++iterator_;
      return;
    default:
      ExpandState(State::Kind::Expression, State::Kind::ResolveStatement);
      return;
  }

  ExpandState(
      State{.kind = State::Kind::DeclaredSymbol, .subtree_start = tree.size()},
      State{.kind = State::Kind::Declaration, .subtree_start = tree.size()});
  tree.append_leaf(k, *++iterator_);
}

void Parser::HandleBeginIfStatementTrueBranch(ParseTree& tree) {
  tree.append_leaf(ParseNode::Kind::BeginIfStatementTrueBranch, *iterator_);
  tree.back().scope_index = PushScope();
  pop_and_discard_state();
}

void Parser::HandleResolveIfStatement(ParseTree& tree) {
  PopScope();
  auto state = pop_state();
  tree.append(ParseNode::Kind::IfStatement, state.token, state.subtree_start);
}

void Parser::HandleParenthesizedExpression(ParseTree& tree) {
  if (current_token().kind() == Token::Kind::LeftParen) {
    ++iterator_;
    IgnoreAnyNewlines();
    ExpandState(
        State{
            .kind               = State::Kind::Expression,
            .ambient_precedence = Precedence::Loosest(),
            .subtree_start      = tree.size(),
        },
        State::Kind::ClosingParenthesis);
  } else {
    NTH_UNIMPLEMENTED();
  }
}

void Parser::HandleBracedStatementSequence(ParseTree& tree) {
  if (current_token().kind() == Token::Kind::LeftBrace) {
    ++iterator_;
    IgnoreAnyNewlines();
    ExpandState(
        State{
            .kind               = State::Kind::StatementSequence,
            .ambient_precedence = Precedence::Loosest(),
            .subtree_start      = tree.size(),
        },
        State::Kind::ClosingBrace);
  } else {
    NTH_UNIMPLEMENTED();
  }
}

void Parser::HandleResolveStatement(ParseTree& tree) {
  State state = pop_state();
  tree.append(ParseNode::Kind::Statement, Token::Invalid(),
              state.subtree_start);
}

void Parser::HandleStatementSequence(ParseTree& tree) {
  if (current_token().kind() == Token::Kind::Eof or
      current_token().kind() == Token::Kind::RightBrace) {
    pop_and_discard_state();
    return;
  }

  tree.append_leaf(ParseNode::Kind::ScopeStart, Token::Invalid());
  ++state().back().subtree_start;
  ExpandState(State::Kind::Statement, State::Kind::Newlines,
              State::Kind::SubsequentStatementSequence,
              State::Kind::ResolveStatementSequence);
}

void Parser::HandleSubsequentStatementSequence(ParseTree& tree) {
  if (current_token().kind() == Token::Kind::Eof or
      current_token().kind() == Token::Kind::RightBrace) {
    pop_and_discard_state();
    return;
  }

  ExpandState(State::Kind::Statement, State::Kind::Newlines,
              State::Kind::SubsequentStatementSequence);
}

void Parser::HandleResolveStatementSequence(ParseTree& tree) {
  State state = pop_state();
  auto& start = tree[ParseNodeIndex(state.subtree_start - 1)];
  NTH_REQUIRE(start.kind == ParseNode::Kind::ScopeStart);
  start.next_sibling_index = ParseNodeIndex(tree.size());
  tree.append(ParseNode::Kind::StatementSequence, Token::Invalid(),
              state.subtree_start);
}

void Parser::HandleDeclaration(ParseTree& tree) {
  ParseNode::Kind node_kind;
  State::Kind state_kind;
  switch (current_token().kind()) {
    case Token::Kind::ColonColonEqual:
      node_kind  = ParseNode::Kind::ColonColonEqual;
      state_kind = State::Kind::ResolveInferredTypeDeclaration;
      break;
    case Token::Kind::ColonEqual:
      node_kind  = ParseNode::Kind::ColonEqual;
      state_kind = State::Kind::ResolveInferredTypeDeclaration;
      break;
    case Token::Kind::ColonColon:
      node_kind  = ParseNode::Kind::ColonColon;
      state_kind = State::Kind::ResolveUninferredTypeDeclaration;
      break;
    case Token::Kind::Colon:
      node_kind  = ParseNode::Kind::Colon;
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
  tree.append(ParseNode::Kind::Declaration, state.token, state.subtree_start);
  ParseNodeIndex index(tree.size() - 1);
  auto iter = tree.child_indices(index).begin();
  ++iter;
  tree[*iter].declaration = index;
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
  tree.append_leaf(ParseNode::Kind::DeclaredIdentifier, *iterator_++);
  pop_and_discard_state();
}

void Parser::HandleExpression(ParseTree& tree) {
  switch (current_token().kind()) {
    case Token::Kind::Star:
      ++iterator_;
      ExpandState(
          State{
              .kind               = State::Kind::Expression,
              .ambient_precedence = Precedence::TightUnary(),
              .token              = current_token(),
              .subtree_start      = tree.size(),
          },
          State::Kind::ResolvePointerType);
      return;
    case Token::Kind::BracketedStar:
      ++iterator_;
      ExpandState(
          State{
              .kind               = State::Kind::Expression,
              .ambient_precedence = Precedence::TightUnary(),
              .token              = current_token(),
              .subtree_start      = tree.size(),
          },
          State::Kind::ResolveBufferPointerType);
      return;
    default:
      ExpandState(State::Kind::AtomicTerm, State::Kind::ExpressionSuffix);
  }
}

void Parser::HandleClosingBrace(ParseTree& tree) {
  IgnoreAnyNewlines();
  if (current_token().kind() == Token::Kind::RightBrace) {
    ++iterator_;
    pop_and_discard_state();
  } else {
    NTH_UNIMPLEMENTED();
  }
}

void Parser::HandleClosingParenthesis(ParseTree& tree) {
  IgnoreAnyNewlines();
  if (current_token().kind() == Token::Kind::RightParen) {
    ++iterator_;
    pop_and_discard_state();
  } else {
    NTH_UNIMPLEMENTED();
  }
}

void Parser::HandleInvocationArgumentSequence(ParseTree& tree) {
  IgnoreAnyNewlines();
  if (current_token().kind() == Token::Kind::RightParen) {
    ExpandState(State::Kind::ResolveInvocationArgumentSequence);
    return;
  } else if (current_token().kind() == Token::Kind::Comma) {
    ++iterator_;
    IgnoreAnyNewlines();
    ExpandState(
        State{.kind = State::Kind::Expression, .subtree_start = tree.size()},
        State::Kind::InvocationArgumentSequence);
  } else {
    NTH_UNREACHABLE("{}") <<= {current_token().kind()};
  }
}

void Parser::HandleCommaSeparatedExpressionSequence(ParseTree& tree) {
  HandleParenthesizedCommaSeparatedSequenceOf(
      State::Kind::Expression, State::Kind::CommaSeparatedExpressionSequence,
      tree);
}

void Parser::HandleResolveFunctionTypeParameters(ParseTree& tree) {
  tree.append(ParseNode::Kind::FunctionTypeParameters, current_token(),
              state().back().subtree_start);
  tree.set_back_child_count();
  ++iterator_;
  pop_and_discard_state();
}

void Parser::HandleResolveMemberTerm(ParseTree& tree) {
  if (current_token().kind() != Token::Kind::Identifier) {
    NTH_UNIMPLEMENTED("{}") <<= {current_token()};
  }
  tree.append(ParseNode::Kind::MemberExpression, current_token(),
              state().back().subtree_start);
  ++iterator_;
  pop_and_discard_state();
}

void Parser::HandleAtomicTerm(ParseTree& tree) {
  ParseNode::Kind k;
  switch (current_token().kind()) {
    case Token::Kind::LeftParen: {
      size_t paren_gap      = iterator_->payload();
      auto kind_after_paren = token_buffer_[paren_gap + 1].kind();
      if (kind_after_paren == Token::Kind::MinusGreater) {
        ++iterator_;
        if (std::distance(token_buffer_.begin(), iterator_) == paren_gap) {
          ExpandState(State::Kind::ResolveFunctionTypeParameters);
        } else {
          ExpandState(State::Kind::Expression,
                      State::Kind::CommaSeparatedExpressionSequence,
                      State::Kind::ResolveFunctionTypeParameters);
        }
        return;
      } else {
        NTH_UNIMPLEMENTED();
      }
    } break;
    case Token::Kind::Import:
      ++iterator_;
      ExpandState(State::Kind::Expression, State::Kind::ResolveImport);
      return;
#define IC_XMACRO_ATOM(token_kind, parse_node_kind)                            \
  case Token::Kind::token_kind:                                                \
    k = ParseNode::Kind::parse_node_kind;                                      \
    break;
#include "common/language/atoms.xmacro.h"
    default: NTH_UNIMPLEMENTED("Token: {}") <<= {current_token()};
  }

  tree.append_leaf(k, *iterator_++);
  pop_and_discard_state();
}

void Parser::HandleResolveImport(ParseTree& tree) {
  tree.append(ParseNode::Kind::Import, current_token(),
              state().back().subtree_start);
  pop_and_discard_state();
  ++iterator_;
}

void Parser::HandleResolveInvocationArgumentSequence(ParseTree& tree) {
  NTH_REQUIRE(current_token().kind() == Token::Kind::RightParen);
  tree.append(ParseNode::Kind::CallExpression, current_token(),
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
      tree.append(ParseNode::Kind::InvocationArgumentStart, current_token(),
                  state().back().subtree_start);
      if (current_token().kind() == Token::Kind::RightParen) {
        ExpandState(State::Kind::ResolveInvocationArgumentSequence,
                    State::Kind::ExpressionSuffix);
      } else {
        ExpandState(State{.kind          = State::Kind::Expression,
                          .subtree_start = tree.size()},
                    State::Kind::InvocationArgumentSequence,
                    State::Kind::ExpressionSuffix);
      }
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
      if (state_.back().kind == State::Kind::ResolveExpressionGroup) {
        tree.append(ParseNode::Kind::ExpressionPrecedenceGroup,
                    Token::Invalid(), state_.back().subtree_start);
        tree.set_back_child_count();
        subtree_start = state.subtree_start;
      } else {
        auto next_state = state_.back();
        state.ambient_precedence =
            std::next(this->state().rbegin())->ambient_precedence;
        ExpandState(next_state, state);
        return;
      }
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

  tree.append_leaf(ParseNode::Kind::InfixOperator, *iterator_++);
  push_state(State{
      .kind               = State::Kind::Expression,
      .ambient_precedence = p,
      .subtree_start      = subtree_start,
  });
}

void Parser::HandleResolveExpressionGroup(ParseTree& tree) {
  auto state = pop_state();
  tree.append(ParseNode::Kind::ExpressionPrecedenceGroup, Token::Invalid(),
              state.subtree_start);
  tree.set_back_child_count();
}

void Parser::HandleResolvePointerType(ParseTree& tree) {
  auto state = pop_state();
  tree.append(ParseNode::Kind::Pointer, Token::Invalid(), state.subtree_start);
}

void Parser::HandleResolveBufferPointerType(ParseTree& tree) {
  auto state = pop_state();
  tree.append(ParseNode::Kind::BufferPointer, Token::Invalid(),
              state.subtree_start);
}

}  // namespace ic

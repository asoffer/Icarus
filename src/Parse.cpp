#ifndef ICARUS_UNITY
#include "Lexer.h"
#include "Rule.h"
#endif

#include "GrammarRules.h"

struct ParseState {
  std::vector<Language::NodeType> node_type_stack_;
  NPtrVec node_stack_;
  NNT lookahead_;

  ParseState(const Cursor &c) : lookahead_(nullptr, Language::bof) {
    lookahead_.node = new AST::TokenNode(c);
  }

  ~ParseState() { delete lookahead_.node; }

  template <size_t N> inline Language::NodeType get_type() const {
    return node_type_stack_[node_type_stack_.size() - N];
  }

  template <size_t N> inline AST::Node *get() const {
    return node_stack_[node_stack_.size() - N];
  }
};

// Print out the debug information for the parse stack, and pause.
static void Debug(ParseState *ps) {
  // Clear the screen
  fprintf(stderr, "\033[2J\033[1;1H\n");
  for (auto x : ps->node_type_stack_) { fprintf(stderr, "%d, ", x); }
  fputs("", stderr);

  for (const auto &node_ptr : ps->node_stack_) {
    fputs(node_ptr->to_string(0).c_str(), stderr);
  }
  fgetc(stdin);
}

static void Shift(ParseState *ps, Lexer *l) {
  ps->node_type_stack_.push_back(ps->lookahead_.node_type);
  ps->node_stack_.push_back(ps->lookahead_.node);
  ps->lookahead_ = l->Next();
}

static bool ShouldShift(ParseState *ps) {
  // If the size is just 1, no rule will match so don't bother checking.
  if (ps->node_stack_.size() < 2) { return true; }

  if (ps->get_type<1>() == Language::dots) {
    return (ps->lookahead_.node_type == Language::op_bl ||
            ps->lookahead_.node_type == Language::op_l ||
            ps->lookahead_.node_type == Language::op_lt ||
            ps->lookahead_.node_type == Language::expr ||
            ps->lookahead_.node_type == Language::fn_expr ||
            ps->lookahead_.node_type == Language::l_paren ||
            ps->lookahead_.node_type == Language::l_bracket);
  }

  if (ps->lookahead_.node_type == Language::l_brace &&
      ps->get_type<1>() == Language::fn_expr &&
      ps->get_type<2>() == Language::fn_arrow) {
    return false;
  }

  if (ps->lookahead_.node_type == Language::l_brace &&
      (ps->get_type<1>() == Language::fn_expr ||
       ps->get_type<1>() == Language::kw_struct ||
       ps->get_type<1>() == Language::kw_block)) {
    return true;
  }

  if (ps->get_type<1>() == Language::newline &&
      ps->get_type<2>() == Language::comma) {
    return false;
  }

  // For now, we require struct params to be in parentheses.
  // TODO determine if this is necessary.
  if (ps->lookahead_.node_type == Language::l_paren &&
      ps->get_type<1>() == Language::kw_struct) {
    return true;
  }

  if (ps->get_type<1>() == Language::op_lt &&
      ps->lookahead_.node_type != Language::newline) {
    return true;
  }

  if (ps->get_type<1>() == Language::kw_block &&
      ps->lookahead_.node_type == Language::newline) {
    return true;
  }

  if (ps->get_type<2>() == Language::kw_block &&
      ps->get_type<1>() == Language::newline) {
    return true;
  }

  if (ps->node_stack_.size() > 2 &&
      ps->get_type<3>() == Language::kw_expr_block &&
      ps->get_type<2>() == Language::expr &&
      ps->get_type<1>() == Language::newline) {
    return true;
  }

  if (ps->lookahead_.node_type == Language::r_paren) { return false; }

  if (ps->get_type<2>() & Language::OP_) {
    auto left_prec = precedence(((AST::TokenNode *)ps->get<2>())->op);
    size_t right_prec;
    if (ps->lookahead_.node_type & Language::OP_) {
      right_prec = precedence(((AST::TokenNode *)ps->lookahead_.node)->op);

    } else if (ps->lookahead_.node_type == Language::l_paren) {
      right_prec = precedence(Language::Operator::Call);

    } else if (ps->lookahead_.node_type == Language::l_bracket) {
      right_prec = precedence(Language::Operator::Index);

    } else {
      goto end;
    }

    if (left_prec < right_prec) { return true; }
    if (left_prec > right_prec) { return false; }
    return (left_prec & assoc_mask) == right_assoc;
  }

end:
  return false;
}

static bool Reduce(ParseState *ps) {
  const Rule *matched_rule_ptr = nullptr;
  size_t debug_counter         = 0;
  size_t debug_match           = 0;

  for (const Rule &rule : Language::Rules) {
    ++debug_counter;
    // If we've already matched a rule, ignore rules of lower precedence (higher
    // integer value). I.e., 0x00 is the highest precedence. 0xff is the lowest.
    if (matched_rule_ptr != nullptr && matched_rule_ptr->prec < rule.prec) {
      continue;
    }

    if (rule.match(ps->node_type_stack_)) {
      if (!((matched_rule_ptr == nullptr ||
             rule.prec != matched_rule_ptr->prec))) {
        std::cerr << debug_counter << ", " << debug_match << std::endl;
      }

      assert((matched_rule_ptr == nullptr ||
              rule.prec != matched_rule_ptr->prec) &&
             "Two rules matched with the same precedence");

      debug_match = debug_counter;
      // Extract a pointer to the rule. It's safe to take a pointer here,
      // because
      // Language::rules is const.
      matched_rule_ptr = &rule;
    }
  }

  // If you make it to the end of the rules and still haven't matched, then
  // return false
  if (matched_rule_ptr == nullptr) { return false; }

  matched_rule_ptr->apply(ps->node_stack_, ps->node_type_stack_);

  return true;
}

void Parse(SourceFile *source) {
  // Start the lookahead with a bof token. This is a simple way to ensure proper
  // initialization, because the newline will essentially be ignored.
  Lexer lexer(source);
  ParseState state(lexer.cursor);

  // Any valid program will clean this up eventually. Therefore, shifting on the
  // bof will not hurt us. The benefit of shifting is that we have now  enforced
  // the invariant that the stack is never empty. This means we do not need to
  // check for an empty stack in the should_shift method.
  Shift(&state, &lexer);

  while (state.lookahead_.node_type != Language::eof) {
    assert(state.node_type_stack_.size() == state.node_stack_.size());
    // Shift if you are supposed to, or if you are unable to reduce.
    if (ShouldShift(&state) || !Reduce(&state)) { Shift(&state, &lexer); }

    if (debug::parser) { Debug(&state); }
  }

  // Cleanup

  // Reduce what you can
  while (Reduce(&state)) {
    if (debug::parser) { Debug(&state); }
  }

  // Shift EOF
  Shift(&state, &lexer); // Shift on the EOF token
  assert(state.get_type<1>() == Language::eof);

  // Reduce what you can again
  while (Reduce(&state)) {
    if (debug::parser) { Debug(&state); }
  }

  // Finish
  if (state.node_stack_.size() > 1) { Error::Log::Log(Cursor(), "Parse error."); }

  assert(state.get<1>()->is_statements());
  source->ast = (AST::Statements *)state.node_stack_.back();
}

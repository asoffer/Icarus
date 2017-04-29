#include "rule.h"
#include "nnt.h"
#include "grammar_rules.h"
#include "util/pstr.h"
#include "util/timer.h"
#include <iostream>

extern NNT NextToken(Cursor &cursor); // Defined in Lexer.cpp

struct ParseState {
  std::vector<Language::NodeType> node_type_stack_;
  NPtrVec node_stack_;
  NNT lookahead_;

  ParseState(const Cursor &c) : lookahead_(nullptr, Language::bof) {
    lookahead_.node.reset(new AST::TokenNode(c));
  }

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
  for (auto x : ps->node_type_stack_) { fprintf(stderr, "%lu, ", x); }
  fputs("", stderr);

  for (const auto &node_ptr : ps->node_stack_) {
    fputs(node_ptr->to_string(0).c_str(), stderr);
  }
  fgetc(stdin);
}

static void Shift(ParseState *ps, Cursor *c) {
  ps->node_type_stack_.push_back(ps->lookahead_.node_type);
  ps->node_stack_.push_back(ps->lookahead_.node.release());
  ps->lookahead_ = NextToken(*c);
}

static bool ShouldShift(ParseState *ps) {
  using namespace Language;
  // If the size is just 1, no rule will match so don't bother checking.
  if (ps->node_stack_.size() < 2) { return true; }

  if (ps->get_type<1>() == dots) {
    return ps->lookahead_.node_type &
           (op_bl | op_l | op_lt | expr | fn_expr | l_paren | l_bracket);
  }

  if (ps->lookahead_.node_type == l_brace && ps->get_type<1>() == fn_expr &&
      ps->get_type<2>() == fn_arrow) {
    return false;
  }

  if (ps->lookahead_.node_type == l_brace &&
      (ps->get_type<1>() & (fn_expr | kw_struct | kw_block))) {
    return true;
  }

  if (ps->get_type<1>() == newline && ps->get_type<2>() == comma) {
    return false;
  }

  // We require struct params to be in parentheses.
  if (ps->lookahead_.node_type == l_paren && ps->get_type<1>() == kw_struct) {
    return true;
  }

  if (ps->get_type<1>() == op_lt && ps->lookahead_.node_type != newline) {
    return true;
  }

  if (ps->get_type<1>() == kw_block && ps->lookahead_.node_type == newline) {
    return true;
  }

  if (ps->get_type<2>() == kw_block && ps->get_type<1>() == newline) {
    return true;
  }

  if (ps->node_stack_.size() > 2 && ps->get_type<3>() == kw_expr_block &&
      ps->get_type<2>() == expr && ps->get_type<1>() == newline) {
    return true;
  }

  if (ps->lookahead_.node_type == r_paren) { return false; }

  if (ps->get_type<2>() & OP_) {
    auto left_prec = precedence(((AST::TokenNode *)ps->get<2>())->op);
    size_t right_prec;
    if (ps->lookahead_.node_type & OP_) {
      right_prec = precedence(
          static_cast<AST::TokenNode *>(ps->lookahead_.node.get())->op);

    } else if (ps->lookahead_.node_type == l_paren) {
      right_prec = precedence(Operator::Call);

    } else if (ps->lookahead_.node_type == l_bracket) {
      right_prec = precedence(Operator::Index);

    } else {
      return false;
    }

    return (left_prec < right_prec) ||
           (left_prec == right_prec && (left_prec & assoc_mask) == right_assoc);
  }

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

      ASSERT(
          (matched_rule_ptr == nullptr || rule.prec != matched_rule_ptr->prec),
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

AST::Statements *Parse(File *source) {
  // Start the lookahead with a bof token. This is a simple way to ensure proper
  // initialization, because the newline will essentially be ignored.
  Cursor cursor;
  cursor.source_file = source;
  pstr temp_blank; // Blank line since we 1-index.
  cursor.source_file->lines.push_back(temp_blank);
  cursor.MoveToNextLine();

  ParseState state(cursor);

  // Any valid program will clean this up eventually. Therefore, shifting on the
  // bof will not hurt us. The benefit of shifting is that we have now enforced
  // the invariant that the stack is never empty. This means we do not need to
  // check for an empty stack in the ShouldShift method.
  Shift(&state, &cursor);

  while (state.lookahead_.node_type != Language::eof) {
    ASSERT(state.node_type_stack_.size() == state.node_stack_.size(), "");
    // Shift if you are supposed to, or if you are unable to reduce.
    if (ShouldShift(&state) || !Reduce(&state)) { Shift(&state, &cursor); }

    if (debug::parser) { Debug(&state); }
  }

  // Cleanup

  // Reduce what you can
  while (Reduce(&state)) {
    if (debug::parser) { Debug(&state); }
  }

  // Shift EOF
  Shift(&state, &cursor); // Shift on the EOF token
  ASSERT(state.get_type<1>() == Language::eof, "");

  // Reduce what you can again
  while (Reduce(&state)) {
    if (debug::parser) { Debug(&state); }
  }

  // Finish
  if (state.node_stack_.size() > 1) {
    std::vector<Cursor> lines;

    size_t last_chosen_line = 0;
    for (size_t i = 0; i < state.node_stack_.size(); ++i) {
      if (state.node_stack_[i]->loc.line_num == last_chosen_line) { continue; }
      if (state.node_type_stack_[i] &
          (Language::braced_stmts | Language::l_paren | Language::r_paren |
           Language::l_bracket | Language::r_bracket | Language::l_brace |
           Language::r_brace | Language::semicolon | Language::hashtag |
           Language::fn_arrow | Language::expr)) {
        lines.push_back(state.node_stack_[i]->loc);
        last_chosen_line = state.node_stack_[i]->loc.line_num;
      }
    }
    ErrorLog::UnknownParserError(source->name, lines);
  }

  return (AST::Statements *)state.node_stack_.back();
}

extern Timer timer;
std::map<std::string, File *> source_map;
std::vector<AST::Statements *>
ParseAllFiles(std::queue<std::string> file_names) {
  std::vector<AST::Statements *> stmts;
  while (!file_names.empty()) {
    std::string file_name = file_names.front();
    file_names.pop();

    if (source_map.find(file_name) != source_map.end()) { continue; }

    RUN(timer, "Parsing a file") {
      auto source_file      = new File(file_name);
      source_map[file_name] = source_file;
      stmts.push_back(Parse(source_file));
    }
  }
  return stmts;
}

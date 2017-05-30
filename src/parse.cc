#include "rule.h"
#include "nnt.h"
#include "grammar_rules.h"
#include "util/timer.h"
#include <iostream>
#include <queue>

#define PAUSE(dbg)                                                             \
  do {                                                                         \
    if (dbg) {                                                                 \
      std::cerr << "Paused in " << __func__ << " at line " << __LINE__;        \
      fgetc(stdin);                                                            \
    }                                                                          \
  } while (false)

extern NNT NextToken(Cursor &cursor); // Defined in Lexer.cpp

enum class ShiftState : char { NeedMore, EndOfExpr, MustReduce };
std::ostream& operator<<(std::ostream& os, ShiftState s) {
  switch (s) {
  case ShiftState::NeedMore:
    return os << "NeedMore";
  case ShiftState::EndOfExpr:
    return os << "EndOfExpr";
  case ShiftState::MustReduce:
    return os << "MustReduce";
  default:
    UNREACHABLE;
  }
}

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

  ShiftState shift_state() const {
    using namespace Language;
    // If the size is just 1, no rule will match so don't bother checking.
    if (node_stack_.size() < 2) { return ShiftState::NeedMore; }

    if (lookahead_.node_type == newline) {
      // TODO it's much more complicated than this. (braces?)
      return ShiftState::EndOfExpr;
    }

    if (get_type<1>() == dots) {
      PAUSE(debug::parser);
      return (lookahead_.node_type &
              (op_bl | op_l | op_lt | expr | fn_expr | l_paren | l_bracket))
                 ? ShiftState::NeedMore
                 : ShiftState::MustReduce;
    }

    if (lookahead_.node_type == l_brace && get_type<1>() == fn_expr &&
        get_type<2>() == fn_arrow) {
      PAUSE(debug::parser);
      return ShiftState::MustReduce;
    }

    if (lookahead_.node_type == l_brace &&
        (get_type<1>() & (fn_expr | kw_struct | kw_block))) {
      PAUSE(debug::parser);
      return ShiftState::NeedMore;
    }

    if (get_type<1>() == newline && get_type<2>() == comma) {
      PAUSE(debug::parser);
      return ShiftState::MustReduce;
    }

    // We require struct params to be in parentheses.
    if (lookahead_.node_type == l_paren && get_type<1>() == kw_struct) {
      PAUSE(debug::parser);
      return ShiftState::NeedMore;
    }

    if (get_type<1>() == op_lt && lookahead_.node_type != newline) {
      PAUSE(debug::parser);
      return ShiftState::NeedMore;
    }

    if (get_type<1>() == kw_block && lookahead_.node_type == newline) {
      PAUSE(debug::parser);
      return ShiftState::NeedMore;
    }

    if (get_type<2>() == kw_block && get_type<1>() == newline) {
      PAUSE(debug::parser);
      return ShiftState::NeedMore;
    }

    if (node_stack_.size() > 2 && get_type<3>() == kw_expr_block &&
        get_type<2>() == expr && get_type<1>() == newline) {
      PAUSE(debug::parser);
      return ShiftState::NeedMore;
    }

    if (lookahead_.node_type == r_paren) {

      PAUSE(debug::parser);
      return ShiftState::MustReduce;
    }

    if (get_type<2>() & OP_) {
      auto left_prec = precedence(((AST::TokenNode *)get<2>())->op);
      size_t right_prec;
      if (lookahead_.node_type & OP_) {
        right_prec = precedence(
            static_cast<AST::TokenNode *>(lookahead_.node.get())->op);
      } else if (lookahead_.node_type == l_paren) {
        right_prec = precedence(Operator::Call);
      } else {
        PAUSE(debug::parser);
        return ShiftState::MustReduce;
      }

      PAUSE(debug::parser);
      return (left_prec < right_prec) ||
                     (left_prec == right_prec &&
                      (left_prec & assoc_mask) == right_assoc)
                 ? ShiftState::NeedMore
                 : ShiftState::MustReduce;
    }
    PAUSE(debug::parser);
    return ShiftState::MustReduce;
  }
};

// Print out the debug information for the parse stack, and pause.
static void Debug(ParseState *ps, Cursor* cursor = nullptr) {
  // Clear the screen
  fprintf(stderr, "\033[2J\033[1;1H\n");
  if (cursor) {
    fprintf(stderr, "%s", cursor->line.c_str());
    fprintf(stderr, "%*s^\n(offset = %lu)\n\n",
            static_cast<int>(cursor->offset), "", cursor->offset);
  }
  for (auto x : ps->node_type_stack_) { fprintf(stderr, "%lu, ", x); }
  fprintf(stderr, " -> %lu", ps->lookahead_.node_type);
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

void CleanUpReduction(ParseState* state, Cursor* cursor) {
  // Reduce what you can
  while (Reduce(state)) {
    if (debug::parser) { Debug(state, cursor); }
  }

  state->node_type_stack_.push_back(Language::eof);
  state->node_stack_.push_back(new AST::TokenNode(*cursor, ""));
  state->lookahead_ =
      NNT(std::unique_ptr<AST::Node>(new AST::TokenNode(*cursor, "")),
          Language::eof);

  // Reduce what you can again
  while (Reduce(state)) {
    if (debug::parser) { Debug(state, cursor); }
  }
  if (debug::parser) { Debug(state, cursor); }
}

AST::Statements *Repl::Parse() {
  Cursor cursor;
  cursor.source_file = this;

  auto state = ParseState(cursor);
  Shift(&state, &cursor);

  while (true) {
    auto shift_state = state.shift_state();
    switch (shift_state) {
    case ShiftState::NeedMore:
      Shift(&state, &cursor);
      if (debug::parser) { Debug(&state, &cursor); }
      continue;
    case ShiftState:: EndOfExpr:
      goto done_with_expr;
    case ShiftState::MustReduce:
      Reduce(&state) || (Shift(&state, &cursor), true);
      if (debug::parser) { Debug(&state, &cursor); }
    }
  }

done_with_expr:
  CleanUpReduction(&state, &cursor);
  return (AST::Statements *)state.node_stack_.back();
}

AST::Statements *File::Parse() {
  Cursor cursor ;
  cursor.source_file = this;

  auto state = ParseState(cursor);
  Shift(&state, &cursor);

  while (state.lookahead_.node_type != Language::eof) {
    ASSERT(state.node_type_stack_.size() == state.node_stack_.size(), "");
    // Shift if you are supposed to, or if you are unable to reduce.
    if (state.shift_state() == ShiftState::NeedMore || !Reduce(&state)) {
      Shift(&state, &cursor);
    }

    if (debug::parser) { Debug(&state); }
  }

  // Cleanup
  CleanUpReduction(&state, &cursor);

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
    ErrorLog::UnknownParserError(name, lines);
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
      stmts.push_back(source_file->Parse());
    }
  }
  return stmts;
}

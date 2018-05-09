#include "log.h"

#include <iomanip>
#include <iostream>

#include "ast/declaration.h"
#include "ast/identifier.h"
#include "base/source.h"
#include "type/tuple.h"
#include "type/type.h"

extern std::unordered_map<Source::Name, File *> source_map;
extern type::Type *Err;

using LineNum          = size_t;
using FileToLineNumMap = std::unordered_map<Source::Name, std::vector<LineNum>>;
static FileToLineNumMap global_non_decl;

static inline size_t NumDigits(size_t n) {
  if (n == 0) { return 1; }
  size_t counter = 0;
  while (n != 0) {
    n /= 10;
    ++counter;
  }
  return counter;
}

static std::string LineToDisplay(size_t line_num, const Source::Line &line,
                                 size_t border_alignment = 0) {
  auto num_digits = NumDigits(line_num);
  if (border_alignment == 0) { border_alignment = num_digits; }
  ASSERT(border_alignment >= num_digits);
  return std::string(border_alignment - num_digits, ' ') +
         std::to_string(line_num) + "| " + line + "\n";
}

namespace {
// TODO templatize? and merge with texspan
struct Interval {
  size_t start = 0, past_end = 0;
};

struct IntervalSet {
  IntervalSet() = default;
  IntervalSet(std::initializer_list<Interval> intervals) {
    for (const auto &interval : intervals) { insert(interval); }
  }

  void insert(const Interval &i) {
    auto lower =
        std::lower_bound(endpoints_.begin(), endpoints_.end(), i.start);
    auto upper =
        std::upper_bound(endpoints_.begin(), endpoints_.end(), i.past_end);

    if (std::distance(lower, upper) == 0) {
      std::vector entries = {i.start, i.past_end};
      endpoints_.insert(lower, entries.begin(), entries.end());
      return;
    }

    if (std::distance(endpoints_.begin(), lower) % 2 == 0) {
      *lower = i.start;
      ++lower;
    }
    if (std::distance(endpoints_.begin(), upper) % 2 == 0) {
      --upper;
      *upper = i.past_end;
    }
    endpoints_.erase(lower, upper);
  }

  std::vector<size_t> endpoints_;
};

struct DisplayAttrs {
  enum Color : char {
    BLACK = '0',
    RED,
    GREEN,
    YELLOW,
    BLUE,
    MAGENTA,
    CYAN,
    WHITE
  } color;
  enum Effect : char { NORMAL = '0', BOLD, FAINT, ITALIC, UNDERLINE } effect;
};

std::ostream& operator<<(std::ostream& os, const DisplayAttrs& attrs) {
  return os << "\033[3" << static_cast<char>(attrs.color) << ';'
            << static_cast<char>(attrs.effect) << 'm';
}

} // namespace

static void
WriteSource(std::ostream &os, const Source &source,
            const IntervalSet &line_intervals, size_t border_alignment,
            const std::vector<std::pair<TextSpan, DisplayAttrs>> &underlines) {
  auto iter = underlines.begin();
  for (size_t i = 0; i < line_intervals.endpoints_.size(); i += 2) {
    size_t line_num = line_intervals.endpoints_[i];
    size_t end_num  = line_intervals.endpoints_[i + 1];
    while (line_num < end_num) {
      const auto &line = source.lines AT(line_num);

      // Line number
      os << "\033[97;1m" << std::right
         << std::setw(static_cast<int>(border_alignment)) << line_num
         << " | \033[0m";

      std::string_view line_view(line);
      iter = std::lower_bound(iter, underlines.end(), line_num,
                              [](const auto &span_and_attrs, size_t n) {
                                return span_and_attrs.first.start.line_num < n;
                              });

      // TODO Handle spans crossing multiple lines.
      size_t prev_start_offset = 0;
      while (iter != underlines.end() &&
             iter->first.start.line_num == line_num) {
        os << line_view.substr(prev_start_offset,
                               iter->first.start.offset - prev_start_offset);

        // TODO what if it goes for multiple lines.
        ASSERT(iter->first.start.line_num == iter->first.finish.line_num);
        ASSERT(iter->first.start.offset < iter->first.finish.offset);
        os << iter->second
           << line_view.substr(iter->first.start.offset,
                               iter->first.finish.offset -
                                   iter->first.start.offset)
           << "\033[0m";

        prev_start_offset = iter->first.finish.offset;
        ++iter;
      }
      os << line_view.substr(prev_start_offset,
                             line_view.size() - prev_start_offset)
         << "\n";

      ++line_num;
    }

    if (i + 2 != line_intervals.endpoints_.size()) {
      if (end_num + 1 == line_intervals.endpoints_[i + 2]) {
        os << "\033[97;1m" << std::right
           << std::setw(static_cast<int>(border_alignment)) << line_num << " | "
           << "\033[0m" << source.lines AT(end_num) << "\n";
      } else {
        os << "\033[97;1m" << std::right
           << std::setw(static_cast<int>(border_alignment) + 3)
           << "  ...\033[0m\n";
      }
    }
  }
}

static void DisplayErrorMessage(const char *msg_head,
                                const std::string &msg_foot,
                                const TextSpan &span, size_t underline_length) {
  auto line = source_map AT(span.source->name)->lines AT(span.start.line_num);

  size_t left_border_width = NumDigits(span.start.line_num) + 6;

  // Extra + 1 at the end because we might point after the end of the line.
  std::string underline(line.size() + left_border_width + 1, ' ');
  for (size_t i = left_border_width + span.start.offset;
       i < left_border_width + span.start.offset + underline_length; ++i) {
    underline[i] = '^';
  }

  std::cerr << msg_head << "\n\n    "
            << LineToDisplay(span.start.line_num, line) << underline << '\n';
  if (msg_foot != "") {
    std::cerr << msg_foot << "\n\n";
  } else {
    std::cerr << '\n';
  }
}

namespace ErrorLog {
void LogGeneric(const TextSpan &, const std::string &msg) { std::cerr << msg; }

void InvalidStringIndex(const TextSpan &span, const type::Type *index_type) {
  std::string msg_head = "String indexed by an invalid type. Expected an int "
                         "or uint, but encountered a " +
                         index_type->to_string() + ".";
  DisplayErrorMessage(msg_head.c_str(), "", span, 1);
}

void NonIntegralArrayIndex(const TextSpan &span, const type::Type *index_type) {
  std::string msg_head = "Array is being indexed by an expression of type " +
                         index_type->to_string() + ".";
  DisplayErrorMessage(
      msg_head.c_str(),
      "Arrays must be indexed by integral types (either int or uint)", span, 1);
}

void MissingMember(const TextSpan &span, const std::string &member_name,
                   const type::Type *t) {
  std::string msg_head = "Expressions of type `" + t->to_string() +
                         "` have no member named '" + member_name + "'.";
  DisplayErrorMessage(msg_head.c_str(), "", span, 1);
}

void IndexingNonArray(const TextSpan &span, const type::Type *t) {
  DisplayErrorMessage("Cannot index into a non-array type.",
                      "Indexed type is a `" + t->to_string() + "`.", span, 1);
}
} // namespace ErrorLog

namespace error {
void Log::UndeclaredIdentifier(AST::Identifier *id) {
  undeclared_ids_[id->token].push_back(id);
}

void Log::PostconditionNeedsBool(AST::Expression *expr) {
  errors_.push_back(
      "Function postcondition must be of type bool, but you provided "
      "an expression of type " +
      expr->type->to_string() + ".");
}

void Log::PreconditionNeedsBool(AST::Expression *expr) {
  errors_.push_back(
      "Function precondition must be of type bool, but you provided "
      "an expression of type " +
      expr->type->to_string() + ".");
}

template <typename ExprContainer>
static decltype(auto) LinesToShow(const ExprContainer &exprs) {
  IntervalSet iset;
  std::vector<std::pair<TextSpan, DisplayAttrs>> underlines;
  for (const auto &expr : exprs) {
    iset.insert(Interval{expr->span.start.line_num - 1,
                         expr->span.finish.line_num + 2});
    underlines.emplace_back(
        expr->span, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE});
  }

  return std::pair(iset, underlines);
}

#define MAKE_LOG_ERROR(fn_name, msg)                                           \
  void Log::fn_name(const TextSpan &span) {                                    \
    std::stringstream ss;                                                      \
    ss << msg "\n\n";                                                          \
    WriteSource(                                                               \
        ss, *span.source,                                                      \
        {Interval{span.start.line_num, span.finish.line_num + 1}},             \
        NumDigits(span.finish.line_num) + 2,                                   \
        {{span, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}}});   \
    ss << "\n\n";                                                              \
    errors_.push_back(ss.str());                                               \
  }
#include "errors.xmacro.h"
#undef MAKE_LOG_ERROR

void Log::RunawayMultilineComment() {
  errors_.push_back("Finished reading file during multi-line comment.\n\n");
}

void Log::DoubleDeclAssignment(const TextSpan &decl_span,
                               const TextSpan &val_span) {
  std::stringstream ss;
  ss << "Attempting to initialize an identifier that already has an initial "
        "value. Did you mean `==` instead of `=`?\n\n";
  WriteSource(
      ss, *decl_span.source,
      {Interval{decl_span.start.line_num, decl_span.finish.line_num + 1},
       Interval{val_span.start.line_num, val_span.finish.line_num + 1}},
      NumDigits(val_span.finish.line_num) + 2,
      {{decl_span, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}},
       {val_span, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}}});
  ss << "\n\n";
  errors_.push_back(ss.str());
}

void Log::DeclarationUsedInUnop(const std::string &unop,
                                const TextSpan &decl_span) {
  std::stringstream ss;
  ss << "Declarations cannot be used as argument to unary operator `" << unop
     << "`.\n\n";
  WriteSource(
      ss, *decl_span.source,
      {Interval{decl_span.start.line_num, decl_span.finish.line_num + 1}},
      NumDigits(decl_span.finish.line_num) + 2,
      {{decl_span, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}}});
  ss << "\n\n";
  errors_.push_back(ss.str());
}

void Log::ReturnTypeMismatch(const type::Type *expected_type,
                             const AST::Expression *ret_expr) {
  std::stringstream ss;
  if (ret_expr->type->is<type::Tuple>()) {
    ss << "Attempting to return multiple values";
  } else {
    ss << "Returning an expression of type `" << ret_expr->type->to_string()
       << '`';
  }
  ss << " from a function which returns `" << expected_type->to_string()
     << "`.\n\n";
  auto &span = ret_expr->span;
  // TODO also show where the return type is specified?
  WriteSource(
      ss, *span.source,
      {Interval{span.start.line_num, span.finish.line_num + 1}},
      NumDigits(span.finish.line_num) + 2,
      {{span, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}}});
  ss << "\n\n";
  errors_.push_back(ss.str());
}

void Log::NoReturnTypes(const AST::Expression *ret_expr) {
  std::stringstream ss;
  // TODO allow "return foo(...)" when foo: ??? -> ().

  ss << "Attempting to return a value when function returns nothing\n\n";
  auto &span = ret_expr->span;
  // TODO also show where the return type is specified?
  WriteSource(
      ss, *span.source,
      {Interval{span.start.line_num, span.finish.line_num + 1}},
      NumDigits(span.finish.line_num) + 2,
      {{span, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}}});
  ss << "\n\n";
  errors_.push_back(ss.str());
}

void Log::ReturningWrongNumber(const AST::Expression *ret_expr,
                               size_t num_rets) {
  std::stringstream ss;
  ss << "Attempting to return "
     << (ret_expr->type->is<type::Tuple>()
             ? ret_expr->type->as<type::Tuple>().entries_.size()
             : 1)
     << " values from a function which has " << num_rets
     << " return values.\n\n";
  auto &span = ret_expr->span;
  // TODO also show where the return type is specified?
  WriteSource(
      ss, *span.source,
      {Interval{span.start.line_num, span.finish.line_num + 1}},
      NumDigits(span.finish.line_num) + 2,
      {{span, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}}});
  ss << "\n\n";
  errors_.push_back(ss.str());
}

void Log::IndexedReturnTypeMismatch(const type::Type *expected_type,
                                    const AST::Expression *ret_expr,
                                    size_t index) {
  std::stringstream ss;
  // TODO should the slots be zero- or one-indexed?
  ss << "Returning an expression in slot #" << (index + 1) << " of type `"
     << ret_expr->type->as<type::Tuple>().entries_ AT(index)->to_string()
     << "` but function expects a value of type `" << expected_type->to_string()
     << "` in that slot.\n\n";
  auto &span = ret_expr->span;
  // TODO also show where the return type is specified?
  WriteSource(
      ss, *span.source,
      {Interval{span.start.line_num, span.finish.line_num + 1}},
      NumDigits(span.finish.line_num) + 2,
      {{span, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}}});
  ss << "\n\n";
  errors_.push_back(ss.str());
}


void Log::DereferencingNonPointer(const type::Type *type,
                                  const TextSpan &span) {
  std::stringstream ss;
  ss << "Attempting to dereference an object of type `" << type->to_string()
     << "` which is not a pointer.";
  WriteSource(
      ss, *span.source,
      {Interval{span.start.line_num, span.finish.line_num + 1}},
      NumDigits(span.finish.line_num) + 2,
      {{span, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}}});
  ss << "\n\n";
  errors_.push_back(ss.str());
}

void Log::FreeingNonPointer(const type::Type *type,
                                  const TextSpan &span) {
  std::stringstream ss;
  ss << "Attempting to free an object of type `" << type->to_string()
     << "` which is not a pointer.";
  WriteSource(
      ss, *span.source,
      {Interval{span.start.line_num, span.finish.line_num + 1}},
      NumDigits(span.finish.line_num) + 2,
      {{span, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}}});
  ss << "\n\n";
  errors_.push_back(ss.str());
}

void Log::Reserved(const TextSpan &span, const std::string &token) {
  std::stringstream ss;
  ss << "Identifier '" << token << "' is a reserved keyword.\n\n";
  WriteSource(
      ss, *span.source,
      {Interval{span.start.line_num, span.finish.line_num + 1}},
      NumDigits(span.finish.line_num) + 2,
      {{span, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}}});
  ss << "\n\n";
  errors_.push_back(ss.str());
}

void Log::NotBinary(const TextSpan &span, const std::string &token) {
  std::stringstream ss;
  ss << "Operator '" << token << "' is not a binary operator.";
  WriteSource(
      ss, *span.source,
      {Interval{span.start.line_num, span.finish.line_num + 1}},
      NumDigits(span.finish.line_num) + 2,
      {{span, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}}});
  ss << "\n\n";
  errors_.push_back(ss.str());
}

void Log::NotAType(AST::Expression *expr) {
  std::stringstream ss;
  ss << "Expression was expected to be a type, but instead it was a(n) "
     << expr->type->to_string() << ".";
  WriteSource(
      ss, *expr->span.source,
      {Interval{expr->span.start.line_num, expr->span.finish.line_num + 1}},
      NumDigits(expr->span.finish.line_num) + 2,
      {{expr->span, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}}});
  ss << "\n\n";
  errors_.push_back(ss.str());
}

void Log::AssignmentTypeMismatch(AST::Expression *lhs,
                                AST::Expression *rhs) {
   std::stringstream ss;
   ss << "Invalid assignment. Left-hand side has type "
      << lhs->type->to_string() << ", but right-hand side has type "
      << rhs->type->to_string() << ".\n\n";
   WriteSource(
       ss, *lhs->span.source,
       {Interval{lhs->span.start.line_num, lhs->span.finish.line_num + 1},
        Interval{rhs->span.start.line_num, rhs->span.finish.line_num + 1}},
       NumDigits(rhs->span.finish.line_num) + 2,
       {{lhs->span, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}},
        {rhs->span, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}}});
   ss << "\n\n";
   errors_.push_back(ss.str());
}

void Log::PositionalArgumentFollowingNamed(
    const std::vector<TextSpan> &pos_spans, const TextSpan &named_span) {
  std::stringstream ss;
  ss << "Positional function arguments cannot follow a named argument.\n\n";
  IntervalSet iset;

  std::vector<std::pair<TextSpan, DisplayAttrs>> underlines;
  // TODO do you also want to show the whole function call?
  iset.insert(
      Interval{named_span.start.line_num - 1, named_span.finish.line_num + 2});
  underlines.emplace_back(named_span, DisplayAttrs{DisplayAttrs::GREEN, DisplayAttrs::UNDERLINE});

  for (const auto &span : pos_spans) {
    iset.insert(Interval{span.start.line_num - 1, span.finish.line_num + 2});
    underlines.emplace_back(span, 
        DisplayAttrs{DisplayAttrs::GREEN, DisplayAttrs::UNDERLINE}
);
  }

  WriteSource(ss, *named_span.source, iset,
              NumDigits(pos_spans.back().finish.line_num) + 2, underlines);
  ss << "\n\n";
  errors_.push_back(ss.str());
}

void Log::UnknownParseError(const std::vector<TextSpan> &lines) {
  // TODO there's something seriously wrong with this
  std::stringstream ss;
  ss << "Parse errors found in \"" << lines.front().source->name
     << "\" on the following lines:\n\n";
  IntervalSet iset;
  for (const auto &span : lines) {
    iset.insert(Interval{span.start.line_num - 1, span.finish.line_num + 2});
  }
  WriteSource(ss, *lines.front().source, iset,
              NumDigits(lines.back().finish.line_num) + 2, {{}});
  ss << "\n\n";
  errors_.push_back(ss.str());
}

std::vector<AST::Identifier *> *Log::CyclicDependency() {
  cyc_dep_vecs_.push_back(
      std::make_unique<std::vector<AST::Identifier *>>());
  return cyc_dep_vecs_.back().get();
}

void Log::ShadowingDeclaration(const AST::Declaration &decl1,
                          const AST::Declaration &decl2) {
  // TODO migrate away from old display.
  auto line1 = source_map AT(decl1.span.source->name)
                   ->lines AT(decl1.span.start.line_num);
  auto line2 = source_map AT(decl2.span.source->name)
                   ->lines AT(decl2.span.start.line_num);
  auto line_num1 = decl1.span.start.line_num;
  auto line_num2 = decl2.span.start.line_num;
  auto align =
      std::max(size_t{4}, NumDigits(std::max(line_num1, line_num2)) + 2);
  std::cerr << "Ambiguous declarations:\n\n"
            << LineToDisplay(line_num1, line1, align) << '\n'
            << LineToDisplay(line_num2, line2, align) << '\n';
}

void Log::UserDefinedError(const std::string &err) {
  errors_.push_back(err + "\n\n");
}

void Log::Dump() const {
  for (auto &ids : cyc_dep_vecs_) {
    // TODO make cyc_dep_vec just identifiers
    std::cerr << "Found a cyclic dependency:\n\n";

    std::sort(ids->begin(), ids->end(),
              [](const AST::Identifier *lhs, const AST::Identifier *rhs) {
                if (lhs->span.start.line_num < rhs->span.start.line_num) {
                  return true;
                }
                if (lhs->span.start.line_num > rhs->span.start.line_num) {
                  return false;
                }
                if (lhs->span.start.offset < rhs->span.start.offset) {
                  return true;
                }
                if (lhs->span.start.offset > rhs->span.start.offset) {
                  return false;
                }
                if (lhs->span.finish.line_num < rhs->span.finish.line_num) {
                  return true;
                }
                if (lhs->span.finish.line_num > rhs->span.finish.line_num) {
                  return false;
                }
                return lhs->span.finish.offset < rhs->span.finish.offset;
              });

    std::unordered_map<AST::Declaration *, size_t> decls;
    for (const auto &id : *ids) {
      decls.emplace(id->as<AST::Identifier>().decl, decls.size());
    }

    IntervalSet iset;
    std::vector<std::pair<TextSpan, DisplayAttrs>> underlines;
    for (const auto &id : *ids) {
      iset.insert(
          Interval{id->span.start.line_num - 1, id->span.finish.line_num + 2});
      // TODO handle case where it's 1 mod 7 and so adjacent entries show up
      // with the same color
      underlines.emplace_back(
          id->span, DisplayAttrs{static_cast<DisplayAttrs::Color>(
                                     DisplayAttrs::RED +
                                     static_cast<char>(decls.at(id->decl) % 7)),
                                 DisplayAttrs::UNDERLINE});
    }

    WriteSource(std::cerr, *ids->front()->span.source, iset,
                NumDigits(iset.endpoints_.back() - 1) + 2, underlines);
    std::cerr << "\n\n";
  }

  for (const auto & [ decl, ids ] : out_of_order_decls_) {
    std::cerr << "Declaration of '" << decl->identifier->token
              << "' is used before it is defined (which is only allowed for "
                 "constants).\n\n";

    auto[iset, underlines] = LinesToShow(ids);
    iset.insert(Interval{decl->span.start.line_num - 1,
                         decl->span.finish.line_num + 2});
    underlines.emplace_back(
        decl->identifier->span,
        DisplayAttrs{DisplayAttrs::GREEN, DisplayAttrs::UNDERLINE});

    WriteSource(std::cerr, *ids.front()->span.source, iset,
                NumDigits(iset.endpoints_.back() - 1) + 2, underlines);
    std::cerr << "\n\n";
  }

  for (const auto & [ token, ids ] : undeclared_ids_) {
    std::cerr << "Use of undeclared identifier '" << token << "':\n";

    auto[iset, underlines] = LinesToShow(ids);
    WriteSource(std::cerr, *ids.front()->span.source, iset,
                NumDigits(iset.endpoints_.back() - 1) + 2, underlines);
    std::cerr << "\n\n";
  }

  for (const auto &err : errors_) { std::cerr << err; }
}

void Log::DeclOutOfOrder(AST::Declaration *decl, AST::Identifier *id) {
  out_of_order_decls_[decl].push_back(id);
}

} // namespace error

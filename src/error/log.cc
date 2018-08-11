#include "error/log.h"

#include <iomanip>
#include <iostream>

#include "ast/declaration.h"
#include "ast/identifier.h"
#include "frontend/source.h"
#include "type/tuple.h"
#include "type/type.h"

extern type::Type *Err;

using LineNum          = size_t;
using FileToLineNumMap = base::unordered_map<frontend::Source::Name, base::vector<LineNum>>;
static FileToLineNumMap global_non_decl;

namespace {
inline size_t NumDigits(size_t n) {
  if (n == 0) { return 1; }
  size_t counter = 0;
  while (n != 0) {
    n /= 10;
    ++counter;
  }
  return counter;
}

std::string LineToDisplay(size_t line_num, const frontend::Source::Line &line,
                                 size_t border_alignment = 0) {
  auto num_digits = NumDigits(line_num);
  if (border_alignment == 0) { border_alignment = num_digits; }
  ASSERT(border_alignment >= num_digits);
  return std::string(border_alignment - num_digits, ' ') +
         std::to_string(line_num) + "| " + line + "\n";
}

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
      auto entries = base::vector<size_t>{i.start, i.past_end};
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

  base::vector<size_t> endpoints_;
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


void
WriteSource(std::ostream &os, const frontend::Source &source,
            const IntervalSet &line_intervals, size_t border_alignment,
            const base::vector<std::pair<TextSpan, DisplayAttrs>> &underlines) {
  auto iter = underlines.begin();
  for (size_t i = 0; i < line_intervals.endpoints_.size(); i += 2) {
    size_t line_num = line_intervals.endpoints_[i];
    size_t end_num  = line_intervals.endpoints_[i + 1];
    while (line_num < end_num) {
      const auto &line = source.lines.at(line_num);

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
           << "\033[0m" << source.lines.at(end_num) << "\n";
      } else {
        os << "\033[97;1m" << std::right
           << std::setw(static_cast<int>(border_alignment) + 3)
           << "  ...\033[0m\n";
      }
    }
  }
}
} // namespace
namespace error {
void Log::UndeclaredIdentifier(AST::Identifier *id) {
  undeclared_ids_[id->token].push_back(id);
}

void Log::PostconditionNeedsBool(AST::Expression *expr) {
  std::stringstream ss;
  ss << "Function postcondition must be of type bool, but you provided an "
        "expression of type "
     << expr->type->to_string() << "\n\n";
  WriteSource(
      ss, *expr->span.source,
      {Interval{expr->span.start.line_num, expr->span.finish.line_num + 1}},
      NumDigits(expr->span.finish.line_num) + 2,
      {{expr->span, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}}});
  ss << "\n\n";
  errors_.push_back(ss.str());                                               \

}

void Log::PreconditionNeedsBool(AST::Expression *expr) {
  std::stringstream ss;
  ss << "Function precondition must be of type bool, but you provided an "
        "expression of type "
     << expr->type->to_string() << "\n\n";
  WriteSource(
      ss, *expr->span.source,
      {Interval{expr->span.start.line_num, expr->span.finish.line_num + 1}},
      NumDigits(expr->span.finish.line_num) + 2,
      {{expr->span, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}}});
  ss << "\n\n";
  errors_.push_back(ss.str());                                               \
}

template <typename ExprContainer>
static auto LinesToShow(const ExprContainer &exprs) {
  IntervalSet iset;
  base::vector<std::pair<TextSpan, DisplayAttrs>> underlines;
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

void Log::MissingMember(const TextSpan &span, const std::string &member_name,
                        const type::Type *t) {
  std::stringstream ss;
  ss << "Expressions of type `" << t->to_string() << "` have no member named `"
     << member_name << "`.\n\n";
  WriteSource(
      ss, *span.source,
      {Interval{span.start.line_num, span.finish.line_num + 1}},
      NumDigits(span.finish.line_num) + 2,
      {{span, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}}});

  ss<<"\n\n";
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
void Log::NoMatchingOperator(const std::string &op, const type::Type *lhs,
                             const type::Type *rhs, const TextSpan &span) {
  std::stringstream ss;
  ss << "No matching operator (" << op << ") with arguments of type "
     << lhs->to_string() << " and " << rhs->to_string() << "\n\n";
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
     << ret_expr->type->as<type::Tuple>().entries_.at(index)->to_string()
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
     << "` which is not a pointer.\n\n";
  WriteSource(
      ss, *span.source,
      {Interval{span.start.line_num, span.finish.line_num + 1}},
      NumDigits(span.finish.line_num) + 2,
      {{span, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}}});
  ss << "\n\n";
  errors_.push_back(ss.str());
}

void Log::WhichNonVariant(const type::Type *type, const TextSpan &span) {
  std::stringstream ss;
  ss << "Attempting to call `which` an object of type `" << type->to_string()
     << "` which is not a variant.\n\n";
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
  ss << "Operator '" << token << "' is not a binary operator.\n\n";
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
  ss << "Expression was expected to be a type or interface, but instead it was "
        "a(n) "
     << expr->type->to_string() << ".\n\n";
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
    const base::vector<TextSpan> &pos_spans, const TextSpan &named_span) {
  std::stringstream ss;
  ss << "Positional function arguments cannot follow a named argument.\n\n";
  IntervalSet iset;

  base::vector<std::pair<TextSpan, DisplayAttrs>> underlines;
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

void Log::UnknownParseError(const base::vector<TextSpan> &lines) {
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

base::vector<AST::Identifier *> *Log::CyclicDependency() {
  cyc_dep_vecs_.push_back(
      std::make_unique<base::vector<AST::Identifier *>>());
  return cyc_dep_vecs_.back().get();
}

void Log::ShadowingDeclaration(const AST::Declaration &decl1,
                          const AST::Declaration &decl2) {
  // TODO migrate away from old display.
  auto line1     = decl1.span.source->lines.at(decl1.span.start.line_num);
  auto line2     = decl2.span.source->lines.at(decl2.span.start.line_num);
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

    base::unordered_map<AST::Declaration *, size_t> decls;
    for (const auto &id : *ids) {
      decls.emplace(id->as<AST::Identifier>().decl, decls.size());
    }

    IntervalSet iset;
    base::vector<std::pair<TextSpan, DisplayAttrs>> underlines;
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

void Log::InvalidCharBufIndex(const TextSpan &span,
                              const type::Type *index_type) {
  std::stringstream ss;
  ss << "Character buffer indexed by an invalid type. Expected an int or uint, "
        "but encountered a "
     << index_type->to_string() << ".";

  WriteSource(
      ss, *span.source,
      {Interval{span.start.line_num, span.finish.line_num + 1}},
      NumDigits(span.finish.line_num) + 2,
      {{span, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}}});
  ss << "\n\n";
  errors_.push_back(ss.str());
}

void Log::NonIntegralArrayIndex(const TextSpan &span,
                                const type::Type *index_type) {
  std::stringstream ss;
  ss << "Array is being indexed by an expression of type "
     << index_type->to_string() << ".";

  WriteSource(
      ss, *span.source,
      {Interval{span.start.line_num, span.finish.line_num + 1}},
      NumDigits(span.finish.line_num) + 2,
      {{span, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}}});
  ss << "\n\n";
  errors_.push_back(ss.str());
}

void Log::IndexingNonArray(const TextSpan &span, const type::Type *t) {
  std::stringstream ss;
  ss << "Cannot index into a non-array type. Indexed type is a `"
     << t->to_string() << "`.";
  WriteSource(
      ss, *span.source,
      {Interval{span.start.line_num, span.finish.line_num + 1}},
      NumDigits(span.finish.line_num) + 2,
      {{span, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}}});
  ss << "\n\n";
  errors_.push_back(ss.str());
}

} // namespace error

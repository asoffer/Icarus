#include "error/log.h"

#include <iomanip>
#include <iostream>

#include "ast/declaration.h"
#include "ast/identifier.h"
#include "base/interval.h"
#include "misc/context.h"
#include "frontend/source.h"
#include "type/enum.h"
#include "type/opaque.h"
#include "type/struct.h"
#include "type/tuple.h"
#include "type/type.h"
#include "type/variant.h"

using LineNum = size_t;
using FileToLineNumMap = std::unordered_map<std::string, std::vector<LineNum>>;
static FileToLineNumMap global_non_decl;

std::vector<std::string> const &LoadLines(frontend::Src *src) {
  static std::unordered_map<frontend::Src *, std::vector<std::string>> lines;
  auto iter = lines.find(src);
  if (iter == lines.end()) {
    iter = lines.emplace(src, src->LoadLines()).first;
  }
  return iter->second;
}

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

std::string LineToDisplay(size_t line_num, const std::string &line,
                          size_t border_alignment = 0) {
  auto num_digits = NumDigits(line_num);
  if (border_alignment == 0) { border_alignment = num_digits; }
  ASSERT(border_alignment >= num_digits);
  return std::string(border_alignment - num_digits, ' ') +
         std::to_string(line_num) + "| " + line + "\n";
}

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

std::ostream &operator<<(std::ostream &os, const DisplayAttrs &attrs) {
  return os << "\033[3" << static_cast<char>(attrs.color) << ';'
            << static_cast<char>(attrs.effect) << 'm';
}

void WriteSource(
    std::ostream &os, frontend::Src *source,
    base::IntervalSet<size_t> const &line_intervals,
    std::vector<std::pair<TextSpan, DisplayAttrs>> const &underlines) {
  size_t border_alignment = NumDigits(line_intervals.endpoints_.back() - 1) + 2;

  auto iter = underlines.begin();
  for (size_t i = 0; i < line_intervals.endpoints_.size(); i += 2) {
    size_t line_num = line_intervals.endpoints_[i];
    size_t end_num  = line_intervals.endpoints_[i + 1];
    while (line_num < end_num) {
      const auto &line = LoadLines(source).at(line_num);

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
           << line_view.substr(
                  iter->first.start.offset,
                  iter->first.finish.offset - iter->first.start.offset)
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
           << "\033[0m" << LoadLines(source).at(end_num) << "\n";
      } else {
        os << "\033[97;1m" << std::right
           << std::setw(static_cast<int>(border_alignment) + 3)
           << "  ...\033[0m\n";
      }
    }
  }
}
}  // namespace
namespace error {
void Log::UndeclaredIdentifier(ast::Identifier *id) {
  undeclared_ids_[id->token].push_back(id);
}

void Log::PostconditionNeedsBool(TextSpan const &span, type::Type const *t) {
  std::stringstream ss;
  ss << "Function postcondition must be of type bool, but you provided an "
        "expression of type "
     << t << ".\n\n";
  WriteSource(
      ss, span.source,
      {base::Interval<size_t>{span.start.line_num, span.finish.line_num + 1}},
      {{span, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}}});
  ss << "\n\n";
  errors_.push_back(ss.str());
}

void Log::PreconditionNeedsBool(TextSpan const &span, type::Type const *t) {
  std::stringstream ss;
  ss << "Function precondition must be of type bool, but you provided an "
        "expression of type "
     << t << ".\n\n";
  WriteSource(
      ss, span.source, {span.lines()},
      {{span, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}}});
  ss << "\n\n";
  errors_.push_back(ss.str());
}

template <typename ExprContainer>
static auto LinesToShow(ExprContainer const &exprs) {
  base::IntervalSet<size_t> iset;
  std::vector<std::pair<TextSpan, DisplayAttrs>> underlines;
  for (auto const &expr : exprs) {
    iset.insert(expr->span.lines().expanded(1).clamped_below(1));
    underlines.emplace_back(
        expr->span, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE});
  }

  return std::pair(iset, underlines);
}

#define MAKE_LOG_ERROR(fn_name, msg)                                           \
  void Log::fn_name(TextSpan const &span) {                                    \
    std::stringstream ss;                                                      \
    ss << msg "\n\n";                                                          \
    WriteSource(                                                               \
        ss, span.source, {span.lines()},                                      \
        {{span, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}}});   \
    ss << "\n\n";                                                              \
    errors_.push_back(ss.str());                                               \
  }
#include "error/errors.xmacro.h"
#undef MAKE_LOG_ERROR

void Log::StatementsFollowingJump(TextSpan const &span) {
  std::stringstream ss;
  ss << "Statements cannot follow a `return` or `yield` statement.\n\n";
  WriteSource(
      ss, span.source, {span.lines().expanded(1).clamped_below(1)},
      {{span, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}}});
  ss << "\n\n";
  errors_.push_back(ss.str());
}

void Log::RunawayMultilineComment() {
  errors_.push_back("Finished reading file during multi-line comment.\n\n");
}

void Log::DoubleDeclAssignment(TextSpan const &decl_span,
                               TextSpan const &val_span) {
  std::stringstream ss;
  ss << "Attempting to initialize an identifier that already has an initial "
        "value. Did you mean `==` instead of `=`?\n\n";
  WriteSource(
      ss, decl_span.source, {decl_span.lines(), val_span.lines()},
      {{decl_span, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}},
       {val_span, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}}});
  ss << "\n\n";
  errors_.push_back(ss.str());
}

void Log::DeclarationUsedInUnop(std::string const &unop,
                                TextSpan const &decl_span) {
  std::stringstream ss;
  ss << "Declarations cannot be used as argument to unary operator `" << unop
     << "`.\n\n";
  WriteSource(
      ss, decl_span.source, {decl_span.lines()},
      {{decl_span, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}}});
  ss << "\n\n";
  errors_.push_back(ss.str());
}

void Log::MissingMember(TextSpan const &span, std::string const &member_name,
                        type::Type const *t) {
  std::stringstream ss;
  ss << "Expressions of type `" << t->to_string() << "` have no member named `"
     << member_name << "`.\n\n";
  WriteSource(
      ss, span.source, {span.lines()},
      {{span, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}}});

  ss << "\n\n";
  errors_.push_back(ss.str());
}

void Log::NonExportedMember(TextSpan const &span,
                            std::string const &member_name,
                            type::Type const *t) {
  std::stringstream ss;
  ss << "Expressions of type `" << t->to_string() << "` do not export the member `"
     << member_name << "`.\n\n";
  WriteSource(
      ss, span.source, {span.lines()},
      {{span, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}}});

  ss << "\n\n";
  errors_.push_back(ss.str());
}

void Log::ReturnTypeMismatch(type::Type const *expected_type,
                             type::Type const *actual_type,
                             TextSpan const &span) {
  std::stringstream ss;
  if (actual_type->is<type::Tuple>()) {
    ss << "Attempting to return multiple values";
  } else {
    ss << "Returning an expression of type `" << actual_type->to_string()
       << '`';
  }
  ss << " from a function which returns `" << expected_type->to_string()
     << "`.\n\n";
  // TODO also show where the return type is specified?
  WriteSource(
      ss, ASSERT_NOT_NULL(span.source), {span.lines()},
      {{span, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}}});
  ss << "\n\n";
  errors_.push_back(ss.str());
}

void Log::NoReturnTypes(ast::Expression const *ret_expr) {
  std::stringstream ss;
  // TODO allow "return foo(...)" when foo: ??? -> ().

  ss << "Attempting to return a value when function returns nothing\n\n";
  auto &span = ret_expr->span;
  // TODO also show where the return type is specified?
  WriteSource(
      ss, span.source, {span.lines()},
      {{span, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}}});
  ss << "\n\n";
  errors_.push_back(ss.str());
}

void Log::ReturningWrongNumber(TextSpan const &span, type::Type const *t,
                               size_t num_rets) {
  std::stringstream ss;
  ss << "Attempting to return "
     << (t->is<type::Tuple>() ? t->as<type::Tuple>().size() : 1)
     << " values from a function which has " << num_rets
     << " return values.\n\n";
  // TODO also show where the return type is specified?
  WriteSource(
      ss, span.source, {span.lines()},
      {{span, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}}});
  ss << "\n\n";
  errors_.push_back(ss.str());
}

void Log::IndexedReturnTypeMismatch(type::Type const *expected_type,
                                    type::Type const *actual_type,
                                    TextSpan const &span, size_t index) {
  std::stringstream ss;
  ss << "Returning an expression in slot #" << index
     << " (zero-indexed) of type `"
     << actual_type->as<type::Tuple>().entries_.at(index)->to_string()
     << "` but function expects a value of type `" << expected_type->to_string()
     << "` in that slot.\n\n";
  // TODO also show where the return type is specified?
  WriteSource(
      ss, span.source,
      {span.lines()},
      {{span, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}}});
  ss << "\n\n";
  errors_.push_back(ss.str());
}

void Log::DereferencingNonPointer(type::Type const *type,
                                  TextSpan const &span) {
  std::stringstream ss;
  ss << "Attempting to dereference an object of type `" << type->to_string()
     << "` which is not a pointer.\n\n";
  WriteSource(
      ss, span.source, {span.lines()},
      {{span, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}}});
  ss << "\n\n";
  errors_.push_back(ss.str());
}

void Log::WhichNonVariant(type::Type const *type, TextSpan const &span) {
  std::stringstream ss;
  ss << "Attempting to call `which` an object of type `" << type->to_string()
     << "` which is not a variant.\n\n";
  WriteSource(
      ss, span.source, {span.lines()},
      {{span, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}}});
  ss << "\n\n";
  errors_.push_back(ss.str());
}

void Log::Reserved(TextSpan const &span, std::string const &token) {
  std::stringstream ss;
  ss << "Identifier `" << token << "` is a reserved keyword.\n\n";
  WriteSource(
      ss, span.source, {span.lines()},
      {{span, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}}});
  ss << "\n\n";
  errors_.push_back(ss.str());
}

void Log::NotBinary(TextSpan const &span, std::string const &token) {
  std::stringstream ss;
  ss << "Operator `" << token << "` is not a binary operator.\n\n";
  WriteSource(
      ss, span.source, {span.lines()},
      {{span, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}}});
  ss << "\n\n";
  errors_.push_back(ss.str());
}

void Log::NotAType(TextSpan const& span, type::Type const* t) {
  std::stringstream ss;
  ss << "Expression was expected to be a type or interface, but instead it was "
        "a(n) " << t->to_string() << ".\n\n";
  WriteSource(
      ss, span.source, {span.lines()},
      {{span, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}}});
  ss << "\n\n";
  errors_.push_back(ss.str());
}

void Log::PositionalArgumentFollowingNamed(
    std::vector<TextSpan> const &pos_spans, TextSpan const &named_span) {
  std::stringstream ss;
  ss << "Positional function arguments cannot follow a named argument.\n\n";
  base::IntervalSet<size_t> iset;

  std::vector<std::pair<TextSpan, DisplayAttrs>> underlines;
  // TODO do you also want to show the whole function call?
  iset.insert(base::Interval<size_t>{named_span.start.line_num - 1,
                                     named_span.finish.line_num + 2});
  underlines.emplace_back(
      named_span, DisplayAttrs{DisplayAttrs::GREEN, DisplayAttrs::UNDERLINE});

  for (const auto &span : pos_spans) {
    iset.insert(base::Interval<size_t>{span.start.line_num - 1,
                                       span.finish.line_num + 2});
    underlines.emplace_back(
        span, DisplayAttrs{DisplayAttrs::GREEN, DisplayAttrs::UNDERLINE});
  }

  WriteSource(ss, named_span.source, iset, underlines);
  ss << "\n\n";
  errors_.push_back(ss.str());
}

void Log::UnknownParseError(std::vector<TextSpan> const &lines) {
  // TODO there's something seriously wrong with this
  // TODO source name?
  std::stringstream ss;
  ss << "Parse errors found in \"<SOME FILE>\" on the following lines:\n\n";
  base::IntervalSet<size_t> iset;
  for (const auto &span : lines) {
    iset.insert(base::Interval<size_t>{span.start.line_num - 1,
                                       span.finish.line_num + 2});
  }
  WriteSource(ss, lines.front().source, iset, {{}});
  ss << "\n\n";
  errors_.push_back(ss.str());
}

void Log::CyclicDependency(std::vector<ast::Identifier const *> cyc_deps) {
  cyc_dep_vecs_.push_back(std::move(cyc_deps));
}

void Log::ShadowingDeclaration(TextSpan const &span1, TextSpan const &span2) {
  // TODO migrate away from old display.
  auto line_num1 = span1.start.line_num;
  auto line_num2 = span2.start.line_num;
  auto line1     = LoadLines(ASSERT_NOT_NULL(span1.source)).at(line_num1);
  auto line2     = LoadLines(ASSERT_NOT_NULL(span2.source)).at(line_num2);
  auto align =
      std::max(size_t{4}, NumDigits(std::max(line_num1, line_num2)) + 2);
  std::stringstream ss;
  ss << "Ambiguous declarations:\n\n"
     << LineToDisplay(line_num1, line1, align) << '\n'
     << LineToDisplay(line_num2, line2, align) << '\n';
  errors_.push_back(ss.str());
}

void Log::UserDefinedError(std::string const &err) {
  errors_.push_back(err + "\n\n");
}

void Log::Dump() const {
  for (auto& cycle : cyc_dep_vecs_) {
    // TODO make cyc_dep_vec just identifiers
    std::cerr << "Found a cyclic dependency:\n\n";

    std::unordered_map<ast::Declaration const *, size_t> decls;
    for (auto const *id : cycle) { decls.emplace(id->decl_, decls.size()); }

    base::IntervalSet<size_t> iset;
    std::vector<std::pair<TextSpan, DisplayAttrs>> underlines;
    for (const auto *id : cycle) {
      iset.insert(base::Interval<size_t>{id->span.start.line_num - 1,
                                         id->span.finish.line_num + 2});
      // TODO handle case where it's 1 mod 7 and so adjacent entries show up
      // with the same color

      TextSpan decl_id_span = id->decl_->span;
      decl_id_span.finish.offset = decl_id_span.start.offset + id->token.size();
      underlines.emplace_back(
          decl_id_span,
          DisplayAttrs{static_cast<DisplayAttrs::Color>(
                           DisplayAttrs::RED +
                           static_cast<char>(decls.at(id->decl_) % 7)),
                       DisplayAttrs::UNDERLINE});
      underlines.emplace_back(
          id->span, DisplayAttrs{static_cast<DisplayAttrs::Color>(
                                     DisplayAttrs::RED +
                                     static_cast<char>(decls.at(id->decl_) % 7)),
                                 DisplayAttrs::UNDERLINE});
    }

    std::sort(underlines.begin(), underlines.end(),
              [](auto const &lhs, auto const &rhs) {
                if (lhs.first.start.line_num < rhs.first.start.line_num) {
                  return true;
                }
                if (lhs.first.start.line_num > rhs.first.start.line_num) {
                  return false;
                }
                if (lhs.first.start.offset < rhs.first.start.offset) {
                  return true;
                }
                if (lhs.first.start.offset > rhs.first.start.offset) {
                  return false;
                }
                if (lhs.first.finish.line_num < rhs.first.finish.line_num) {
                  return true;
                }
                if (lhs.first.finish.line_num > rhs.first.finish.line_num) {
                  return false;
                }
                return lhs.first.finish.offset < rhs.first.finish.offset;
              });

    WriteSource(std::cerr, cycle.front()->span.source, iset, underlines);
    std::cerr << "\n\n";
  }

  for (const auto & [ decl, ids ] : out_of_order_decls_) {
    std::cerr << "Declaration of `" << decl->id_
              << "` is used before it is defined (which is only allowed for "
                 "constants).\n\n";

    auto[iset, underlines] = LinesToShow(ids);
    iset.insert(base::Interval<size_t>{decl->span.start.line_num - 1,
                                       decl->span.finish.line_num + 2});
    // TODO highlight just the identifier
    underlines.emplace_back(
        decl->span, DisplayAttrs{DisplayAttrs::GREEN, DisplayAttrs::UNDERLINE});

    WriteSource(std::cerr, ids.front()->span.source, iset, underlines);
    std::cerr << "\n\n";
  }

  for (const auto & [ token, ids ] : undeclared_ids_) {
    std::cerr << "Use of undeclared identifier `" << token << "`:\n\n";

    auto[iset, underlines] = LinesToShow(ids);
    WriteSource(std::cerr, ids.front()->span.source, iset, underlines);
    std::cerr << "\n\n";
  }

  for (const auto &err : errors_) { std::cerr << err; }
}

void Log::DeclOutOfOrder(ast::Declaration *decl, ast::Identifier *id) {
  out_of_order_decls_[decl].push_back(id);
}

void Log::InvalidByteViewIndex(TextSpan const &span,
                               type::Type const *index_type) {
  std::stringstream ss;
  ss << "byte_view indexed by an invalid type. Expected an int or uint, but "
        "encountered a "
     << index_type->to_string() << ".";

  WriteSource(
      ss, span.source, {span.lines()},
      {{span, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}}});
  ss << "\n\n";
  errors_.push_back(ss.str());
}

void Log::InvalidIndexing(TextSpan const &span, type::Type const *t) {
  std::stringstream ss;
  ss << "Cannot index into a non-array, non-buffer type. Indexed type is a `"
     << t->to_string() << "`.\n\n";
  WriteSource(
      ss, span.source, {span.lines()},
      {{span, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}}});
  ss << "\n\n";
  errors_.push_back(ss.str());
}

void Log::InvalidIndexType(TextSpan const &span, type::Type const *t,
                           type::Type const *index_type) {
  std::stringstream ss;
  ss << "Attempting to index a value of type `" << t->to_string()
     << "` with a non-integral index. Indices must be integers, but you "
        "provided an index of type `"
     << index_type->to_string() << "`.\n\n";
  WriteSource(
      ss, span.source, {span.lines()},
      {{span, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}}});
  ss << "\n\n";
  errors_.push_back(ss.str());
}

void Log::TypeMustBeInitialized(TextSpan const &span, type::Type const *t) {
  std::stringstream ss;
  if (t->is<type::Enum>()) {
    ss << "Enums have no default initial value and must be explicitly "
          "initialized.\n\n";
  } else if (t->is<type::Opaque>()) {
    ss << "Opaque types cannot be initialized.\n\n";
  } else if (t->is<type::Variant>()) {
    ss << "Variants have no default initial value and must be explicitly "
          "initialized.\n\n";
  } else if (t->is<type::Struct>()) {
    ss << "Struct " << t
       << " has no default initial value and must be explicitly "
          "initialized.\n\n";
  } else {
    UNREACHABLE();
  }
  WriteSource(
      ss, span.source, {span.lines()},
      {{span, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}}});
  ss << "\n\n";
  errors_.push_back(ss.str());
}

void Log::ComparingIncomparables(type::Type const *lhs, type::Type const *rhs,
                                 TextSpan const &span) {
  std::stringstream ss;
  ss << "Values of type `" << lhs->to_string() << "` and `" << rhs->to_string()
     << "` are being compared but no such comparison is allowed:\n\n";
  WriteSource(
      ss, span.source, {span.lines()},
      {{span, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}}});
  ss << "\n\n";
  errors_.push_back(ss.str());
}

void Log::MismatchedAssignmentSize(TextSpan const &span, size_t lhs,
                                   size_t rhs) {
  std::stringstream ss;
  ss << "Assigning multiple values but left- and right-hand side have "
        "different numbers of elements  ("
     << lhs << " vs " << rhs << ").\n\n";
  WriteSource(
      ss, span.source, {span.lines()},
      {{span, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}}});
  ss << "\n\n";
  errors_.push_back(ss.str());
}

void Log::InvalidNumber(TextSpan const &span, std::string_view err) {
  std::stringstream ss;
  ss << err << "\n\n";
  WriteSource(
      ss, span.source, {span.lines()},
      {{span, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}}});
  ss << "\n\n";
  errors_.push_back(ss.str());
}

void Log::NoCallMatch(TextSpan const &span,
                      std::vector<std::string> const &generic_failure_reasons,
                      std::unordered_map<ast::Expression const *,
                                          std::string> const &failure_reasons) {
  std::stringstream ss;
  ss << "Failed to find a matching function signature to call.\n\n";
  WriteSource(
      ss, span.source, {span.lines()},
      {{span, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}}});

  for (std::string const &reason : generic_failure_reasons) {
    ss << "\n  * " << reason << "\n";
  }
  for (auto const & [ expr, reason ] : failure_reasons) {
    ss << "\n  * " << reason << ":\n\n";
    WriteSource(ss, expr->span.source, {expr->span.lines()}, {});
  }
  ss << "\n\n";
  errors_.push_back(ss.str());
}

void Log::MissingDispatchContingency(
    TextSpan const &span,
    std::vector<core::FnArgs<type::Type const *>> const &missing_dispatch) {
  std::stringstream ss;
  ss << "Failed to find a valid function to call for all required dispatches.\n\n";
  WriteSource(
      ss, span.source, {span.lines()},
      {{span, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}}});

  for (auto const &fnargs : missing_dispatch) {
    ss << "\n * No function taking arguments ("
       << fnargs.Transform([](type::Type const *t) { return t->to_string(); })
              .to_string()
       << ")\n";
  }
  ss << "\n";
  errors_.push_back(ss.str());
}

void Log::NotCopyable(TextSpan const &span, type::Type const *from) {
  std::stringstream ss;
  ss << "Attempting to copy an uncopyable type " << from->to_string()
     << ".\n\n";
  WriteSource(
      ss, span.source, {span.lines()},
      {{span, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}}});

  ss << "\n\n";
  errors_.push_back(ss.str());
}

void Log::NotMovable(TextSpan const &span, type::Type const *from) {
  std::stringstream ss;
  ss << "Attempting to move an immovable type " << from->to_string()
     << ".\n\n";
  WriteSource(
      ss, span.source, {span.lines()},
      {{span, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}}});

  ss << "\n\n";
  errors_.push_back(ss.str());
}

void Log::IndexingTupleOutOfBounds(TextSpan const &span, type::Tuple const *tup,
                                   size_t index) {
  std::stringstream ss;
  ss << "Tuple is indexed out of bounds. Tuple of type `" << tup->to_string()
     << "` has size " << tup->size()
     << " but you are attempting to access position " << index << ".\n\n";
  WriteSource(
      ss, span.source, {span.lines()},
      {{span, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}}});

  ss << "\n\n";
  errors_.push_back(ss.str());

}

void Log::MissingModule(std::filesystem::path const &src,
                        std::filesystem::path const &requestor) {
  std::string requestor_str = requestor.string();
  if (requestor_str.empty()) {
    requestor_str = "command line";
  } else {
    requestor_str = "\"" + requestor_str + "\"";
  }
  std::stringstream ss;
  ss << "Could not find module named \"" << src.string()
     << "\" requested from " << requestor_str << ".\n\n";
  errors_.push_back(ss.str());
}

void Log::UninferrableType(InferenceFailureReason reason,
                           TextSpan const &span) {
  std::stringstream ss;
  switch (reason) {
    case InferenceFailureReason::Inferrable: UNREACHABLE();
    case InferenceFailureReason::EmptyArray:
      ss << "Unable to infer the type of the following expression because the "
            "type of an empty array cannot be inferred. Either specify the "
            "type explicitly, or cast it to a specific array type:\n\n";
      break;
    case InferenceFailureReason::NullPtr:
      ss << "Unable to infer the type of the following expression because the "
            "type of `null` cannot be inferred. Either specify the type "
            "explicitly, or cast it to a specific pointer type:\n\n";
      break;
    case InferenceFailureReason::Hole:
      ss << "Unable to infer the type of a value that is uninitalized:\n\n";
      break;
  }
  WriteSource(
      ss, span.source, {span.lines()},
      {{span, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}}});
  ss << "\n\n";
  errors_.push_back(ss.str());
}

void Log::BuiltinError(TextSpan const &span, std::string_view text) {
  std::stringstream ss;
  ss << text << "\n\n";
  WriteSource(
      ss, span.source, {span.lines()},
      {{span, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}}});
  ss << "\n\n";
  errors_.push_back(ss.str());
}

}  // namespace error

#include "error/log.h"

#include <iomanip>
#include <iostream>

#include "absl/strings/str_format.h"
#include "ast/ast.h"
#include "base/interval.h"
#include "diagnostic/diagnostic.h"
#include "frontend/source/range.h"
#include "frontend/source/source.h"

using FileToLineNumMap =
    absl::flat_hash_map<std::string, std::vector</* line num= */ size_t>>;
static FileToLineNumMap global_non_decl;

std::vector<std::string> const &LoadLines(frontend::Source *src) {
  static absl::flat_hash_map<frontend::Source *, std::vector<std::string>>
      lines;
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

std::string LineToDisplay(frontend::LineNum line_num, const std::string &line,
                          size_t border_alignment = 0) {
  auto num_digits = NumDigits(line_num.value);
  if (border_alignment == 0) { border_alignment = num_digits; }
  ASSERT(border_alignment >= num_digits);
  return absl::StrFormat("%*s%d| %s\n", border_alignment - num_digits, " ",
                         line_num.value, line);
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
    std::ostream &os, frontend::Source *source,
    base::IntervalSet<frontend::LineNum> const &line_intervals,
    std::vector<std::pair<frontend::SourceRange, DisplayAttrs>> const
        &underlines) {
  size_t border_alignment =
      NumDigits(line_intervals.endpoints_.back().value - 1) + 2;

  auto iter = underlines.begin();
  for (size_t i = 0; i < line_intervals.endpoints_.size(); i += 2) {
    auto line_num = frontend::LineNum(line_intervals.endpoints_[i]);
    auto end_num  = frontend::LineNum(line_intervals.endpoints_[i + 1]);
    while (line_num < end_num) {
      const auto &line = LoadLines(source).at(line_num.value);

      // Line number
      // TODO wrap this into a useful function.
      os << "\033[97;1m" << std::right
         << std::setw(static_cast<int>(border_alignment)) << line_num.value
         << " | \033[0m";

      std::string_view line_view(line);
      iter =
          std::lower_bound(iter, underlines.end(), line_num,
                           [](auto const &span_and_attrs, frontend::LineNum n) {
                             return span_and_attrs.first.begin().line_num < n;
                           });

      // TODO Handle spans crossing multiple lines.
      size_t prev_start_offset = 0;
      while (iter != underlines.end() &&
             iter->first.begin().line_num == line_num) {
        os << line_view.substr(
            prev_start_offset,
            (iter->first.begin().offset - prev_start_offset).value);

        // TODO what if it goes for multiple lines.
        ASSERT(iter->first.begin().line_num == iter->first.end().line_num);
        ASSERT(iter->first.begin().offset < iter->first.end().offset);
        os << iter->second
           << line_view.substr(iter->first.begin().offset.value,
                               iter->first.end().offset.value -
                                   iter->first.begin().offset.value)
           << "\033[0m";

        prev_start_offset = iter->first.end().offset.value;
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
           << std::setw(static_cast<int>(border_alignment)) << line_num.value
           << " | "
           << "\033[0m" << LoadLines(source).at(end_num.value) << "\n";
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
void Log::UndeclaredIdentifier(ast::Identifier const *id) {
  undeclared_ids_[std::string{id->token()}].push_back(id);
}

void Log::PostconditionNeedsBool(frontend::SourceRange const &range,
                                 std::string_view type) {
  renderer_.AddError(diagnostic::Diagnostic(
      diagnostic::Text("Function postcondition must be of type bool, but you "
                       "provided an expression of type `%s`."),
      diagnostic::SourceQuote(src_).Highlighted(range, diagnostic::Style{})));
}

void Log::PreconditionNeedsBool(frontend::SourceRange const &range,
                                std::string_view type) {
  renderer_.AddError(diagnostic::Diagnostic(
      diagnostic::Text(
          "Function precondition must be of type bool, but you provided an "
          "expression of type %s.",
          type),
      diagnostic::SourceQuote(src_).Highlighted(range, diagnostic::Style{})));
}

template <typename ExprContainer>
static auto LinesToShow(ExprContainer const &exprs) {
  base::IntervalSet<frontend::LineNum> iset;
  std::vector<std::pair<frontend::SourceRange, DisplayAttrs>> underlines;
  for (auto const &expr : exprs) {
    iset.insert(
        expr->span.lines().expanded(1).clamped_below(frontend::LineNum(1)));
    underlines.emplace_back(
        expr->span, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE});
  }

  return std::pair(iset, underlines);
}

#define MAKE_LOG_ERROR(fn_name, msg)                                           \
  void Log::fn_name(frontend::SourceRange const &range) {                      \
    renderer_.AddError(diagnostic::Diagnostic(                                 \
        diagnostic::Text(msg), diagnostic::SourceQuote(src_).Highlighted(      \
                                   range, diagnostic::Style{})));              \
  }
#include "error/errors.xmacro.h"
#undef MAKE_LOG_ERROR

void Log::StatementsFollowingJump(frontend::SourceRange const &range) {
  renderer_.AddError(diagnostic::Diagnostic(
      diagnostic::Text(
          "Statements cannot follow a `return` or `yield` statement."),
      diagnostic::SourceQuote(src_).Highlighted(range, diagnostic::Style{})));
}

void Log::RunawayMultilineComment() {
  renderer_.AddError(diagnostic::Diagnostic(
      diagnostic::Text("Finished reading file during multi-line comment.")));
}

void Log::DoubleDeclAssignment(frontend::SourceRange const &decl_range,
                               frontend::SourceRange const &val_range) {
  renderer_.AddError(diagnostic::Diagnostic(
      diagnostic::Text(
          "Attempting to initialize an identifier that already has an initial "
          "value. Did you mean `==` instead of `=`?"),
      diagnostic::SourceQuote(src_)
          .Highlighted(decl_range, diagnostic::Style{})
          .Highlighted(val_range, diagnostic::Style{})));
}

void Log::DeclarationUsedInUnop(std::string const &unop,
                                frontend::SourceRange const &decl_range) {
  renderer_.AddError(diagnostic::Diagnostic(
      diagnostic::Text(
          "Declarations cannot be used as argument to unary operator `%s`.",
          unop),
      diagnostic::SourceQuote(src_).Highlighted(decl_range,
                                                diagnostic::Style{})));
}

void Log::MissingMember(frontend::SourceRange const &range,
                        std::string_view member_name, std::string_view type) {
  renderer_.AddError(diagnostic::Diagnostic(
      diagnostic::Text("Expressions of type `%s` have no member named `%s`.",
                       type, member_name),
      diagnostic::SourceQuote(src_).Highlighted(range, diagnostic::Style{})));
}

void Log::NonExportedMember(frontend::SourceRange const &range,
                            std::string_view member_name,
                            std::string_view type) {
  renderer_.AddError(diagnostic::Diagnostic(
      diagnostic::Text(
          "Expressions of type `%s` do not export the member `%s`.", type,
          member_name),
      diagnostic::SourceQuote(src_).Highlighted(range, diagnostic::Style{})));
}

void Log::ReturnTypeMismatch(std::string_view expected_type,
                             std::string_view actual_type,
                             frontend::SourceRange const &range) {
  // TODO also show where the return type is specified?
  renderer_.AddError(diagnostic::Diagnostic(
      diagnostic::Text("Returning an expression of type `%s` from a function "
                       "which returns `%s`.",
                       actual_type, expected_type),
      diagnostic::SourceQuote(src_).Highlighted(range, diagnostic::Style{})));
}

void Log::NoReturnTypes(ast::ReturnStmt const *ret_expr) {
  std::stringstream ss;
  // TODO allow "return foo(...)" when foo: ??? -> ().

  ss << "Attempting to return a value when function returns nothing\n\n";
  auto &range = ret_expr->span;
  // TODO also show where the return type is specified?
  WriteSource(
      ss, src_, {range.lines()},
      {{range, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}}});
  ss << "\n\n";
  errors_.push_back(ss.str());
}

void Log::ReturningWrongNumber(frontend::SourceRange const &range,
                               size_t actual, size_t expected) {
  std::stringstream ss;
  ss << "Attempting to return " << actual
     << " values from a function which has " << expected
     << " return values.\n\n";
  // TODO also show where the return type is specified?
  WriteSource(
      ss, src_, {range.lines()},
      {{range, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}}});
  ss << "\n\n";
  errors_.push_back(ss.str());
}

void Log::IndexedReturnTypeMismatch(std::string_view expected_type,
                                    std::string_view actual_type,
                                    frontend::SourceRange const &range,
                                    size_t index) {
  std::stringstream ss;
  ss << "Returning an expression in slot #" << index
     << " (zero-indexed) of type `" << actual_type
     << "` but function expects a value of type `" << expected_type
     << "` in that slot.\n\n";
  // TODO also show where the return type is specified?
  WriteSource(
      ss, src_, {range.lines()},
      {{range, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}}});
  ss << "\n\n";
  errors_.push_back(ss.str());
}

void Log::DereferencingNonPointer(std::string_view type,
                                  frontend::SourceRange const &range) {
  renderer_.AddError(diagnostic::Diagnostic(
      diagnostic::Text("Attempting to dereference an object of type `%s` which "
                       "is not a pointer",
                       type),
      diagnostic::SourceQuote(src_).Highlighted(range, diagnostic::Style{})));
}

void Log::WhichNonVariant(std::string_view type,
                          frontend::SourceRange const &range) {
  renderer_.AddError(diagnostic::Diagnostic(
      diagnostic::Text("Attempting to call `which` an object of type `%s` "
                       "which is not a variant.",
                       type),
      diagnostic::SourceQuote(src_).Highlighted(range, diagnostic::Style{})));
}

void Log::Reserved(frontend::SourceRange const &range,
                   std::string const &token) {
  std::stringstream ss;
  ss << "Identifier `" << token << "` is a reserved keyword.\n\n";
  WriteSource(
      ss, src_, {range.lines()},
      {{range, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}}});
  ss << "\n\n";
  errors_.push_back(ss.str());
}

void Log::NotBinary(frontend::SourceRange const &range,
                    std::string const &token) {
  std::stringstream ss;
  ss << "Operator `" << token << "` is not a binary operator.\n\n";
  WriteSource(
      ss, src_, {range.lines()},
      {{range, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}}});
  ss << "\n\n";
  errors_.push_back(ss.str());
}

void Log::NotAType(frontend::SourceRange const &range, std::string_view type) {
  std::stringstream ss;
  ss << "Expression was expected to be a type, but instead it was a(n) " << type
     << ".\n\n";
  WriteSource(
      ss, src_, {range.lines()},
      {{range, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}}});
  ss << "\n\n";
  errors_.push_back(ss.str());
}

void Log::PositionalArgumentFollowingNamed(
    std::vector<frontend::SourceRange> const &pos_ranges,
    frontend::SourceRange const &named_range) {
  std::stringstream ss;
  ss << "Positional function arguments cannot follow a named argument.\n\n";
  base::IntervalSet<frontend::LineNum> iset;

  std::vector<std::pair<frontend::SourceRange, DisplayAttrs>> underlines;
  // TODO do you also want to show the whole function call?
  iset.insert(named_range.lines().expanded(1));
  underlines.emplace_back(
      named_range, DisplayAttrs{DisplayAttrs::GREEN, DisplayAttrs::UNDERLINE});

  for (const auto &range : pos_ranges) {
    iset.insert(named_range.lines().expanded(1));
    underlines.emplace_back(
        range, DisplayAttrs{DisplayAttrs::GREEN, DisplayAttrs::UNDERLINE});
  }

  WriteSource(ss, src_, iset, underlines);
  ss << "\n\n";
  errors_.push_back(ss.str());
}

void Log::UnknownParseError(std::vector<frontend::SourceRange> const &lines) {
  // TODO there's something seriously wrong with this
  // TODO source name?
  std::stringstream ss;
  ss << "Parse errors found in \"<SOME FILE>\" on the following lines:\n\n";
  base::IntervalSet<frontend::LineNum> iset;
  for (const auto &range : lines) { iset.insert(range.lines().expanded(1)); }
  WriteSource(ss, src_, iset, {{}});
  ss << "\n\n";
  errors_.push_back(ss.str());
}

void Log::CyclicDependency(std::vector<ast::Identifier const *> cyc_deps) {
  cyc_dep_vecs_.push_back(std::move(cyc_deps));
}

void Log::ShadowingDeclaration(frontend::SourceRange const &span1,
                               frontend::SourceRange const &span2) {
  // TODO migrate away from old display.
  auto line_num1 = span1.begin().line_num;
  auto line_num2 = span2.begin().line_num;
  auto line1     = LoadLines(src_).at(line_num1.value);
  auto line2     = LoadLines(src_).at(line_num2.value);
  auto align =
      std::max(size_t{4}, NumDigits(std::max(line_num1, line_num2).value) + 2);
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
  for (auto &cycle : cyc_dep_vecs_) {
    // TODO make cyc_dep_vec just identifiers
    std::cerr << "Found a cyclic dependency:\n\n";

    absl::flat_hash_map<ast::Declaration const *, size_t> decls;
    for (auto const *id : cycle) { decls.emplace(id->decl(), decls.size()); }

    base::IntervalSet<frontend::LineNum> iset;
    std::vector<std::pair<frontend::SourceRange, DisplayAttrs>> underlines;
    for (const auto *id : cycle) {
      iset.insert(id->span.lines().expanded(1));
      // TODO handle case where it's 1 mod 7 and so adjacent entries show up
      // with the same color

      frontend::SourceRange decl_id_range = id->decl()->span;
      decl_id_range.end().offset =
          decl_id_range.begin().offset + id->token().size();
      underlines.emplace_back(
          decl_id_range,
          DisplayAttrs{static_cast<DisplayAttrs::Color>(
                           DisplayAttrs::RED +
                           static_cast<char>(decls.at(id->decl()) % 7)),
                       DisplayAttrs::UNDERLINE});
      underlines.emplace_back(
          id->span,
          DisplayAttrs{static_cast<DisplayAttrs::Color>(
                           DisplayAttrs::RED +
                           static_cast<char>(decls.at(id->decl()) % 7)),
                       DisplayAttrs::UNDERLINE});
    }

    std::sort(underlines.begin(), underlines.end(),
              [](auto const &lhs, auto const &rhs) {
                if (lhs.first.begin() < rhs.first.begin()) { return true; }
                if (lhs.first.begin() > rhs.first.begin()) { return false; }
                return lhs.first.end() < rhs.first.end();
              });

    // TODO src_ is probably wrong
    WriteSource(std::cerr, src_, iset, underlines);
    std::cerr << "\n\n";
  }

  for (auto const &[decl, ids] : out_of_order_decls_) {
    std::cerr << "Variable `" << decl->id()
              << "` is used before it is defined (which is only allowed for "
                 "constants).\n\n";

    auto [iset, underlines] = LinesToShow(ids);
    iset.insert(
        decl->span.lines().expanded(1).clamped_below(frontend::LineNum(1)));
    // TODO highlight just the identifier
    underlines.emplace_back(
        decl->span, DisplayAttrs{DisplayAttrs::GREEN, DisplayAttrs::UNDERLINE});

    // TODO src_ is probably wrong
    WriteSource(std::cerr, src_, iset, underlines);
    std::cerr << "\n\n";
  }

  for (const auto &[token, ids] : undeclared_ids_) {
    std::cerr << "Use of undeclared identifier `" << token << "`:\n\n";

    auto [iset, underlines] = LinesToShow(ids);
    // TODO src_ is probably wrong
    WriteSource(std::cerr, src_, iset, underlines);
    std::cerr << "\n\n";
  }

  for (const auto &err : errors_) { std::cerr << err; }
}

void Log::DeclOutOfOrder(ast::Declaration const *decl,
                         ast::Identifier const *id) {
  out_of_order_decls_[decl].push_back(id);
}

void Log::InvalidIndexing(frontend::SourceRange const &range,
                          std::string_view type) {
  std::stringstream ss;
  ss << "Cannot index into a non-array, non-buffer type. Indexed type is a `"
     << type << "`.\n\n";
  WriteSource(
      ss, src_, {range.lines()},
      {{range, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}}});
  ss << "\n\n";
  errors_.push_back(ss.str());
}

void Log::InvalidIndexType(frontend::SourceRange const &range,
                           std::string_view type, std::string_view index_type) {
  std::stringstream ss;
  ss << "Attempting to index a value of type `" << type
     << "` with a non-integral index. Indices must be integers, but you "
        "provided an index of type `"
     << index_type << "`.\n\n";
  WriteSource(
      ss, src_, {range.lines()},
      {{range, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}}});
  ss << "\n\n";
  errors_.push_back(ss.str());
}

void Log::TypeMustBeInitialized(frontend::SourceRange const &range,
                                std::string_view type) {
  std::stringstream ss;
  ss << "There is no default value for the type `" << type << "`.\n\n";
  WriteSource(
      ss, src_, {range.lines()},
      {{range, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}}});
  ss << "\n\n";
  errors_.push_back(ss.str());
}

void Log::ComparingIncomparables(std::string_view lhs, std::string_view rhs,
                                 frontend::SourceRange const &range) {
  std::stringstream ss;
  ss << "Values of type `" << lhs << "` and `" << rhs
     << "` are being compared but no such comparison is allowed:\n\n";
  WriteSource(
      ss, src_, {range.lines()},
      {{range, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}}});
  ss << "\n\n";
  errors_.push_back(ss.str());
}

void Log::MismatchedAssignmentSize(frontend::SourceRange const &range,
                                   size_t lhs, size_t rhs) {
  std::stringstream ss;
  ss << "Assigning multiple values but left- and right-hand side have "
        "different numbers of elements  ("
     << lhs << " vs " << rhs << ").\n\n";
  WriteSource(
      ss, src_, {range.lines()},
      {{range, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}}});
  ss << "\n\n";
  errors_.push_back(ss.str());
}

void Log::InvalidNumber(frontend::SourceRange const &range,
                        std::string_view err) {
  std::stringstream ss;
  ss << err << "\n\n";
  WriteSource(
      ss, src_, {range.lines()},
      {{range, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}}});
  ss << "\n\n";
  errors_.push_back(ss.str());
}

void Log::NoCallMatch(frontend::SourceRange const &range,
                      std::vector<std::string> const &generic_failure_reasons,
                      absl::flat_hash_map<ast::Expression const *,
                                          std::string> const &failure_reasons) {
  std::stringstream ss;
  ss << "Failed to find a matching function signature to call.\n\n";
  WriteSource(
      ss, src_, {range.lines()},
      {{range, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}}});

  for (std::string const &reason : generic_failure_reasons) {
    ss << "\n  * " << reason << "\n";
  }
  for (auto const &[expr, reason] : failure_reasons) {
    ss << "\n  * " << reason << ":\n\n";
    WriteSource(ss, src_, {expr->span.lines()}, {});
  }
  ss << "\n\n";
  errors_.push_back(ss.str());
}

void Log::MissingDispatchContingency(
    frontend::SourceRange const &range,
    std::vector<core::FnArgs<std::string>> const &missing_dispatch) {
  std::stringstream ss;
  ss << "Failed to find a valid function to call for all required "
        "dispatches.\n\n";
  WriteSource(
      ss, src_, {range.lines()},
      {{range, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}}});

  for (auto const &fnargs : missing_dispatch) {
    ss << "\n * No function taking arguments (" << fnargs.to_string() << ")\n";
  }
  ss << "\n";
  errors_.push_back(ss.str());
}

void Log::NotCopyable(frontend::SourceRange const &range,
                      std::string_view from) {
  std::stringstream ss;
  ss << "Attempting to copy an uncopyable type " << from << ".\n\n";
  WriteSource(
      ss, src_, {range.lines()},
      {{range, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}}});

  ss << "\n\n";
  errors_.push_back(ss.str());
}

void Log::NotMovable(frontend::SourceRange const &range,
                     std::string_view from) {
  std::stringstream ss;
  ss << "Attempting to move an immovable type " << from << ".\n\n";
  WriteSource(
      ss, src_, {range.lines()},
      {{range, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}}});

  ss << "\n\n";
  errors_.push_back(ss.str());
}

void Log::IndexingTupleOutOfBounds(frontend::SourceRange const &range,
                                   std::string_view tup, size_t tup_size,
                                   size_t index) {
  std::stringstream ss;
  ss << "Tuple is indexed out of bounds. Tuple of type `" << tup
     << "` has size " << tup_size
     << " but you are attempting to access position " << index << ".\n\n";
  WriteSource(
      ss, src_, {range.lines()},
      {{range, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}}});

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
  ss << "Could not find module named \"" << src.string() << "\" requested from "
     << requestor_str << ".\n\n";
  errors_.push_back(ss.str());
}

void Log::UninferrableType(InferenceFailureReason reason,
                           frontend::SourceRange const &range) {
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
      ss, src_, {range.lines()},
      {{range, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}}});
  ss << "\n\n";
  errors_.push_back(ss.str());
}

void Log::MismatchedBinopArithmeticType(std::string_view lhs,
                                        std::string_view rhs,
                                        frontend::SourceRange const &range) {
  renderer_.AddError(diagnostic::Diagnostic(
      diagnostic::Text("MismatchedTypes `%s` and `%s` in binary operator.", lhs,
                       rhs),
      diagnostic::SourceQuote(src_).Highlighted(range, diagnostic::Style{})));

  std::stringstream ss;
  ss << "Mismatched types `" << lhs << "` and `" << rhs
     << "` in binary operator.\n\n";
  WriteSource(
      ss, src_, {range.lines()},
      {{range, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}}});

  ss << "\n\n";
  errors_.push_back(ss.str());
}

void Log::InvalidCast(std::string_view from, std::string_view to,
                      frontend::SourceRange const &range) {
  std::stringstream ss;
  ss << "No viable cast from `" << from << "` to `" << to << "`.\n\n";
  WriteSource(
      ss, src_, {range.lines()},
      {{range, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}}});

  ss << "\n\n";
  errors_.push_back(ss.str());
}

void Log::PrintMustReturnVoid(std::string_view type,
                              frontend::SourceRange const &range) {
  std::stringstream ss;
  ss << "`print` must return void, but evaluates to an object of type " << type
     << ".\n\n";
  WriteSource(
      ss, src_, {range.lines()},
      {{range, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}}});

  ss << "\n\n";
  errors_.push_back(ss.str());
}

void Log::SwitchConditionNeedsBool(std::string_view type,
                                   frontend::SourceRange const &range) {
  std::stringstream ss;
  ss << "Expressionless switch conditions must evaluate to a `bool`, but you "
        "provided a `"
     << type << "`.\n\n";
  WriteSource(
      ss, src_, {range.lines()},
      {{range, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}}});

  ss << "\n\n";
  errors_.push_back(ss.str());
}

void Log::BuiltinError(frontend::SourceRange const &range,
                       std::string_view text) {
  std::stringstream ss;
  ss << text << "\n\n";
  WriteSource(
      ss, src_, {range.lines()},
      {{range, DisplayAttrs{DisplayAttrs::RED, DisplayAttrs::UNDERLINE}}});
  ss << "\n\n";
  errors_.push_back(ss.str());
}

}  // namespace error

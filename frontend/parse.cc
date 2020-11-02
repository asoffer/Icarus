#include <array>
#include <cstdio>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "absl/strings/str_format.h"
#include "ast/ast.h"
#include "base/debug.h"
#include "base/global.h"
#include "diagnostic/consumer/consumer.h"
#include "diagnostic/message.h"
#include "frontend/lex/lex.h"
#include "frontend/lex/operators.h"
#include "frontend/lex/tagged_node.h"
#include "frontend/lex/token.h"
#include "frontend/parse_rule.h"
#include "frontend/source/source.h"

namespace debug {
bool parser = false;
}  // namespace debug

namespace frontend {
namespace {

struct TodoDiagnostic {
  static constexpr std::string_view kCategory = "todo";
  static constexpr std::string_view kName     = "todo";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("TODO: Diagnostic emit from %s, line %u.",
                         loc.file_name(), loc.line()));
  }

  std::experimental::source_location loc =
      std::experimental::source_location::current();
};

template <typename T>
struct InheritsFrom : public matcher::UntypedMatcher<InheritsFrom<T>> {
  template <typename Expr>
  struct Matcher : public ::matcher::Matcher<Expr> {
    Matcher(InheritsFrom const &m) {}
    bool match(Expr const &input) const override {
      if constexpr (matcher::is_pointery<Expr>::value) {
        return dynamic_cast<T const *>(
                   matcher::is_pointery<Expr>{}.get(input)) != nullptr;
      }
    }
    std::string describe(bool positive) const override {
      return (positive ? "inherits from " : "does not inherit from ") +
             std::string(typeid(T).name());
    }
  };
};

struct AccessRhsNotIdentifier {
  static constexpr std::string_view kCategory = "parse-error";
  static constexpr std::string_view kName     = "access-rhs-not-identifier";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Right-hand side must be an identifier"),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  frontend::SourceRange range;
};

// TODO: do we want to talk about this as already having a label, or giving it
// multiple labels? "multiple labels" seems clearer because it doesn't refer to
// parser state.
struct ScopeNodeAlreadyHasLabel {
  static constexpr std::string_view kCategory = "parse-error";
  static constexpr std::string_view kName     = "scope-already-has-label";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("This scope already has a label."),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  frontend::SourceRange label_range;
  frontend::SourceRange range;
};

struct ReservedKeyword {
  static constexpr std::string_view kCategory = "parse-error";
  static constexpr std::string_view kName     = "reserved-keyword";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Identifier `%s` is a reserved keyword.", keyword),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  frontend::SourceRange range;
  std::string keyword;
};

struct CallingDeclaration {
  static constexpr std::string_view kCategory = "parse-error";
  static constexpr std::string_view kName     = "calling-declaration";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Declarations cannot be called"),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  frontend::SourceRange range;
};

struct IndexingDeclaration {
  static constexpr std::string_view kCategory = "parse-error";
  static constexpr std::string_view kName     = "indexing-declaration";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Declarations cannot be indexed"),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  frontend::SourceRange range;
};

struct NonDeclarationInStruct {
  static constexpr std::string_view kCategory = "parse-error";
  static constexpr std::string_view kName     = "non-declaration-in-struct";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "Each struct member must be defined using a declaration."),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  frontend::SourceRange range;
};

struct UnknownParseError {
  static constexpr std::string_view kCategory = "parse-error";
  static constexpr std::string_view kName     = "unknown-parse-error";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    diagnostic::SourceQuote quote(src);
    for (auto const &range : lines) {
      quote.Highlighted(range, diagnostic::Style{});
    }
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "Parse errors found in \"<SOME FILE>\" on the following lines:"),
        std::move(quote));
  }

  std::vector<frontend::SourceRange> lines;
};

struct CommaSeparatedListStatement {
  static constexpr std::string_view kCategory = "parse-error";
  static constexpr std::string_view kName = "comma-separated-list-statement";

  diagnostic::DiagnosticMessage ToMessage(Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Comma-separated lists are not allowed as statements"),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  SourceRange range;
};

struct DeclarationUsedInUnaryOperator {
  static constexpr std::string_view kCategory = "parse-error";
  static constexpr std::string_view kName =
      "declaration-used-in-unary-operator";

  diagnostic::DiagnosticMessage ToMessage(Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "Declarations cannot be used as argument to unary operator."),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  SourceRange range;
};

struct PositionalArgumentFollowingNamed {
  static constexpr std::string_view kCategory = "parse-error";
  static constexpr std::string_view kName =
      "positional-argument-followed-by-named";

  diagnostic::DiagnosticMessage ToMessage(Source const *src) const {
    diagnostic::SourceQuote quote(src);
    quote.Highlighted(last_named, diagnostic::Style{});
    for (auto const &pos_range : pos_ranges) {
      quote.Highlighted(pos_range, diagnostic::Style{});
    }
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "Positional function arguments cannot follow a named argument."),
        std::move(quote));
  }

  std::vector<SourceRange> pos_ranges;
  SourceRange last_named;
};

struct UnknownBuiltinHashtag {
  static constexpr std::string_view kCategory = "parse-error";
  static constexpr std::string_view kName     = "unknown-builtin-hashtag";

  diagnostic::DiagnosticMessage ToMessage(Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Unknown builtin hashtag #%s", token),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  std::string token;
  SourceRange range;
};

struct BracedShortFunctionLiteral {
  static constexpr std::string_view kCategory = "parse-error";
  static constexpr std::string_view kName     = "braced-short-function-literal";

  diagnostic::DiagnosticMessage ToMessage(Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Unexpected braces in short function literal."),
        diagnostic::SourceQuote(src)
            .Highlighted(open_brace, diagnostic::Style::ErrorText())
            .Highlighted(close_brace, diagnostic::Style::ErrorText()),
        diagnostic::Text(
            "Short function literals do not use braces in Icarus. Rather than "
            "writing `(n: int64) => { n * n }`, remove the braces and write "
            "`(n: int64) => n * n`."));
  }

  SourceRange open_brace, close_brace;
};

std::unique_ptr<ast::Identifier> MakeInvalidNode(
    SourceRange range = SourceRange()) {
  return std::make_unique<ast::Identifier>(range, "invalid_node");
}

struct Statements : public ast::Node {
  Statements(SourceRange const &range) : ast::Node(range) {}
  ~Statements() override {}
  Statements(Statements &&) noexcept = default;
  Statements &operator=(Statements &&) noexcept = default;

  void Accept(ast::VisitorBase *visitor, void *ret,
              void *arg_tuple) const override {
    visitor->ErasedVisit(this, ret, arg_tuple);
  }

  void set_range(SourceRange const &range) { range_ = range; }

  size_t size() const { return content_.size(); }
  void append(std::unique_ptr<ast::Node> &&node) {
    if (auto *stmts = node->if_as<Statements>()) {
      content_.insert(content_.end(),
                      std::make_move_iterator(stmts->content_.begin()),
                      std::make_move_iterator(stmts->content_.end()));
    } else {
      content_.push_back(std::move(node));
    }
  }
  std::vector<std::unique_ptr<ast::Node>> extract() && {
    return std::move(content_);
  }
  std::vector<std::unique_ptr<ast::Node>> content_;
};

struct CommaList : ast::Expression {
  explicit CommaList(SourceRange const &range = {}) : ast::Expression(range) {}
  ~CommaList() override {}

  CommaList(CommaList const &) noexcept = default;
  CommaList(CommaList &&) noexcept      = default;
  CommaList &operator=(CommaList const &) noexcept = default;
  CommaList &operator=(CommaList &&) noexcept = default;

  void Accept(ast::VisitorBase *visitor, void *ret,
              void *arg_tuple) const override {
    visitor->ErasedVisit(this, ret, arg_tuple);
  }

  std::vector<std::unique_ptr<ast::Expression>> &&extract() && {
    return std::move(exprs_);
  }

  std::vector<std::unique_ptr<ast::Expression>> exprs_;
};

template <typename To, typename From>
std::unique_ptr<To> move_as(std::unique_ptr<From> &val) {
  ASSERT(val, InheritsFrom<To>());
  return std::unique_ptr<To>(static_cast<To *>(val.release()));
}

void ValidateStatementSyntax(ast::Node *node,
                             diagnostic::DiagnosticConsumer &diag) {
  if (auto *cl = node->if_as<CommaList>()) {
    diag.Consume(CommaSeparatedListStatement{.range = cl->range()});
    // TODO: Do we call this more than once?
  }
}

constexpr size_t left_assoc  = 0;
constexpr size_t right_assoc = 1;
constexpr size_t non_assoc   = 2;
constexpr size_t chain_assoc = 3;
constexpr size_t assoc_mask  = 3;

constexpr size_t precedence(Operator op) {
  switch (op) {
#define OPERATOR_MACRO(name, symbol, tag, prec, assoc)                         \
  case Operator::name:                                                         \
    return (((prec) << 2) + (assoc));
#include "frontend/lex/operators.xmacro.h"
#undef OPERATOR_MACRO
  }

  // Use the builtin directly rather than `UNREACHABLE()` because
  // `UNREACHABLE()` cannot be used in constexpr.
  __builtin_unreachable();
}

std::unique_ptr<ast::Node> AddHashtag(
    absl::Span<std::unique_ptr<ast::Node>> nodes,
    diagnostic::DiagnosticConsumer &diag) {
  auto expr              = move_as<ast::Expression>(nodes.back());
  std::string_view token = nodes.front()->as<Token>().token;

  for (auto [name, tag] : ir::BuiltinHashtagsByName) {
    if (token == name) {
      expr->hashtags.insert(tag);
      return expr;
    }
  }

  if (token.front() == '{' or token.back() == '}') {
    diag.Consume(UnknownBuiltinHashtag{.token = std::string{token},
                                       .range = nodes.front()->range()});
  } else {
    // TODO: User-defined hashtag.
  }
  return expr;
}

std::unique_ptr<ast::Node> OneBracedStatement(
    absl::Span<std::unique_ptr<ast::Node>> nodes,
    diagnostic::DiagnosticConsumer &diag) {
  auto stmts = std::make_unique<Statements>(
      SourceRange(nodes.front()->range().begin(), nodes.back()->range().end()));
  stmts->append(std::move(nodes[1]));
  ValidateStatementSyntax(stmts->content_.back().get(), diag);
  return stmts;
}

std::unique_ptr<ast::Node> EmptyBraces(
    absl::Span<std::unique_ptr<ast::Node>> nodes,
    diagnostic::DiagnosticConsumer &) {
  return std::make_unique<Statements>(
      SourceRange(nodes.front()->range().begin(), nodes.back()->range().end()));
}

std::unique_ptr<ast::Node> BuildControlHandler(
    std::unique_ptr<ast::Node> node) {
  auto &tk = node->as<Token>().token;
  if (tk == "return") {
    return std::make_unique<ast::ReturnStmt>(node->range());
  }
  UNREACHABLE();
}

std::unique_ptr<ast::Node> BracedStatementsSameLineEnd(
    absl::Span<std::unique_ptr<ast::Node>> nodes,
    diagnostic::DiagnosticConsumer &diag) {
  auto stmts = move_as<Statements>(nodes[1]);
  stmts->set_range(
      SourceRange(nodes.front()->range().begin(), nodes.back()->range().end()));
  if (nodes[2]->is<Statements>()) {
    for (auto &stmt : nodes[2]->as<Statements>().content_) {
      stmts->append(std::move(stmt));
      ValidateStatementSyntax(stmts->content_.back().get(), diag);
    }
  } else {
    stmts->append(std::move(nodes[2]));
    ValidateStatementSyntax(stmts->content_.back().get(), diag);
  }
  return stmts;
}

std::unique_ptr<ast::Node> BracedStatementsJumpSameLineEnd(
    absl::Span<std::unique_ptr<ast::Node>> nodes,
    diagnostic::DiagnosticConsumer &diag) {
  nodes[2] = BuildControlHandler(std::move(nodes[2]));
  return BracedStatementsSameLineEnd(std::move(nodes), diag);
}

std::unique_ptr<ast::Node> BuildRightUnop(
    absl::Span<std::unique_ptr<ast::Node>> nodes,
    diagnostic::DiagnosticConsumer &diag) {
  const std::string &tk = nodes[1]->as<Token>().token;
  if (tk == ":?") {
    SourceRange range(nodes[0]->range().begin(), nodes[1]->range().end());
    auto unop = std::make_unique<ast::UnaryOperator>(
        range, ast::UnaryOperator::Kind::TypeOf,
        move_as<ast::Expression>(nodes[0]));

    if (unop->operand()->is<ast::Declaration>()) {
      diag.Consume(
          DeclarationUsedInUnaryOperator{.range = unop->operand()->range()});
    }

    return unop;
  } else {
    UNREACHABLE();
  }
}

std::unique_ptr<ast::Node> BuildCallImpl(
    SourceRange range, std::unique_ptr<ast::Expression> callee,
    std::unique_ptr<ast::Expression> args_expr,
    diagnostic::DiagnosticConsumer &diag) {
  if (not args_expr) {
    return std::make_unique<ast::Call>(range, std::move(callee),
                                       core::OrderedFnArgs<ast::Expression>{});
  }

  std::vector<std::pair<std::string, std::unique_ptr<ast::Expression>>> args;
  if (auto *cl = args_expr->if_as<CommaList>()) {
    std::optional<SourceRange> last_named_range_before_error = std::nullopt;
    std::vector<SourceRange> positional_error_ranges;

    for (auto &expr : cl->exprs_) {
      if (auto *a = expr->if_as<ast::Assignment>()) {
        if (positional_error_ranges.empty()) {
          last_named_range_before_error = a->lhs()[0]->range();
        }
        // TODO: Error if there are multiple entries in this assignment.
        auto [lhs, rhs] = std::move(*a).extract();
        args.emplace_back(std::string{lhs[0]->as<ast::Identifier>().name()},
                          std::move(rhs[0]));
      } else {
        if (last_named_range_before_error.has_value()) {
          positional_error_ranges.push_back(expr->range());
        }
        args.emplace_back("", std::move(expr));
      }
    }

    if (not positional_error_ranges.empty()) {
      diag.Consume(PositionalArgumentFollowingNamed{
          .pos_ranges = positional_error_ranges,
          .last_named = *last_named_range_before_error});
    }
  } else {
    if (auto *a = args_expr->if_as<ast::Assignment>()) {
      auto [lhs, rhs] = std::move(*a).extract();
      // TODO: Error if there are multiple entries in this assignment.
      args.emplace_back(std::string{lhs[0]->as<ast::Identifier>().name()},
                        std::move(rhs[0]));
    } else {
      args.emplace_back("", std::move(args_expr));
    }
  }

  if (callee->is<ast::Declaration>()) {
    diag.Consume(CallingDeclaration{.range = callee->range()});
  }

  return std::make_unique<ast::Call>(
      range, std::move(callee),
      core::OrderedFnArgs<ast::Expression>(std::move(args)));
}

std::unique_ptr<ast::Node> BuildCall(
    absl::Span<std::unique_ptr<ast::Node>> nodes,
    diagnostic::DiagnosticConsumer &diag) {
  SourceRange range(nodes.front()->range().begin(),
                    nodes.back()->range().end());
  return BuildCallImpl(range, move_as<ast::Expression>(nodes[0]),
                       move_as<ast::Expression>(nodes[2]), diag);
}

std::unique_ptr<ast::Node> BuildLeftUnop(
    absl::Span<std::unique_ptr<ast::Node>> nodes,
    diagnostic::DiagnosticConsumer &diag) {
  const std::string &tk = nodes[0]->as<Token>().token;

  if (tk == "import") {
    auto range =
        SourceRange(nodes[0]->range().begin(), nodes[1]->range().end());
    return std::make_unique<ast::Import>(range,
                                         move_as<ast::Expression>(nodes[1]));
  } else if (tk == "'") {
    SourceRange range(nodes.front()->range().begin(),
                      nodes.back()->range().end());
    return BuildCallImpl(range, move_as<ast::Expression>(nodes[1]), nullptr,
                         diag);
  } else if (tk == "$") {
    SourceRange range(nodes[0]->range().begin(), nodes[1]->range().end());
    if (auto *id = nodes[1]->if_as<ast::Identifier>()) {
      return std::make_unique<ast::ArgumentType>(range,
                                                 std::move(*id).extract());
    } else {
      diag.Consume(TodoDiagnostic{});
      return std::make_unique<ast::ArgumentType>(range, "");
    }
  }

  static base::Global kUnaryOperatorMap =
      absl::flat_hash_map<std::string_view, ast::UnaryOperator::Kind>{
          {"copy", ast::UnaryOperator::Kind::Copy},
          {"init", ast::UnaryOperator::Kind::Init},
          {"move", ast::UnaryOperator::Kind::Move},
          {"[*]", ast::UnaryOperator::Kind::BufferPointer},
          {":?", ast::UnaryOperator::Kind::TypeOf},
          {"`", ast::UnaryOperator::Kind::Evaluate},
          {"&", ast::UnaryOperator::Kind::Address},
          {"@", ast::UnaryOperator::Kind::At},
          {"*", ast::UnaryOperator::Kind::Pointer},
          {"-", ast::UnaryOperator::Kind::Negate},
          {"!", ast::UnaryOperator::Kind::Not}};

  SourceRange range(nodes[0]->range().begin(), nodes[1]->range().end());

  auto &operand = nodes[1];
  auto op       = kUnaryOperatorMap->find(tk)->second;

  if (operand->is<ast::Declaration>()) {
    diag.Consume(DeclarationUsedInUnaryOperator{.range = range});
    return std::make_unique<ast::UnaryOperator>(
        range, op, MakeInvalidNode(nodes[1]->range()));

  } else if (not operand->is<ast::Expression>()) {
    diag.Consume(TodoDiagnostic{});
    return std::make_unique<ast::UnaryOperator>(
        range, op, MakeInvalidNode(nodes[1]->range()));

  } else {
    return std::make_unique<ast::UnaryOperator>(
        range, op, move_as<ast::Expression>(nodes[1]));
  }
}

std::unique_ptr<ast::Node> BuildLabeledYield(
    absl::Span<std::unique_ptr<ast::Node>> nodes,
    diagnostic::DiagnosticConsumer &diag) {
  auto range =
      SourceRange(nodes.front()->range().begin(), nodes.back()->range().end());

  std::vector<std::unique_ptr<ast::Expression>> exprs;
  if (nodes.size() > 2) {
    ASSERT(nodes.size() == 3);
    if (auto *cl = nodes[2]->if_as<CommaList>();
        cl and not cl->parenthesized_) {
      exprs = std::move(*cl).extract();
    } else {
      exprs.push_back(move_as<ast::Expression>(nodes[2]));
    }
  }

  auto stmts = std::make_unique<Statements>(range);
  stmts->append(std::make_unique<ast::YieldStmt>(
      range, std::move(exprs), move_as<ast::Label>(nodes[0])));
  return stmts;
}

std::unique_ptr<ast::Node> BuildUnlabeledYield(
    absl::Span<std::unique_ptr<ast::Node>> nodes,
    diagnostic::DiagnosticConsumer &diag) {
  auto range =
      SourceRange(nodes.front()->range().begin(), nodes.back()->range().end());
  std::vector<std::unique_ptr<ast::Expression>> exprs;
  if (auto *cl = nodes[1]->if_as<CommaList>(); cl and not cl->parenthesized_) {
    exprs = std::move(*cl).extract();
  } else {
    exprs.push_back(move_as<ast::Expression>(nodes[1]));
  }

  auto stmts = std::make_unique<Statements>(range);
  stmts->append(std::make_unique<ast::YieldStmt>(range, std::move(exprs)));
  return stmts;
}

std::unique_ptr<ast::Node> BuildChainOp(
    absl::Span<std::unique_ptr<ast::Node>> nodes,
    diagnostic::DiagnosticConsumer &diag) {
  auto op = nodes[1]->as<Token>().op;
  std::unique_ptr<ast::ComparisonOperator> chain;

  // Add to a chain so long as the precedence levels match. The only thing at
  // that precedence level should be the operators which can be chained.
  if (nodes[0]->is<ast::ComparisonOperator>() and
      precedence(nodes[0]->as<ast::ComparisonOperator>().ops().front()) ==
          precedence(op)) {
    chain = move_as<ast::ComparisonOperator>(nodes[0]);

  } else {
    SourceRange range(nodes[0]->range().begin(), nodes[2]->range().end());
    chain = std::make_unique<ast::ComparisonOperator>(
        range, move_as<ast::Expression>(nodes[0]));
  }

  chain->append(op, move_as<ast::Expression>(nodes[2]));
  return chain;
}

std::unique_ptr<ast::Node> BuildCommaList(
    absl::Span<std::unique_ptr<ast::Node>> nodes,
    diagnostic::DiagnosticConsumer &diag) {
  std::unique_ptr<CommaList> comma_list = nullptr;
  if (nodes[0]->is<CommaList>() and
      not nodes[0]->as<CommaList>().parenthesized_) {
    comma_list = move_as<CommaList>(nodes[0]);
  } else {
    comma_list = std::make_unique<CommaList>(
        SourceRange(nodes[0]->range().begin(), nodes[2]->range().end()));
    comma_list->exprs_.push_back(move_as<ast::Expression>(nodes[0]));
  }
  comma_list->exprs_.push_back(move_as<ast::Expression>(nodes[2]));
  comma_list->range().end() = comma_list->exprs_.back()->range().end();
  return comma_list;
}

std::unique_ptr<ast::Node> BuildAccess(
    absl::Span<std::unique_ptr<ast::Node>> nodes,
    diagnostic::DiagnosticConsumer &diag) {
  auto range = SourceRange(nodes[0]->range().begin(), nodes[2]->range().end());
  auto &&operand = move_as<ast::Expression>(nodes[0]);
  if (not nodes[2]->is<ast::Identifier>()) {
    diag.Consume(AccessRhsNotIdentifier{.range = nodes[2]->range()});
    return std::make_unique<ast::Access>(range, std::move(operand),
                                         "invalid_node");
  }

  return std::make_unique<ast::Access>(
      range, std::move(operand),
      std::string{nodes[2]->as<ast::Identifier>().name()});
}

std::unique_ptr<ast::Node> BuildIndexOperator(
    absl::Span<std::unique_ptr<ast::Node>> nodes,
    diagnostic::DiagnosticConsumer &diag) {
  auto range =
      SourceRange(nodes.front()->range().begin(), nodes.back()->range().end());
  auto index =
      std::make_unique<ast::Index>(range, move_as<ast::Expression>(nodes[0]),
                                   move_as<ast::Expression>(nodes[2]));

  if (index->lhs()->is<ast::Declaration>()) {
    diag.Consume(IndexingDeclaration{.range = nodes[0]->range()});
  }

  // TODO: This check is correct except that we're using indexes as a temporary
  // state for building args in block nodes. Also this needs to check deeper.
  // For example, commalists could have declarations in them and we wouldn't
  // catch it. Probably we should have a frontend-only temporary node that
  // converts to an index or block node once we're sure we know which it is.
  // if (index->rhs()->is<ast::Declaration>()) {
  //   error_log->DeclarationInIndex(index->rhs()->range());
  // }

  return index;
}

std::unique_ptr<ast::Node> BuildEmptyArray(
    absl::Span<std::unique_ptr<ast::Node>> nodes,
    diagnostic::DiagnosticConsumer &diag) {
  return std::make_unique<ast::ArrayLiteral>(
      SourceRange(nodes.front()->range().begin(), nodes.back()->range().end()),
      std::vector<std::unique_ptr<ast::Expression>>{});
}

std::unique_ptr<ast::Node> BuildEmptyCommaList(
    absl::Span<std::unique_ptr<ast::Node>> nodes,
    diagnostic::DiagnosticConsumer &diag) {
  return std::make_unique<CommaList>(
      SourceRange(nodes[0]->range().begin(), nodes[1]->range().end()));
}

std::unique_ptr<ast::Node> BuildArrayLiteral(
    absl::Span<std::unique_ptr<ast::Node>> nodes,
    diagnostic::DiagnosticConsumer &diag) {
  if (auto *cl = nodes[1]->if_as<CommaList>(); cl and not cl->parenthesized_) {
    return std::make_unique<ast::ArrayLiteral>(nodes[0]->range(),
                                               std::move(*cl).extract());
  } else {
    return std::make_unique<ast::ArrayLiteral>(
        nodes[0]->range(), move_as<ast::Expression>(nodes[1]));
  }
}

std::unique_ptr<ast::Node> BuildArrayType(
    absl::Span<std::unique_ptr<ast::Node>> nodes,
    diagnostic::DiagnosticConsumer &diag) {
  if (auto *cl = nodes[1]->if_as<CommaList>(); cl and not cl->parenthesized_) {
    auto range = SourceRange(nodes.front()->range().begin(),
                             nodes.back()->range().end());
    return std::make_unique<ast::ArrayType>(range, std::move(*cl).extract(),
                                            move_as<ast::Expression>(nodes[3]));
  } else {
    return std::make_unique<ast::ArrayType>(nodes[0]->range(),
                                            move_as<ast::Expression>(nodes[1]),
                                            move_as<ast::Expression>(nodes[3]));
  }
}

template <bool IsConst>
std::unique_ptr<ast::Node> BuildDeclaration(
    absl::Span<std::unique_ptr<ast::Node>> nodes,
    diagnostic::DiagnosticConsumer &diag) {
  auto op = nodes[1]->as<Token>().op;
  SourceRange decl_range(nodes.front()->range().begin(),
                         nodes.back()->range().end());
  std::string id;
  SourceRange id_range;
  if (nodes[0]->is<ast::Identifier>()) {
    id_range = nodes[0]->range();
    id       = std::string(nodes[0]->as<ast::Identifier>().name());
  }

  std::unique_ptr<ast::Expression> type_expr, init_val;
  if (op == Operator::Colon or op == Operator::DoubleColon) {
    type_expr = move_as<ast::Expression>(nodes[2]);
  } else {
    init_val = move_as<ast::Expression>(nodes[2]);
  }

  bool initial_value_is_hole = false;
  if (auto const *init_id = init_val->if_as<ast::Identifier>()) {
    if (init_id->name() == "") { initial_value_is_hole = true; }
  }

  return std::make_unique<ast::Declaration>(
      decl_range, std::move(id), id_range, std::move(type_expr),
      std::move(init_val),
      (initial_value_is_hole ? ast::Declaration::f_InitIsHole : 0) |
          (IsConst ? ast::Declaration::f_IsConst : 0));
}

static std::vector<std::unique_ptr<ast::Expression>> ExtractIfCommaList(
    std::unique_ptr<ast::Expression> expr) {
  std::vector<std::unique_ptr<ast::Expression>> exprs;
  if (auto *e = expr->if_as<CommaList>()) {
    exprs = std::move(*e).extract();
  } else {
    exprs.push_back(std::move(expr));
  }
  return exprs;
}

std::vector<std::unique_ptr<ast::Declaration>> ExtractInputs(
    std::vector<std::unique_ptr<ast::Expression>> params,
    diagnostic::DiagnosticConsumer &diag) {
  std::vector<std::unique_ptr<ast::Declaration>> inputs;
  inputs.reserve(params.size());

  for (auto &expr : params) {
    if (expr->is<ast::Declaration>()) {
      inputs.push_back(move_as<ast::Declaration>(expr));
    } else {
      diag.Consume(TodoDiagnostic{});
    }
  }
  return inputs;
}

std::vector<std::unique_ptr<ast::Declaration>> ExtractInputs(
    std::unique_ptr<ast::Expression> params,
    diagnostic::DiagnosticConsumer &diag) {
  return ExtractInputs(ExtractIfCommaList(std::move(params)), diag);
}

std::unique_ptr<ast::Node> BuildFunctionLiteral(
    SourceRange const &range,
    std::vector<std::unique_ptr<ast::Declaration>> inputs,
    std::vector<std::unique_ptr<ast::Expression>> output, Statements &&stmts,
    diagnostic::DiagnosticConsumer &diag) {
  return std::make_unique<ast::FunctionLiteral>(
      range, std::move(inputs), std::move(stmts).extract(), std::move(output));
}

std::unique_ptr<ast::Node> BuildFunctionLiteral(
    SourceRange const &range,
    std::vector<std::unique_ptr<ast::Declaration>> inputs,
    std::unique_ptr<ast::Expression> output, Statements &&stmts,
    diagnostic::DiagnosticConsumer &diag) {
  if (output == nullptr) {
    return std::make_unique<ast::FunctionLiteral>(range, std::move(inputs),
                                                  std::move(stmts).extract());
  } else {
    return BuildFunctionLiteral(range, std::move(inputs),
                                ExtractIfCommaList(std::move(output)),
                                std::move(stmts), diag);
  }
}

// Represents a sequence of the form:
// `<expr> <infix-operator> <braced-statements>`
// The only valid expressions of this form are designated initializers, where
// `expr` is the type being initialized and the infix operator is `.`. However,
// we should not assume that designated initializers are the intent here. For
// example, The sequence `(n: int64) => { n * n }` would trigger this rule and
// more clearly shows that the user likely expected a short-function literal.
//
// Currently, we use the operator as the signal about user intent.
std::unique_ptr<ast::Node> BuildDesignatedInitializer(
    absl::Span<std::unique_ptr<ast::Node>> nodes,
    diagnostic::DiagnosticConsumer &diag) {
  auto *tok = nodes[1]->if_as<Token>();
  SourceRange range(nodes[0]->range().begin(), nodes.back()->range().end());
  if (not tok) {
    diag.Consume(TodoDiagnostic{});
    return MakeInvalidNode(range);
  }

  // TODO: This is either in the wrong place, or this function is poorly named.
  if (tok->token == "=>") {
    diag.Consume(BracedShortFunctionLiteral{
        .open_brace  = SourceRange(nodes.back()->range().begin(), 1),
        .close_brace = SourceRange(nodes.back()->range().end(), -1),
    });
    return MakeInvalidNode(range);
  }

  if (tok->token != ".") {
    diag.Consume(TodoDiagnostic{});
    return MakeInvalidNode(range);
  }

  if (auto *stmts = nodes[2]->if_as<Statements>()) {
    auto extracted_stmts = std::move(*stmts).extract();
    std::vector<std::unique_ptr<ast::Assignment>> initializers;
    initializers.reserve(extracted_stmts.size());
    for (auto &stmt : extracted_stmts) {
      if (auto const *assignment = stmt->if_as<ast::Assignment>()) {
        initializers.push_back(move_as<ast::Assignment>(stmt));
        for (auto const *expr : assignment->lhs()) {
          if (not expr->is<ast::Identifier>()) {
            diag.Consume(TodoDiagnostic{});
          }
        }
      } else {
        diag.Consume(TodoDiagnostic{});
        continue;
      }
    }

    return std::make_unique<ast::DesignatedInitializer>(
        range, move_as<ast::Expression>(nodes[0]), std::move(initializers));
  } else {
    return std::make_unique<ast::DesignatedInitializer>(
        range, move_as<ast::Expression>(nodes[0]),
        std::vector<std::unique_ptr<ast::Assignment>>{});
  }
}

std::unique_ptr<ast::Node> BuildNormalFunctionLiteral(
    absl::Span<std::unique_ptr<ast::Node>> nodes,
    diagnostic::DiagnosticConsumer &diag) {
  auto range =
      SourceRange(nodes[0]->range().begin(), nodes.back()->range().end());
  auto [params, outs] = std::move(nodes[0]->as<ast::FunctionType>()).extract();
  return BuildFunctionLiteral(range, ExtractInputs(std::move(params), diag),
                              std::move(outs),
                              std::move(nodes[1]->as<Statements>()), diag);
}

std::unique_ptr<ast::Node> BuildInferredFunctionLiteral(
    absl::Span<std::unique_ptr<ast::Node>> nodes,
    diagnostic::DiagnosticConsumer &diag) {
  auto range =
      SourceRange(nodes[0]->range().begin(), nodes.back()->range().end());
  return BuildFunctionLiteral(
      range, ExtractInputs(move_as<ast::Expression>(nodes[0]), diag), nullptr,
      std::move(nodes[2]->as<Statements>()), diag);
}

// TODO: this loses syntactic information that a formatter cares about.
std::unique_ptr<ast::Node> BuildShortFunctionLiteral(
    std::unique_ptr<ast::Expression> args,
    std::unique_ptr<ast::Expression> body,
    diagnostic::DiagnosticConsumer &diag) {
  auto range  = SourceRange(args->range().begin(), body->range().end());
  auto inputs = ExtractInputs(std::move(args), diag);

  std::vector<std::unique_ptr<ast::Expression>> ret_vals;
  if (auto *cl = body->if_as<CommaList>()) {
    ret_vals = std::move(*cl).extract();
  } else {
    ret_vals.push_back(std::move(body));
  }

  if (ret_vals.size() != 1u) {
    NOT_YET("Haven't handled multiple returns yet.");
  }
  return std::make_unique<ast::ShortFunctionLiteral>(range, std::move(inputs),
                                                     std::move(ret_vals[0]));
}

std::unique_ptr<ast::Node> BuildOneElementCommaList(
    absl::Span<std::unique_ptr<ast::Node>> nodes,
    diagnostic::DiagnosticConsumer &diag) {
  auto comma_list = std::make_unique<CommaList>(
      SourceRange(nodes[0]->range().begin(), nodes[3]->range().end()));
  comma_list->exprs_.push_back(move_as<ast::Expression>(nodes[1]));
  comma_list->parenthesized_ = true;
  return comma_list;
}

void ExtractRightChainImpl(Operator op, std::unique_ptr<ast::Expression> node,
                           std::vector<std::unique_ptr<ast::Expression>> &out) {
  if (auto *b = node->if_as<ast::BinaryOperator>();
      b and b->op() == op and not b->parenthesized_) {
    auto [lhs, rhs] = std::move(*b).extract();
    ExtractRightChainImpl(op, std::move(lhs), out);
    out.push_back(std::move(rhs));
  } else {
    out.push_back(std::move(node));
  }
}

std::vector<std::unique_ptr<ast::Expression>> ExtractRightChain(
    Operator op, std::unique_ptr<ast::Expression> node) {
  std::vector<std::unique_ptr<ast::Expression>> exprs;
  ExtractRightChainImpl(op, std::move(node), exprs);
  return exprs;
}

std::vector<std::unique_ptr<ast::Call>> BuildJumpOptions(
    std::unique_ptr<ast::Expression> node,
    diagnostic::DiagnosticConsumer &diag) {
  std::vector<std::unique_ptr<ast::Call>> call_exprs;
  auto exprs = ExtractRightChain(Operator::Or, std::move(node));
  for (auto &expr : exprs) {
    if (expr->is<ast::Call>()) {
      call_exprs.push_back(move_as<ast::Call>(expr));
    } else {
      diag.Consume(TodoDiagnostic{});
    }
  }

  return call_exprs;
}

std::unique_ptr<ast::Node> BuildStatementLeftUnop(
    absl::Span<std::unique_ptr<ast::Node>> nodes,
    diagnostic::DiagnosticConsumer &diag) {
  auto stmts = std::make_unique<Statements>(
      SourceRange(nodes.front()->range().begin(), nodes.back()->range().end()));
  const std::string &tk = nodes[0]->as<Token>().token;

  if (tk == "goto") {
    auto range = SourceRange(nodes.front()->range().begin(),
                             nodes.back()->range().end());
    auto exprs = ExtractIfCommaList(move_as<ast::Expression>(nodes[1]));
    switch (exprs.size()) {
      case 1: {
        auto jumps = BuildJumpOptions(move_as<ast::Expression>(exprs[0]), diag);
        stmts->append(
            std::make_unique<ast::UnconditionalGoto>(range, std::move(jumps)));
      } break;
      case 3: {
        auto true_jumps =
            BuildJumpOptions(move_as<ast::Expression>(exprs[1]), diag);
        auto false_jumps =
            BuildJumpOptions(move_as<ast::Expression>(exprs[2]), diag);
        stmts->append(std::make_unique<ast::ConditionalGoto>(
            range, move_as<ast::Expression>(exprs[0]), std::move(true_jumps),
            std::move(false_jumps)));
      } break;
      default: diag.Consume(TodoDiagnostic{}); return MakeInvalidNode(range);
    }
  } else if (tk == "return") {
    auto range = SourceRange(nodes.front()->range().begin(),
                             nodes.back()->range().end());
    std::vector<std::unique_ptr<ast::Expression>> exprs;
    if (auto *cl = nodes[1]->if_as<CommaList>();
        cl and not cl->parenthesized_) {
      exprs = std::move(*cl).extract();
    } else {
      exprs.push_back(move_as<ast::Expression>(nodes[1]));
    }
    stmts->append(std::make_unique<ast::ReturnStmt>(range, std::move(exprs)));
  }
  return stmts;
}

std::unique_ptr<ast::Node> BuildOneStatement(
    absl::Span<std::unique_ptr<ast::Node>> nodes,
    diagnostic::DiagnosticConsumer &diag) {
  auto stmts = std::make_unique<Statements>(nodes[0]->range());
  stmts->append(std::move(nodes[0]));
  ValidateStatementSyntax(stmts->content_.back().get(), diag);
  return stmts;
}

std::unique_ptr<ast::Node> BuildMoreStatements(
    absl::Span<std::unique_ptr<ast::Node>> nodes,
    diagnostic::DiagnosticConsumer &diag) {
  std::unique_ptr<Statements> stmts = move_as<Statements>(nodes[0]);
  stmts->append(std::move(nodes[1]));
  ValidateStatementSyntax(stmts->content_.back().get(), diag);
  return stmts;
}

std::unique_ptr<ast::Node> OneBracedJump(
    absl::Span<std::unique_ptr<ast::Node>> nodes,
    diagnostic::DiagnosticConsumer &diag) {
  auto stmts = std::make_unique<Statements>(
      SourceRange(nodes.front()->range().begin(), nodes.back()->range().end()));
  stmts->append(BuildControlHandler(std::move(nodes[1])));
  ValidateStatementSyntax(stmts->content_.back().get(), diag);
  return stmts;
}

std::unique_ptr<ast::Node> BuildControlHandler(
    absl::Span<std::unique_ptr<ast::Node>> nodes,
    diagnostic::DiagnosticConsumer &) {
  return BuildControlHandler(std::move(nodes[0]));
}

std::unique_ptr<ast::Node> BuildScopeNode(
    absl::Span<std::unique_ptr<ast::Node>> nodes,
    diagnostic::DiagnosticConsumer &diag) {
  SourceRange range(nodes.front()->range().begin(),
                    nodes.back()->range().end());
  auto [callee, ordered_fn_args] =
      std::move(nodes[0]->as<ast::Call>()).extract();
  std::vector<ast::BlockNode> blocks;
  blocks.push_back(std::move(nodes[1]->as<ast::BlockNode>()));
  return std::make_unique<ast::ScopeNode>(
      range, std::move(callee), std::move(ordered_fn_args), std::move(blocks));
}

std::unique_ptr<ast::Node> BuildBlockNode(
    absl::Span<std::unique_ptr<ast::Node>> nodes,
    diagnostic::DiagnosticConsumer &diag) {
  auto range =
      SourceRange(nodes.front()->range().begin(), nodes.back()->range().end());
  if (auto *id = nodes.front()->if_as<ast::Identifier>()) {
    return std::make_unique<ast::BlockNode>(
        range, std::string{id->name()},
        std::move(nodes.back()->as<Statements>()).extract());
  } else if (auto *index = nodes.front()->if_as<ast::Index>()) {
    auto [lhs, rhs] = std::move(*index).extract();
    std::vector<std::unique_ptr<ast::Declaration>> params;
    if (auto *cl = rhs->if_as<CommaList>()) {
      auto exprs = std::move(*cl).extract();
      for (auto &expr : exprs) {
        params.push_back(move_as<ast::Declaration>(expr));
      }

    } else {
      params.push_back(move_as<ast::Declaration>(rhs));
    }
    return std::make_unique<ast::BlockNode>(
        range, std::string{lhs->as<ast::Identifier>().name()},
        std::move(params), std::move(nodes.back()->as<Statements>()).extract());

  } else {
    diag.Consume(TodoDiagnostic{});
    return nullptr;
  }
}

std::unique_ptr<ast::Node> ExtendScopeNode(
    absl::Span<std::unique_ptr<ast::Node>> nodes,
    diagnostic::DiagnosticConsumer &diag) {
  nodes[0]->as<ast::ScopeNode>().append_block_syntactically(
      std::move(nodes[1]->as<ast::BlockNode>()));
  return std::move(nodes[0]);
}

std::unique_ptr<ast::Node> SugaredExtendScopeNode(
    absl::Span<std::unique_ptr<ast::Node>> nodes,
    diagnostic::DiagnosticConsumer &diag) {
  SourceRange range(nodes.front()->range().begin(),
                    nodes.back()->range().end());
  auto *updated_last_scope_node = &nodes[2]->as<ast::ScopeNode>();
  std::vector<std::unique_ptr<ast::Node>> block_stmt_nodes;
  block_stmt_nodes.push_back(std::move(nodes[2]));

  nodes[0]->as<ast::ScopeNode>().append_block_syntactically(
      ast::BlockNode(range,
                     std::string{nodes[1]->as<ast::Identifier>().name()},
                     std::move(block_stmt_nodes)),
      updated_last_scope_node);
  return std::move(nodes[0]);
}

std::unique_ptr<ast::Node> BuildBinaryOperator(
    absl::Span<std::unique_ptr<ast::Node>> nodes,
    diagnostic::DiagnosticConsumer &diag) {
  static base::Global kChainOps =
      absl::flat_hash_map<std::string_view, Operator>{
          {",", Operator::Comma}, {"==", Operator::Eq}, {"!=", Operator::Ne},
          {"<", Operator::Lt},    {">", Operator::Gt},  {"<=", Operator::Le},
          {">=", Operator::Ge}};

  std::string const &tk = nodes[1]->as<Token>().token;
  if (auto iter = kChainOps->find(tk); iter != kChainOps->end()) {
    nodes[1]->as<Token>().op = iter->second;
    return (iter->second == Operator::Comma)
               ? BuildCommaList(std::move(nodes), diag)
               : BuildChainOp(std::move(nodes), diag);
  }

  if (tk == ".") {
    return BuildAccess(std::move(nodes), diag);

  } else if (tk == ":" or tk == ":=") {
    return BuildDeclaration<false>(std::move(nodes), diag);

  } else if (tk == "::" or tk == "::=") {
    return BuildDeclaration<true>(std::move(nodes), diag);

  } else if (tk == "=>") {
    return BuildShortFunctionLiteral(move_as<ast::Expression>(nodes[0]),
                                     move_as<ast::Expression>(nodes[2]), diag);
  } else if (tk == "=") {
    SourceRange range(nodes[0]->range().begin(), nodes[2]->range().end());

    if (nodes[0]->is<ast::Declaration>()) {
      if (nodes[0]->as<ast::Declaration>().IsInferred()) {
        // NOTE: It might be that this was supposed to be a bool ==? How can we
        // give a good error message if that's what is intended?
        diag.Consume(TodoDiagnostic{});
        return move_as<ast::Declaration>(nodes[0]);
      }

      auto decl = move_as<ast::Declaration>(nodes[0]);
      decl->set_initial_value(move_as<ast::Expression>(nodes[2]));
      return decl;

    } else {
      auto lhs = ExtractIfCommaList(move_as<ast::Expression>(nodes[0]));
      auto rhs = ExtractIfCommaList(move_as<ast::Expression>(nodes[2]));
      return std::make_unique<ast::Assignment>(range, std::move(lhs),
                                               std::move(rhs));
    }
  } else if (tk == "as") {
    SourceRange range(nodes[0]->range().begin(), nodes[2]->range().end());
    return std::make_unique<ast::Cast>(range,
                                       move_as<ast::Expression>(nodes[0]),
                                       move_as<ast::Expression>(nodes[2]));
  } else if (tk == "'") {
    SourceRange range(nodes.front()->range().begin(),
                      nodes.back()->range().end());
    return BuildCallImpl(range, move_as<ast::Expression>(nodes[2]),
                         move_as<ast::Expression>(nodes[0]), diag);
  } else if (tk == "->") {
    SourceRange range(nodes.front()->range().begin(),
                      nodes.back()->range().end());
    auto params = ExtractIfCommaList(move_as<ast::Expression>(nodes[0]));
    auto outs   = ExtractIfCommaList(move_as<ast::Expression>(nodes[2]));
    return std::make_unique<ast::FunctionType>(range, std::move(params),
                                               std::move(outs));
  }

  static base::Global kSymbols =
      absl::flat_hash_map<std::string_view, Operator>{
          {"|=", Operator::OrEq},  {"&=", Operator::AndEq},
          {"^=", Operator::XorEq}, {"+=", Operator::AddEq},
          {"-=", Operator::SubEq}, {"*=", Operator::MulEq},
          {"/=", Operator::DivEq}, {"%=", Operator::ModEq},
          {"+", Operator::Add},    {"-", Operator::Sub},
          {"*", Operator::Mul},    {"/", Operator::Div},
          {"%", Operator::Mod},    {"^", Operator::Xor},
          {"&", Operator::And},    {"|", Operator::Or}};
  return std::make_unique<ast::BinaryOperator>(
      move_as<ast::Expression>(nodes[0]), kSymbols->find(tk)->second,
      move_as<ast::Expression>(nodes[2]));
}

std::unique_ptr<ast::Node> BuildEnumOrFlagLiteral(
    absl::Span<std::unique_ptr<ast::Node>> nodes, ast::EnumLiteral::Kind kind,
    diagnostic::DiagnosticConsumer &diag) {
  SourceRange range(nodes[0]->range().begin(), nodes[1]->range().end());
  std::vector<std::string> enumerators;
  absl::flat_hash_map<std::string, std::unique_ptr<ast::Expression>> values;
  if (auto *stmts = nodes[1]->if_as<Statements>()) {
    // TODO: if you want these values to depend on compile-time parameters,
    // you'll need to actually build the AST nodes.
    for (auto &stmt : stmts->content_) {
      if (auto *id = stmt->if_as<ast::Identifier>()) {
        enumerators.push_back(std::move(*id).extract());
      } else if (auto *decl = stmt->if_as<ast::Declaration>()) {
        if (not(decl->flags() & ast::Declaration::f_IsConst)) {
          diag.Consume(TodoDiagnostic{});
        }
        auto [id, type_expr, init_val] = std::move(*decl).extract();
        // TODO: Use the type expression?
        auto &name = enumerators.emplace_back(std::move(id));
        values.emplace(name, std::move(init_val));
      } else {
        LOG("", "%s", stmt->DebugString());
        diag.Consume(TodoDiagnostic{});
      }
    }
  }

  return std::make_unique<ast::EnumLiteral>(range, std::move(enumerators),
                                            std::move(values), kind);
}

std::unique_ptr<ast::Node> BuildScopeLiteral(
    std::unique_ptr<ast::Expression> state_type,
    std::unique_ptr<Statements> stmts, SourceRange const &range,
    diagnostic::DiagnosticConsumer &diag) {
  std::vector<ast::Declaration> decls;
  for (auto &stmt : stmts->content_) {
    if (auto *decl = stmt->if_as<ast::Declaration>()) {
      decls.push_back(std::move(*decl));
    } else {
      diag.Consume(TodoDiagnostic{});
    }
  }
  return std::make_unique<ast::ScopeLiteral>(range, std::move(state_type),
                                             std::move(decls));
}

std::unique_ptr<ast::Node> BuildBlock(std::unique_ptr<Statements> stmts,
                                      diagnostic::DiagnosticConsumer &diag) {
  auto range = stmts->range();  // TODO: it's really bigger than this because it
                                // involves the keyword too.

  std::vector<std::unique_ptr<ast::Declaration>> before, after;
  for (auto &stmt : stmts->content_) {
    if (auto *decl = stmt->if_as<ast::Declaration>()) {
      if (decl->id() == "before") {
        before.push_back(move_as<ast::Declaration>(stmt));
      } else if (decl->id() == "after") {
        after.push_back(move_as<ast::Declaration>(stmt));
      } else {
        diag.Consume(TodoDiagnostic{});
      }
    } else {
      diag.Consume(TodoDiagnostic{});
    }
  }
  return std::make_unique<ast::BlockLiteral>(range, std::move(before),
                                             std::move(after));
}

std::unique_ptr<ast::StructLiteral> BuildStructLiteral(
    Statements &&stmts, SourceRange range,
    diagnostic::DiagnosticConsumer &diag) {
  std::vector<std::unique_ptr<ast::Node>> node_stmts =
      std::move(stmts).extract();
  std::vector<ast::Declaration> fields;
  fields.reserve(node_stmts.size());
  for (auto &stmt : node_stmts) {
    if (auto *decl = stmt->if_as<ast::Declaration>()) {
      fields.push_back(std::move(*decl));
    } else {
      diag.Consume(NonDeclarationInStruct{.range = stmt->range()});
    }
  }

  return std::make_unique<ast::StructLiteral>(range, std::move(fields));
}

std::unique_ptr<ast::Node> BuildStatefulJump(
    absl::Span<std::unique_ptr<ast::Node>> nodes,
    diagnostic::DiagnosticConsumer &diag) {
  auto const &tk = nodes[0]->as<Token>().token;
  if (tk != "jump") {
    diag.Consume(TodoDiagnostic{});
    return nullptr;
  }

  SourceRange range(nodes.front()->range().begin(),
                    nodes.back()->range().end());
  std::vector<std::unique_ptr<ast::Declaration>> params;
  if (nodes.size() == 6) {
    if (nodes[2]->is<CommaList>()) {
      for (auto &expr : nodes[3]->as<CommaList>().exprs_) {
        ASSERT(expr, InheritsFrom<ast::Declaration>());  // TODO: handle failure
        auto decl = move_as<ast::Declaration>(expr);
        decl->flags() |= ast::Declaration::f_IsFnParam;
        params.push_back(std::move(decl));
      }
    } else {
      auto decl = move_as<ast::Declaration>(nodes[3]);
      decl->flags() |= ast::Declaration::f_IsFnParam;
      params.push_back(std::move(decl));
    }
  }

  auto &state_node = nodes[1];
  auto *array_expr = state_node->if_as<ast::ArrayLiteral>();
  if (not array_expr) {
    diag.Consume(TodoDiagnostic{});
    return nullptr;
  }
  if (array_expr->size() != 1) {
    diag.Consume(TodoDiagnostic{});
    return nullptr;
  }

  std::unique_ptr<ast::Expression> state_expr =
      std::move(std::move(*array_expr).extract()[0]);
  if (not state_expr->is<ast::Declaration>()) {
    diag.Consume(TodoDiagnostic{});
    return nullptr;
  }

  return std::make_unique<ast::Jump>(
      range, move_as<ast::Declaration>(state_expr), std::move(params),
      std::move(nodes.back()->as<Statements>()).extract());
}

std::unique_ptr<ast::Node> BuildParameterizedKeywordScope(
    absl::Span<std::unique_ptr<ast::Node>> nodes,
    diagnostic::DiagnosticConsumer &diag) {
  // TODO: should probably not do this with a token but some sort of enumerator
  // so we can ensure coverage/safety.
  ASSERT(nodes[0], InheritsFrom<Token>());
  auto const &tk = nodes[0]->as<Token>().token;
  if (tk == "jump") {
    SourceRange range(nodes.front()->range().begin(),
                      nodes.back()->range().end());
    std::vector<std::unique_ptr<ast::Declaration>> params;
    if (nodes.size() == 5) {
      if (nodes[2]->is<CommaList>()) {
        for (auto &expr : nodes[2]->as<CommaList>().exprs_) {
          ASSERT(expr,
                 InheritsFrom<ast::Declaration>());  // TODO: handle failure
          auto decl = move_as<ast::Declaration>(expr);
          decl->flags() |= ast::Declaration::f_IsFnParam;
          params.push_back(std::move(decl));
        }
      } else {
        auto decl = move_as<ast::Declaration>(nodes[2]);
        decl->flags() |= ast::Declaration::f_IsFnParam;
        params.push_back(std::move(decl));
      }
    }

    return std::make_unique<ast::Jump>(
        range, nullptr, std::move(params),
        std::move(nodes.back()->as<Statements>()).extract());

  } else if (tk == "scope") {
    SourceRange range(nodes.front()->range().begin(),
                      nodes.back()->range().end());
    return BuildScopeLiteral(move_as<ast::Expression>(nodes[2]),
                             move_as<Statements>(nodes.back()), range, diag);

  } else if (tk == "struct") {
    auto &stmts = nodes.back()->as<Statements>();
    std::vector<ast::Declaration> fields;
    for (auto &stmt : stmts.content_) {
      if (auto *decl = stmt->if_as<ast::Declaration>()) {
        fields.push_back(std::move(*decl));
      } else {
        diag.Consume(TodoDiagnostic{});
      }
    }
    auto inputs = ExtractInputs(move_as<ast::Expression>(nodes[2]), diag);
    std::vector<std::unique_ptr<ast::Declaration>> params;
    for (auto &expr : inputs) {
      if (expr->is<ast::Declaration>()) {
        params.push_back(move_as<ast::Declaration>(expr));
      } else {
        diag.Consume(TodoDiagnostic{});
      }
    }

    return std::make_unique<ast::ParameterizedStructLiteral>(
        SourceRange(nodes.front()->range().begin(),
                    nodes.back()->range().end()),
        std::move(params), std::move(fields));
  } else {
    UNREACHABLE();
  }
}

std::unique_ptr<ast::Node> BuildConcreteStruct(
    absl::Span<std::unique_ptr<ast::Node>> nodes,
    diagnostic::DiagnosticConsumer &diag) {
  return BuildStructLiteral(
      std::move(nodes[1]->as<Statements>()),
      SourceRange(nodes.front()->range().begin(), nodes.back()->range().end()),
      diag);
}

std::unique_ptr<ast::Node> BuildKWBlock(
    absl::Span<std::unique_ptr<ast::Node>> nodes,
    diagnostic::DiagnosticConsumer &diag) {
  if (nodes[0]->is<Token>()) {
    std::string const &tk = nodes[0]->as<Token>().token;

    if (bool is_enum = (tk == "enum"); is_enum or tk == "flags") {
      return BuildEnumOrFlagLiteral(std::move(nodes),
                                    is_enum ? ast::EnumLiteral::Kind::Enum
                                            : ast::EnumLiteral::Kind::Flags,
                                    diag);

    } else if (tk == "struct") {
      return BuildConcreteStruct(std::move(nodes), diag);

    } else if (tk == "scope") {
      SourceRange range(nodes.front()->range().begin(),
                        nodes.back()->range().end());
      return BuildScopeLiteral(nullptr, move_as<Statements>(nodes[1]), range,
                               diag);

    } else if (tk == "block") {
      return BuildBlock(move_as<Statements>(nodes[1]), diag);
    } else {
      UNREACHABLE(tk);
    }
  } else {
    UNREACHABLE(nodes[0].get());
  }
}

std::unique_ptr<ast::Node> Parenthesize(
    absl::Span<std::unique_ptr<ast::Node>> nodes,
    diagnostic::DiagnosticConsumer &diag) {
  auto result            = move_as<ast::Expression>(nodes[1]);
  result->parenthesized_ = true;
  return result;
}

std::unique_ptr<ast::Node> BuildEmptyParen(
    absl::Span<std::unique_ptr<ast::Node>> nodes,
    diagnostic::DiagnosticConsumer &diag) {
  if (nodes[0]->is<ast::Declaration>()) {
    diag.Consume(CallingDeclaration{.range = nodes[0]->range()});
  }
  SourceRange range(nodes[0]->range().begin(), nodes[2]->range().end());
  return std::make_unique<ast::Call>(range, move_as<ast::Expression>(nodes[0]),
                                     core::OrderedFnArgs<ast::Expression>{});
}

template <size_t N>
std::unique_ptr<ast::Node> drop_all_but(
    absl::Span<std::unique_ptr<ast::Node>> nodes,
    diagnostic::DiagnosticConsumer &diag) {
  return std::move(nodes[N]);
}

std::unique_ptr<ast::Node> CombineColonEq(
    absl::Span<std::unique_ptr<ast::Node>> nodes,
    diagnostic::DiagnosticConsumer &diag) {
  auto *tk_node = &nodes[0]->as<Token>();
  tk_node->token += "=";  // Change : to := and :: to ::=
  tk_node->op = Operator::ColonEq;
  return drop_all_but<0>(std::move(nodes), diag);
}

template <size_t ReturnIndex, size_t... ReservedIndices>
std::unique_ptr<ast::Node> ReservedKeywords(
    absl::Span<std::unique_ptr<ast::Node>> nodes,
    diagnostic::DiagnosticConsumer &diag) {
  (diag.Consume(
       ReservedKeyword{.range   = nodes[ReservedIndices]->range(),
                       .keyword = nodes[ReservedIndices]->as<Token>().token}),
   ...);
  return MakeInvalidNode(nodes[ReturnIndex]->range());
}

std::unique_ptr<ast::Node> BuildOperatorIdentifier(
    absl::Span<std::unique_ptr<ast::Node>> nodes,
    diagnostic::DiagnosticConsumer &diag) {
  auto range = nodes[1]->range();
  return std::make_unique<ast::Identifier>(range,
                                           move_as<Token>(nodes[1])->token);
}

std::unique_ptr<ast::Node> LabelScopeNode(
    absl::Span<std::unique_ptr<ast::Node>> nodes,
    diagnostic::DiagnosticConsumer &diag) {
  auto scope_node = move_as<ast::ScopeNode>(nodes[1]);
  if (scope_node->label()) {
    diag.Consume(
        ScopeNodeAlreadyHasLabel{.label_range = scope_node->label()->range(),
                                 .range       = scope_node->range()});
  } else {
    scope_node->range() =
        SourceRange(nodes[0]->range().begin(), scope_node->range().end());
    scope_node->set_label(std::move(nodes[0]->as<ast::Label>()));
  }
  return scope_node;
}

constexpr uint64_t OP_B = op_b | comma | colon | eq;
constexpr uint64_t EXPR = expr | fn_expr | scope_expr | fn_call_expr;
// Used in error productions only!
constexpr uint64_t RESERVED = kw_struct | kw_block_head | op_lt;
constexpr uint64_t KW_BLOCK = kw_struct | kw_block_head | kw_block;
// Here are the definitions for all rules in the langugae. For a rule to be
// applied, the node types on the top of the stack must match those given in the
// list (second line of each rule). If so, then the function given in the third
// line of each rule is applied, replacing the matched nodes. Lastly, the new
// nodes type is set to the given type in the first line.
static base::Global kRules = std::array{
    // Construction of braced statements
    ParseRule(braced_stmts, {l_brace, stmts, stmts | EXPR, r_brace},
              BracedStatementsSameLineEnd),
    ParseRule(braced_stmts, {l_brace, stmts, op_lt, r_brace},
              BracedStatementsJumpSameLineEnd),
    ParseRule(braced_stmts, {l_brace, stmts, r_brace}, drop_all_but<1>),
    ParseRule(braced_stmts, {l_brace, r_brace}, EmptyBraces),
    ParseRule(braced_stmts, {l_brace, EXPR, r_brace}, OneBracedStatement),
    ParseRule(braced_stmts, {l_brace, op_lt, r_brace}, OneBracedJump),

    // Construction of block expressions
    ParseRule(block_expr, {expr, braced_stmts}, BuildBlockNode),

    // Construction of scope expressions
    ParseRule(scope_expr, {fn_call_expr, block_expr}, BuildScopeNode),
    ParseRule(scope_expr, {scope_expr, block_expr}, ExtendScopeNode),
    ParseRule(scope_expr, {scope_expr, expr, scope_expr},
              SugaredExtendScopeNode),
    ParseRule(scope_expr, {label, scope_expr}, LabelScopeNode),

    // Construction of function call expressions
    ParseRule(fn_call_expr, {EXPR, l_paren, EXPR, r_paren}, BuildCall),
    ParseRule(fn_call_expr, {EXPR, l_paren, r_paren}, BuildEmptyParen),

    ParseRule(fn_expr, {EXPR, fn_arrow, EXPR}, BuildBinaryOperator),
    ParseRule(expr, {EXPR, (op_bl | OP_B), EXPR}, BuildBinaryOperator),
    ParseRule(op_b, {colon, eq}, CombineColonEq),
    ParseRule(fn_expr, {EXPR, fn_arrow, RESERVED}, ReservedKeywords<1, 2>),
    ParseRule(fn_expr, {RESERVED, fn_arrow, EXPR | kw_block},
              ReservedKeywords<1, 0>),
    ParseRule(fn_expr, {RESERVED, fn_arrow, RESERVED},
              ReservedKeywords<1, 0, 2>),
    ParseRule(expr, {EXPR, (OP_B | yield | op_bl), RESERVED},
              ReservedKeywords<1, 2>),
    ParseRule(expr, {RESERVED, (OP_B | yield | op_bl), RESERVED},
              ReservedKeywords<1, 0, 2>),
    ParseRule(expr, {l_paren, op_l | op_b | eq | op_bl, r_paren},
              BuildOperatorIdentifier),
    ParseRule(expr, {l_paren, r_paren}, BuildEmptyCommaList),
    ParseRule(expr, {EXPR, l_bracket, EXPR, r_bracket}, BuildIndexOperator),
    ParseRule(expr, {l_bracket, r_bracket}, BuildEmptyArray),
    ParseRule(expr, {l_bracket, EXPR, semicolon, EXPR, r_bracket},
              BuildArrayType),
    ParseRule(expr, {l_bracket, EXPR, semicolon, RESERVED, r_bracket},
              ReservedKeywords<0, 3>),
    ParseRule(expr, {l_bracket, RESERVED, semicolon, EXPR, r_bracket},
              ReservedKeywords<0, 1>),
    ParseRule(expr, {l_bracket, RESERVED, semicolon, RESERVED, r_bracket},
              ReservedKeywords<0, 1, 3>),
    // TODO: more specifically, the op_b needs to be a '.'
    ParseRule(expr, {expr | fn_call_expr, op_b, braced_stmts},
              BuildDesignatedInitializer),

    ParseRule(expr, {fn_expr, braced_stmts}, BuildNormalFunctionLiteral),
    ParseRule(expr, {expr, fn_arrow, braced_stmts},
              BuildInferredFunctionLiteral),
    ParseRule(expr, {hashtag, EXPR}, AddHashtag),

    // Call and index operator with reserved words. We can't put reserved words
    // in the first slot because that might conflict with a real use case.
    ParseRule(expr, {EXPR, l_paren, RESERVED, r_paren}, ReservedKeywords<0, 2>),
    ParseRule(expr, {EXPR, l_bracket, RESERVED, r_bracket},
              ReservedKeywords<0, 2>),

    ParseRule(expr, {EXPR, op_r}, BuildRightUnop),
    ParseRule(expr, {(op_l | op_bl | op_lt), EXPR}, BuildLeftUnop),
    ParseRule(stmts, {sop_lt | sop_l, EXPR}, BuildStatementLeftUnop),

    ParseRule(stmts, {label, yield, EXPR}, BuildLabeledYield),
    ParseRule(stmts, {yield, EXPR}, BuildUnlabeledYield),
    ParseRule(stmts, {label, yield}, BuildLabeledYield),

    ParseRule(expr, {RESERVED, (OP_B | yield | op_bl), EXPR},
              ReservedKeywords<1, 0>),
    ParseRule(expr, {l_paren | l_ref, EXPR, r_paren}, Parenthesize),
    ParseRule(expr, {l_bracket, EXPR, r_bracket}, BuildArrayLiteral),
    ParseRule(expr, {l_paren, RESERVED, r_paren}, ReservedKeywords<1, 1>),
    ParseRule(expr, {l_bracket, RESERVED, r_bracket}, ReservedKeywords<1, 1>),
    ParseRule(stmts, {stmts, (EXPR | stmts), newline | eof},
              BuildMoreStatements),
    ParseRule(expr, {kw_struct, l_paren, expr, r_paren, braced_stmts},
              BuildParameterizedKeywordScope),
    ParseRule(expr, {kw_struct, l_paren, r_paren, braced_stmts},
              BuildParameterizedKeywordScope),
    ParseRule(expr, {kw_struct, expr, l_paren, r_paren, braced_stmts},
              BuildStatefulJump),
    ParseRule(expr, {kw_struct, expr, l_paren, expr, r_paren, braced_stmts},
              BuildStatefulJump),
    ParseRule(expr, {KW_BLOCK, braced_stmts}, BuildKWBlock),

    ParseRule(expr, {(op_l | op_bl | op_lt), RESERVED}, ReservedKeywords<0, 1>),
    // TODO: does this rule prevent chained scope blocks on new lines or is it
    // preceeded by a shift rule that eats newlines after a right-brace?
    ParseRule(stmts, {EXPR, (newline | eof)}, BuildOneStatement),
    ParseRule(expr, {l_paren, EXPR, comma, r_paren}, BuildOneElementCommaList),

    // TODO: also need to handle labels with yields.
    ParseRule(stmts, {op_lt}, BuildControlHandler),
    ParseRule(stmts, {stmts, eof}, drop_all_but<0>),
};

enum class ShiftState { NeedMore, EndOfExpr, MustReduce };
struct ParseState {
  // TODO: storing the `diag` reference twice is unnecessary.
  explicit ParseState(std::vector<Lexeme> tokens,
                      diagnostic::DiagnosticConsumer &diag)
      : tokens_(std::move(tokens)), diag_(diag) {}

  template <size_t N>
  inline Tag get_type() const {
    return tag_stack_[tag_stack_.size() - N];
  }

  template <size_t N>
  inline ast::Node *get() const {
    return node_stack_[node_stack_.size() - N].get();
  }

  ShiftState shift_state() {
    // If the size is just 1, no rule will match so don't bother checking.
    if (node_stack_.size() < 2) { return ShiftState::NeedMore; }

    const auto &ahead = Next();
    if (ahead.tag_ == newline) {
      return brace_count == 0 ? ShiftState::EndOfExpr : ShiftState::MustReduce;
    }

    if (ahead.tag_ == expr and
        (get_type<1>() == fn_call_expr or get_type<1>() == scope_expr)) {
      return ShiftState::NeedMore;
    }

    if (ahead.tag_ == l_brace and (get_type<1>() & kw_block) and
        get_type<2>() == fn_arrow) {
      return ShiftState::MustReduce;
    }

    if (ahead.tag_ == l_brace and get_type<1>() == fn_expr and
        get_type<2>() == fn_arrow) {
      return ShiftState::MustReduce;
    }

    if (ahead.tag_ == l_brace and
        (get_type<1>() & (fn_expr | kw_block_head | kw_struct))) {
      return ShiftState::NeedMore;
    }

    if (get_type<1>() == newline and get_type<2>() == comma) {
      return ShiftState::MustReduce;
    }

    if (get_type<1>() & (op_lt | yield) and ahead.tag_ != newline) {
      return ShiftState::NeedMore;
    }

    if ((get_type<1>() & (kw_block_head | kw_struct)) and
        ahead.tag_ == newline) {
      return ShiftState::NeedMore;
    }

    if ((get_type<2>() & (kw_block_head | kw_struct)) and
        get_type<1>() == newline) {
      return ShiftState::NeedMore;
    }

    if (ahead.tag_ == r_paren) { return ShiftState::MustReduce; }

    if (get_type<1>() == r_paren and ahead.tag_ == l_brace) {
      size_t i = tag_stack_.size() - 1;
      while (i > 0) {
        if (tag_stack_[i] == fn_arrow) { return ShiftState::MustReduce; }
        if (tag_stack_[i] == stmts) { return ShiftState::NeedMore; }
        --i;
      }
      return ShiftState::NeedMore;
    }

    constexpr uint64_t OP = hashtag | op_r | op_l | op_b | colon | eq | comma |
                            op_bl | op_lt | fn_arrow | yield | sop_l | sop_lt;
    if (get_type<2>() & OP) {
      if (get_type<1>() == r_paren) {
        // TODO: this feels like a hack, but maybe this whole function is.
        return ShiftState::MustReduce;
      }
      auto left_prec = precedence(get<2>()->as<Token>().op);
      size_t right_prec;
      if (ahead.tag_ & OP) {
        right_prec = precedence(ahead.node_->as<Token>().op);
      } else if (ahead.tag_ == l_bracket) {
        right_prec = precedence(Operator::Index);

      } else if (ahead.tag_ == l_paren) {
        // TODO: this might be a hack. To get the following example to parse
        // correctly:
        //
        //    #tag
        //    (+) ::= ...
        //
        // As it stands we're assuming there's some expression being called
        // between the tag and the paren where the newline actually is. We can
        // get around this here by just explicitly checking that case, but
        // perhaps we should actually just lex "(+)" as it's own symbol with
        // it's own tag type. That might be more robust.
        if (get_type<1>() == newline) { return ShiftState::MustReduce; }
        right_prec = precedence(Operator::Call);
      } else {
        return ShiftState::MustReduce;
      }
      return (left_prec < right_prec) or
                     (left_prec == right_prec and
                      (left_prec & assoc_mask) == right_assoc)
                 ? ShiftState::NeedMore
                 : ShiftState::MustReduce;
    }
    return ShiftState::MustReduce;
  }

  void LookAhead() {
    if (token_index_ < tokens_.size()) {
      lookahead_ = std::move(tokens_[token_index_++]);
    } else {
      lookahead_ = std::nullopt;
    }
  }

  const TaggedNode &Next() {
    if (not lookahead_) { LookAhead(); }
    return *lookahead_;
  }

  std::vector<Tag> tag_stack_;
  std::vector<std::unique_ptr<ast::Node>> node_stack_;
  std::optional<TaggedNode> lookahead_;
  std::vector<Lexeme> tokens_;
  int token_index_ = 0;

  // We actually don't care about mathing braces because we are only using this
  // to determine for the REPL if we should prompt for further input. If it's
  // wrong, we won't be able to to parse anyway, so it only needs to be the
  // correct value when the braces match.
  int brace_count = 0;
  diagnostic::DiagnosticConsumer &diag_;
};

// Print out the debug information for the parse stack, and pause.
void Debug(ParseState *ps) {
  // Clear the screen
  fputs("\033[2J\033[1;1H\n", stderr);
  for (auto x : ps->tag_stack_) { absl::FPrintF(stderr, "%s, ", stringify(x)); }
  absl::FPrintF(stderr, " -> %s\n", stringify(ps->Next().tag_));

  for (const auto &node_ptr : ps->node_stack_) {
    fputs(node_ptr->DebugString().c_str(), stderr);
  }
  fgetc(stdin);
}

void Shift(ParseState *ps) {
  if (not ps->lookahead_) { ps->LookAhead(); }
  auto ahead = *std::exchange(ps->lookahead_, std::nullopt);
  ps->tag_stack_.push_back(ahead.tag_);
  ps->node_stack_.push_back(std::move(ahead.node_));

  LOG("parse", "shifting %s onto the stack.",
      ps->node_stack_.back()->DebugString());
  auto tag_ahead = ps->Next().tag_;
  if (tag_ahead & (l_paren | l_bracket | l_brace)) {
    ++ps->brace_count;
  } else if (tag_ahead & (r_paren | r_bracket | r_brace)) {
    --ps->brace_count;
  }
}

bool Reduce(ParseState *ps) {
  LOG("parse", "reducing");
  const ParseRule *matched_rule_ptr = nullptr;
  for (ParseRule const &rule : *kRules) {
    if (rule.Match(ps->tag_stack_)) {
      matched_rule_ptr = &rule;
      break;
    }
  }

  if (matched_rule_ptr == nullptr) {
    // If there are no good rules to match, look for some defaults. We could
    // encode these in `kRules` as well, but typically these do strange things
    // like preserving the tag type, so we'd have to encode it many times if it
    // were in `kRules`.
    if (ps->tag_stack_.size() >= 2 and ps->get_type<2>() == newline) {
      auto tag = ps->tag_stack_.back();
      ps->tag_stack_.pop_back();
      ps->tag_stack_.back() = tag;

      auto node = std::move(ps->node_stack_.back());
      ps->node_stack_.pop_back();
      ps->node_stack_.back() = std::move(node);
    } else if (ps->get_type<1>() == newline) {
      ps->tag_stack_.pop_back();
      ps->node_stack_.pop_back();
    } else {
      return false;
    }

    return true;
  }

  matched_rule_ptr->Apply(&ps->node_stack_, &ps->tag_stack_, ps->diag_);

  return true;
}

void CleanUpReduction(ParseState *state) {
  // Reduce what you can
  while (Reduce(state)) {
    if (debug::parser) { Debug(state); }
  }

  Shift(state);

  // Reduce what you can again
  while (Reduce(state)) {
    if (debug::parser) { Debug(state); }
  }
  if (debug::parser) { Debug(state); }
}
}  // namespace

std::vector<std::unique_ptr<ast::Node>> Parse(
    Source &src, diagnostic::DiagnosticConsumer &diag,
    LineNum initial_line_num) {
  auto nodes = Lex(src, diag, initial_line_num);
  // TODO: Shouldn't need this protection.
  if (nodes.size() == 1) { return {}; }
  ParseState state(std::move(nodes), diag);

  while (state.Next().tag_ != eof) {
    ASSERT(state.tag_stack_.size() == state.node_stack_.size());
    // Shift if you are supposed to, or if you are unable to reduce.
    if (state.shift_state() == ShiftState::NeedMore or not Reduce(&state)) {
      LOG("parse", "Need to shift");
      Shift(&state);
      LOG("parse", "shift_state == %d",
          static_cast<std::underlying_type_t<ShiftState>>(state.shift_state()));
    }

    if (debug::parser) { Debug(&state); }
  }

  // Cleanup
  CleanUpReduction(&state);

  // end()
  switch (state.node_stack_.size()) {
    case 0: UNREACHABLE();
    case 1:
      // TODO: log an error
      if (state.tag_stack_.back() & (eof | bof)) { return {}; }
      return std::move(move_as<Statements>(state.node_stack_.back())->content_);

    default: {
      std::vector<SourceRange> lines;

      for (size_t i = 0; i < state.node_stack_.size(); ++i) {
        if (state.tag_stack_[i] &
            (braced_stmts | l_paren | r_paren | l_bracket | r_bracket |
             l_brace | r_brace | semicolon | fn_arrow | expr)) {
          lines.push_back(state.node_stack_[i]->range());
        }
      }
      if (lines.empty()) {
        // We really have no idea what happened, just shove all the lines in.
        for (const auto &ns : state.node_stack_) {
          lines.push_back(ns->range());
        }
      }

      // This is an exceedingly crappy error message.
      diag.Consume(UnknownParseError{.lines = std::move(lines)});
      return {};
    }
  }
}

}  // namespace frontend

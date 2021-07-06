#include "frontend/parse.h"

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

ABSL_FLAG(bool, debug_parser, false,
          "Step through the parser step-by-step for debugging.");

namespace frontend {
namespace {

struct TodoDiagnostic {
  static constexpr std::string_view kCategory = "todo";
  static constexpr std::string_view kName     = "todo";

  diagnostic::DiagnosticMessage ToMessage(Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("TODO: Diagnostic emit from %s, line %u.",
                         loc.file_name(), loc.line()),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  std::experimental::source_location loc =
      std::experimental::source_location::current();
  SourceRange range;
};

struct NonIdentifierBinding {
  static constexpr std::string_view kCategory = "parse-error";
  static constexpr std::string_view kName     = "non-identifier-binding";

  diagnostic::DiagnosticMessage ToMessage(Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("A backtick (`) must be followed by an identifier"),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  SourceRange range;
};


struct DeclaringNonIdentifier {
  static constexpr std::string_view kCategory = "parse-error";
  static constexpr std::string_view kName     = "declaring-non-identifier";

  diagnostic::DiagnosticMessage ToMessage(Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Encountered a declaration where the expression being "
                         "declared is not an identifier."),
        diagnostic::SourceQuote(src).Highlighted(
            id_range, diagnostic::Style::ErrorText()));
  }

  SourceRange id_range;
};

struct AssigningNonIdentifier {
  static constexpr std::string_view kCategory = "parse-error";
  static constexpr std::string_view kName     = "assigning-non-identifier";

  diagnostic::DiagnosticMessage ToMessage(Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Encountered an assignment where the expression being "
                         "assigned to is not an identifier."),
        diagnostic::SourceQuote(src).Highlighted(
            id_range, diagnostic::Style::ErrorText()));
  }

  SourceRange id_range;
};

struct JumpingToNonCall {
  static constexpr std::string_view kCategory = "parse-error";
  static constexpr std::string_view kName     = "jumping-to-non-call";

  diagnostic::DiagnosticMessage ToMessage(Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Jump targets must be call expressions."),
        diagnostic::SourceQuote(src).Highlighted(
            range, diagnostic::Style::ErrorText()));
  }

  SourceRange range;
};

struct AccessRhsNotIdentifier {
  static constexpr std::string_view kCategory = "parse-error";
  static constexpr std::string_view kName     = "access-rhs-not-identifier";

  diagnostic::DiagnosticMessage ToMessage(Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Right-hand side must be an identifier"),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  SourceRange range;
};

// TODO: do we want to talk about this as already having a label, or giving it
// multiple labels? "multiple labels" seems clearer because it doesn't refer to
// parser state.
struct ScopeNodeAlreadyHasLabel {
  static constexpr std::string_view kCategory = "parse-error";
  static constexpr std::string_view kName     = "scope-already-has-label";

  diagnostic::DiagnosticMessage ToMessage(Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("This scope already has a label."),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  SourceRange label_range;
  SourceRange range;
};

struct ReservedKeyword {
  static constexpr std::string_view kCategory = "parse-error";
  static constexpr std::string_view kName     = "reserved-keyword";

  diagnostic::DiagnosticMessage ToMessage(Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Identifier `%s` is a reserved keyword.", keyword),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  SourceRange range;
  std::string keyword;
};

struct CallingDeclaration {
  static constexpr std::string_view kCategory = "parse-error";
  static constexpr std::string_view kName     = "calling-declaration";

  diagnostic::DiagnosticMessage ToMessage(Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Declarations cannot be called"),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  SourceRange range;
};

struct IndexingDeclaration {
  static constexpr std::string_view kCategory = "parse-error";
  static constexpr std::string_view kName     = "indexing-declaration";

  diagnostic::DiagnosticMessage ToMessage(Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Declarations cannot be indexed"),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  SourceRange range;
};

struct UnknownDeclarationInBlock {
  static constexpr std::string_view kCategory = "parse-error";
  static constexpr std::string_view kName     = "unknown-declaration-in-block";

  diagnostic::DiagnosticMessage ToMessage(Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Scope blocks may only declare 'before' and 'after'."),
        diagnostic::SourceQuote(src)
            .Highlighted(error_range, diagnostic::Style::ErrorText())
            .Highlighted(context_range, diagnostic::Style{}));
  }

  SourceRange error_range, context_range;
};

struct NonDeclarationInBlock {
  static constexpr std::string_view kCategory = "parse-error";
  static constexpr std::string_view kName     = "non-declaration-in-block";

  diagnostic::DiagnosticMessage ToMessage(Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Scope blocks may only contain declarations."),
        diagnostic::SourceQuote(src)
            .Highlighted(error_range, diagnostic::Style::ErrorText())
            .Highlighted(context_range, diagnostic::Style{}));
  }

  SourceRange error_range, context_range;
};

struct NonDeclarationInScope {
  static constexpr std::string_view kCategory = "parse-error";
  static constexpr std::string_view kName     = "non-declaration-in-scope";

  diagnostic::DiagnosticMessage ToMessage(Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Scopes may only contain declarations."),
        diagnostic::SourceQuote(src)
            .Highlighted(error_range, diagnostic::Style::ErrorText())
            .Highlighted(context_range, diagnostic::Style{}));
  }

  SourceRange error_range, context_range;
};

struct NonDeclarationInStruct {
  static constexpr std::string_view kCategory = "parse-error";
  static constexpr std::string_view kName     = "non-declaration-in-struct";

  diagnostic::DiagnosticMessage ToMessage(Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "Each struct member must be defined using a declaration."),
        diagnostic::SourceQuote(src)
            .Highlighted(error_range, diagnostic::Style::ErrorText())
            .Highlighted(context_range, diagnostic::Style{}));
  }

  SourceRange error_range, context_range;
};

struct NonDeclarationInInterface {
  static constexpr std::string_view kCategory = "parse-error";
  static constexpr std::string_view kName     = "non-declaration-in-interface";

  diagnostic::DiagnosticMessage ToMessage(Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "Each interface member must be defined using a declaration."),
        diagnostic::SourceQuote(src)
            .Highlighted(error_range, diagnostic::Style::ErrorText())
            .Highlighted(context_range, diagnostic::Style{}));
  }

  SourceRange error_range, context_range;
};

struct InvalidArgumentTypeVar {
  static constexpr std::string_view kCategory = "parse-error";
  static constexpr std::string_view kName     = "invalid-argument-type-var";

  diagnostic::DiagnosticMessage ToMessage(Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Argument type variables must be valid identifiers."),
        diagnostic::SourceQuote(src)
            .Highlighted(error_range, diagnostic::Style::ErrorText())
            .Highlighted(context_range, diagnostic::Style{}));
  }

  SourceRange error_range, context_range;
};

struct NonAssignmentInDesignatedInitializer {
  static constexpr std::string_view kCategory = "parse-error";
  static constexpr std::string_view kName =
      "non-assignment-in-designated-initializer";

  diagnostic::DiagnosticMessage ToMessage(Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "Each struct member must be initialized with an assignment."),
        diagnostic::SourceQuote(src)
            .Highlighted(error_range, diagnostic::Style::ErrorText())
            .Highlighted(context_range, diagnostic::Style{}));
  }

  SourceRange error_range, context_range;
};

struct UnknownParseError {
  static constexpr std::string_view kCategory = "parse-error";
  static constexpr std::string_view kName     = "unknown-parse-error";

  diagnostic::DiagnosticMessage ToMessage(Source const *src) const {
    diagnostic::SourceQuote quote(src);
    for (auto const &range : lines) {
      quote.Highlighted(range, diagnostic::Style{});
    }
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Parse errors found in \"%s\" on the following lines:",
                         src->FileName()),
        std::move(quote));
  }

  std::vector<SourceRange> lines;
};

struct ExceedinglyCrappyParseError {
  static constexpr std::string_view kCategory = "parse-error";
  static constexpr std::string_view kName = "exceedingly-crappy-parse-error";

  diagnostic::DiagnosticMessage ToMessage(Source const *src) const {
    diagnostic::SourceQuote quote(src);
    for (auto const &range : lines) {
      quote.Highlighted(range, diagnostic::Style{});
    }
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Parse errors found somewhere in \"%s\":",
                         src->FileName()),
        std::move(quote));
  }

  std::vector<SourceRange> lines;
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
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}),
        diagnostic::Text(
            "\nShort function literals do not use braces in Icarus.\n"
            "Rather than writing `(n: i64) => { n * n }`, remove the braces "
            "and write `(n: i64) => n * n`."));
  }

  SourceRange range;
};

struct InvalidGoto {
  static constexpr std::string_view kCategory = "parse-error";
  static constexpr std::string_view kName     = "invalid-goto";

  diagnostic::DiagnosticMessage ToMessage(Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Invalid number of arguments for `goto`."),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}),
        diagnostic::Text(
            "\nThe `goto` keyword can be used in one of two ways:\n"
            " 1) an unconditional jump, like `goto done()`\n"
            " 2) a conditional jump like `goto x > 3, higher(), lower()`."));
  }

  SourceRange range;
};

std::unique_ptr<ast::Identifier> MakeInvalidNode(
    SourceRange range = SourceRange()) {
  return std::make_unique<ast::Identifier>(range, "invalid_node");
}

struct Statements : ast::Node {
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

  void DebugStrAppend(std::string *out, size_t indent) const override {
    absl::StrAppend(out, "(",
                    absl::StrJoin(nodes_, ", ",
                                  [](std::string *out, auto const &n) {
                                    absl::StrAppend(out, n->DebugString());
                                  }),
                    ")");
  }

  std::vector<std::unique_ptr<ast::Node>> &&extract() && {
    return std::move(nodes_);
  }

  std::vector<std::unique_ptr<ast::Node>> nodes_;
};

std::vector<std::unique_ptr<ast::Node>> ExtractStatements(
    std::unique_ptr<ast::Node> node) {
  if (auto *s = node->if_as<Statements>()) { return std::move(*s).extract(); }
  std::vector<std::unique_ptr<ast::Node>> result;
  result.push_back(std::move(node));
  return result;
}

template <typename To, typename From>
std::unique_ptr<To> move_as(std::unique_ptr<From> &val) {
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

std::unique_ptr<ast::Node> BuildRightUnop(
    absl::Span<std::unique_ptr<ast::Node>> nodes,
    diagnostic::DiagnosticConsumer &diag) {
  const std::string &tk = nodes[1]->as<Token>().token;
  if (tk == ":?") {
    SourceRange range(nodes[0]->range().begin(), nodes[1]->range().end());
    auto unop = std::make_unique<ast::UnaryOperator>(
        range, ast::UnaryOperator::Kind::TypeOf,
        move_as<ast::Expression>(nodes[0]));

    if (unop->operand()->is<ast::Declaration>() and
        not unop->operand()->is<ast::BindingDeclaration>()) {
      diag.Consume(
          DeclarationUsedInUnaryOperator{.range = unop->operand()->range()});
    }

    return unop;
  } else {
    UNREACHABLE();
  }
}

void MergeIntoArgs(std::vector<ast::Call::Argument> &args,
                   std::unique_ptr<ast::Node> args_expr,
                   diagnostic::DiagnosticConsumer &diag) {
  if (auto *cl = args_expr->if_as<CommaList>()) {
    std::optional<SourceRange> last_named_range_before_error = std::nullopt;
    std::vector<SourceRange> positional_error_ranges;

    for (auto &expr : cl->nodes_) {
      if (auto *a = expr->if_as<ast::Assignment>()) {
        if (positional_error_ranges.empty()) {
          last_named_range_before_error = a->lhs()[0]->range();
        }
        // TODO: Error if there are multiple entries in this assignment.
        auto [lhs, rhs] = std::move(*a).extract();
        args.emplace_back(std::string(lhs[0]->as<ast::Identifier>().name()),
                          std::move(rhs[0]));
      } else {
        if (last_named_range_before_error.has_value()) {
          positional_error_ranges.push_back(expr->range());
        }
        args.emplace_back("", move_as<ast::Expression>(expr));
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
      args.emplace_back(std::string(lhs[0]->as<ast::Identifier>().name()),
                        std::move(rhs[0]));
    } else {
      args.emplace_back("", move_as<ast::Expression>(args_expr));
    }
  }
}

std::unique_ptr<ast::Node> BuildFullCall(
    absl::Span<std::unique_ptr<ast::Node>> nodes,
    diagnostic::DiagnosticConsumer &diag) {
  // Due to constraints of predecence, The token must be a `'`.
  ASSERT(nodes[1]->as<Token>().token == "'");
  SourceRange range(nodes.front()->range().begin(),
                    nodes.back()->range().end());

  std::vector<ast::Call::Argument> args;
  MergeIntoArgs(args, std::move(nodes[0]), diag);
  size_t split = args.size();
  MergeIntoArgs(args, std::move(nodes[3]), diag);

  auto callee = move_as<ast::Expression>(nodes[2]);

  if (callee->is<ast::Declaration>()) {
    diag.Consume(CallingDeclaration{.range = callee->range()});
  }

  return std::make_unique<ast::Call>(range, std::move(callee), std::move(args),
                                     split);
}

std::unique_ptr<ast::Node> BuildParenCall(
    absl::Span<std::unique_ptr<ast::Node>> nodes,
    diagnostic::DiagnosticConsumer &diag) {
  SourceRange range(nodes.front()->range().begin(),
                    nodes.back()->range().end());
  auto callee = move_as<ast::Expression>(nodes[0]);

  std::vector<ast::Call::Argument> args;
  MergeIntoArgs(args, std::move(nodes[1]), diag);

  if (callee->is<ast::Declaration>()) {
    diag.Consume(CallingDeclaration{.range = callee->range()});
  }

  return std::make_unique<ast::Call>(range, std::move(callee), std::move(args),
                                     0);
}

std::unique_ptr<ast::Node> BuildLeftUnop(
    absl::Span<std::unique_ptr<ast::Node>> nodes,
    diagnostic::DiagnosticConsumer &diag) {
  const std::string &tk = nodes[0]->as<Token>().token;
  SourceRange range(nodes[0]->range().begin(), nodes[1]->range().end());

  if (tk == "import") {
    return std::make_unique<ast::Import>(range,
                                         move_as<ast::Expression>(nodes[1]));

  } else if (tk == "`") {
    std::string id_str;
    if (auto *id = nodes[1]->if_as<ast::Identifier>()) {
      id_str = std::move(*id).extract();
    } else {
      diag.Consume(NonIdentifierBinding{.range = nodes[0]->range()});
    }

    return std::make_unique<ast::BindingDeclaration>(
        range, ast::Declaration::Id(std::move(id_str), nodes[0]->range()));
  } else if (tk == "~") {
    SourceRange range(nodes.front()->range().begin(),
                      nodes.back()->range().end());
    return std::make_unique<ast::PatternMatch>(
        range, move_as<ast::Expression>(nodes[1]));
  } else if (tk == "$") {
    std::string id_str;
    if (auto *id = nodes[1]->if_as<ast::Identifier>()) {
      id_str = std::move(*id).extract();
    } else {
      diag.Consume(InvalidArgumentTypeVar{
          .error_range   = nodes[1]->range(),
          .context_range = range,
      });
    }
    return std::make_unique<ast::ArgumentType>(range, std::move(id_str));
  }

  static base::Global kUnaryOperatorMap =
      absl::flat_hash_map<std::string_view, ast::UnaryOperator::Kind>{
          {"copy", ast::UnaryOperator::Kind::Copy},
          {"init", ast::UnaryOperator::Kind::Init},
          {"move", ast::UnaryOperator::Kind::Move},
          {"destroy", ast::UnaryOperator::Kind::Destroy},
          {"[*]", ast::UnaryOperator::Kind::BufferPointer},
          {":?", ast::UnaryOperator::Kind::TypeOf},
          {"&", ast::UnaryOperator::Kind::Address},
          {"@", ast::UnaryOperator::Kind::At},
          {"*", ast::UnaryOperator::Kind::Pointer},
          {"-", ast::UnaryOperator::Kind::Negate},
          {"not", ast::UnaryOperator::Kind::Not}};

  auto &operand = nodes[1];
  auto op       = kUnaryOperatorMap->find(tk)->second;

  if (operand->is<ast::Declaration>() and
      not operand->is<ast::BindingDeclaration>()) {
    diag.Consume(DeclarationUsedInUnaryOperator{.range = range});
    return std::make_unique<ast::UnaryOperator>(
        range, op, MakeInvalidNode(nodes[1]->range()));

  } else if (not operand->is<ast::Expression>()) {
    diag.Consume(TodoDiagnostic{.range = range});
    return std::make_unique<ast::UnaryOperator>(
        range, op, MakeInvalidNode(nodes[1]->range()));

  } else {
    return std::make_unique<ast::UnaryOperator>(
        range, op, move_as<ast::Expression>(nodes[1]));
  }
}

template <typename T>
static std::vector<std::unique_ptr<T>> ExtractIfCommaList(
    std::unique_ptr<ast::Node> node, bool even_if_parenthesized = false) {
  std::vector<std::unique_ptr<T>> nodes;
  if (auto *cl = node->if_as<CommaList>();
      cl and (even_if_parenthesized or cl->num_parentheses() == 0)) {
    auto extracted = std::move(*cl).extract();
    nodes.reserve(extracted.size());
    for (auto &n : extracted) { nodes.push_back(move_as<T>(n)); }
  } else {
    nodes.push_back(move_as<T>(node));
  }
  return nodes;
}

std::unique_ptr<ast::Node> BuildLabeledYield(
    absl::Span<std::unique_ptr<ast::Node>> nodes,
    diagnostic::DiagnosticConsumer &diag) {
  auto range =
      SourceRange(nodes.front()->range().begin(), nodes.back()->range().end());

  std::vector<std::unique_ptr<ast::Expression>> exprs;
  if (nodes.size() > 2) {
    ASSERT(nodes.size() == 3);
    exprs = ExtractIfCommaList<ast::Expression>(std::move(nodes[2]));
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
  std::vector<std::unique_ptr<ast::Expression>> exprs =
      ExtractIfCommaList<ast::Expression>(std::move(nodes[1]));

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
      nodes[0]->as<CommaList>().num_parentheses() == 0) {
    comma_list = move_as<CommaList>(nodes[0]);
  } else {
    comma_list = std::make_unique<CommaList>(
        SourceRange(nodes[0]->range().begin(), nodes[2]->range().end()));
    comma_list->nodes_.push_back(std::move(nodes[0]));
  }
  comma_list->nodes_.push_back(std::move(nodes[2]));
  comma_list->range().end() = comma_list->nodes_.back()->range().end();
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

std::unique_ptr<ast::Node> BuildSlice(
    absl::Span<std::unique_ptr<ast::Node>> nodes,
    diagnostic::DiagnosticConsumer &diag) {
  auto range =
      SourceRange(nodes.front()->range().begin(), nodes.back()->range().end());
  auto slice = std::make_unique<ast::SliceType>(
      range, move_as<ast::Expression>(nodes[1]));

  return slice;
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
  return std::make_unique<ast::ArrayLiteral>(
      SourceRange(nodes.front()->range().begin(), nodes.back()->range().end()),
      ExtractIfCommaList<ast::Expression>(std::move(nodes[1])));
}

std::unique_ptr<ast::Node> BuildArrayType(
    absl::Span<std::unique_ptr<ast::Node>> nodes,
    diagnostic::DiagnosticConsumer &diag) {
  if (auto *cl = nodes[1]->if_as<CommaList>();
      cl and cl->num_parentheses() == 0) {
    SourceRange range(nodes.front()->range().begin(),
                      nodes.back()->range().end());
    return std::make_unique<ast::ArrayType>(
        range, ExtractIfCommaList<ast::Expression>(std::move(nodes[1])),
        move_as<ast::Expression>(nodes[3]));
  } else {
    return std::make_unique<ast::ArrayType>(
        SourceRange(nodes[0]->range().begin(), nodes[4]->range().end()),
        move_as<ast::Expression>(nodes[1]), move_as<ast::Expression>(nodes[3]));
  }
}

std::unique_ptr<ast::Node> BuildDeclaration(
    absl::Span<std::unique_ptr<ast::Node>> nodes,
    diagnostic::DiagnosticConsumer &diag) {
  std::string_view token = nodes[1]->as<Token>().token;
  bool is_const          = (token == "::" or token == "::=");
  auto op                = nodes[1]->as<Token>().op;
  SourceRange decl_range(nodes.front()->range().begin(),
                         nodes.back()->range().end());
  std::vector<ast::Declaration::Id> ids;
  bool error = false;
  if (auto *id = nodes[0]->if_as<ast::Identifier>()) {
    frontend::SourceRange range = nodes[0]->range();
    ids.emplace_back(std::move(*id).extract(), range);
  } else if (auto *cl = nodes[0]->if_as<CommaList>()) {
    ASSERT(cl->num_parentheses() != 0u);
    for (auto &&i : std::move(*cl).extract()) {
      if (auto *id = i->if_as<ast::Identifier>()) {
        frontend::SourceRange range = id->range();
        ids.emplace_back(std::move(*id).extract(), range);
      } else {
        diag.Consume(DeclaringNonIdentifier{.id_range = i->range()});
        error = true;
      }
    }

  } else {
    diag.Consume(DeclaringNonIdentifier{.id_range = nodes[0]->range()});
    error = true;
  }
  if (error) { return MakeInvalidNode(decl_range); }

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
      decl_range, std::move(ids), std::move(type_expr), std::move(init_val),
      (initial_value_is_hole ? ast::Declaration::f_InitIsHole : 0) |
          (is_const ? ast::Declaration::f_IsConst : 0));
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
    return BuildFunctionLiteral(
        range, std::move(inputs),
        ExtractIfCommaList<ast::Expression>(std::move(output), true),
        std::move(stmts), diag);
  }
}

// Represents a sequence of the form: `<expr> . <braced-statements>`
// The only valid expressions of this form are designated initializers, where
// `expr` is the type being initialized.
std::unique_ptr<ast::Node> BuildDesignatedInitializer(
    absl::Span<std::unique_ptr<ast::Node>> nodes,
    diagnostic::DiagnosticConsumer &diag) {
  SourceRange range(nodes[0]->range().begin(), nodes.back()->range().end());

  auto extracted_stmts = ExtractStatements(std::move(nodes[2]));

  std::vector<std::unique_ptr<ast::Assignment>> initializers;
  initializers.reserve(extracted_stmts.size());
  for (auto &stmt : extracted_stmts) {
    if (auto const *assignment = stmt->if_as<ast::Assignment>()) {
      initializers.push_back(move_as<ast::Assignment>(stmt));
      for (auto const *expr : assignment->lhs()) {
        if (not expr->is<ast::Identifier>()) {
          diag.Consume(AssigningNonIdentifier{.id_range = expr->range()});
        }
      }
    } else {
      diag.Consume(NonAssignmentInDesignatedInitializer{
          .error_range   = stmt->range(),
          .context_range = range,
      });
      continue;
    }
  }

  return std::make_unique<ast::DesignatedInitializer>(
      range, move_as<ast::Expression>(nodes[0]), std::move(initializers));
}

// Represents a sequence of the form: `(decls) => <braced-statements>`
// This isn't a valid short function literal, but it's a common mistake so we
// have a special parse rule to call it out with a useful error message.
std::unique_ptr<ast::Node> HandleBracedShortFunctionLiteral(
    absl::Span<std::unique_ptr<ast::Node>> nodes,
    diagnostic::DiagnosticConsumer &diag) {
  SourceRange range(nodes[0]->range().begin(), nodes.back()->range().end());
  diag.Consume(BracedShortFunctionLiteral{.range = range});
  return MakeInvalidNode(range);
}

std::unique_ptr<ast::Node> BuildNormalFunctionLiteral(
    absl::Span<std::unique_ptr<ast::Node>> nodes,
    diagnostic::DiagnosticConsumer &diag) {
  SourceRange range(nodes[0]->range().begin(), nodes.back()->range().end());
  auto [params, outs] = std::move(nodes[0]->as<ast::FunctionType>()).extract();
  Statements stmts(nodes[1]->range());
  if (auto *s = nodes[1]->if_as<Statements>()) {
    stmts = std::move(*s);
  } else {
    stmts.append(std::move(nodes[1]));
  }
  std::vector<std::unique_ptr<ast::Declaration>> decls;
  decls.reserve(params.size());
  for (auto &p : params) { decls.push_back(move_as<ast::Declaration>(p)); }

  return BuildFunctionLiteral(range, std::move(decls), std::move(outs),
                              std::move(stmts), diag);
}

std::unique_ptr<ast::Node> BuildInferredFunctionLiteral(
    absl::Span<std::unique_ptr<ast::Node>> nodes,
    diagnostic::DiagnosticConsumer &diag) {
  auto range =
      SourceRange(nodes[0]->range().begin(), nodes.back()->range().end());

  Statements stmts(nodes[2]->range());
  if (auto *s = nodes[2]->if_as<Statements>()) {
    stmts = std::move(*s);
  } else {
    stmts.append(std::move(nodes[1]));
  }
  return BuildFunctionLiteral(
      range, ExtractIfCommaList<ast::Declaration>(std::move(nodes[0]), true),
      nullptr, std::move(stmts), diag);
}

std::unique_ptr<ast::Node> BuildShortFunctionLiteral(
    absl::Span<std::unique_ptr<ast::Node>> nodes,
    diagnostic::DiagnosticConsumer &diag) {
  auto args   = move_as<ast::Expression>(nodes[0]);
  auto body   = move_as<ast::Expression>(nodes[2]);
  auto range  = SourceRange(args->range().begin(), body->range().end());
  auto inputs = ExtractIfCommaList<ast::Declaration>(std::move(args), true);

  std::vector<std::unique_ptr<ast::Expression>> ret_vals =
      ExtractIfCommaList<ast::Expression>(std::move(body));

  if (ret_vals.size() != 1u) {
    NOT_YET("Haven't handled multiple returns yet.");
  }
  return std::make_unique<ast::ShortFunctionLiteral>(range, std::move(inputs),
                                                     std::move(ret_vals[0]));
}

void ExtractRightChainImpl(Operator op, std::unique_ptr<ast::Expression> node,
                           std::vector<std::unique_ptr<ast::Expression>> &out) {
  if (auto *b = node->if_as<ast::BinaryOperator>();
      b and b->op() == op and b->num_parentheses() == 0) {
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
  auto exprs = ExtractRightChain(Operator::SymbolOr, std::move(node));
  for (auto &expr : exprs) {
    if (expr->is<ast::Call>()) {
      call_exprs.push_back(move_as<ast::Call>(expr));
    } else {
      diag.Consume(JumpingToNonCall{.range = expr->range()});
    }
  }

  return call_exprs;
}

std::unique_ptr<ast::Node> BuildStatementLeftUnop(
    absl::Span<std::unique_ptr<ast::Node>> nodes,
    diagnostic::DiagnosticConsumer &diag) {
  SourceRange range(nodes.front()->range().begin(),
                    nodes.back()->range().end());
  auto stmts = std::make_unique<Statements>(range);

  const std::string &tk = nodes[0]->as<Token>().token;
  if (tk == "goto") {
    auto exprs = ExtractIfCommaList<ast::Expression>(std::move(nodes[1]));
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
      default:
        diag.Consume(InvalidGoto{.range = range});
        return MakeInvalidNode(range);
    }
  } else if (tk == "return") {
    std::vector<std::unique_ptr<ast::Expression>> exprs =
        ExtractIfCommaList<ast::Expression>(std::move(nodes[1]));
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

std::unique_ptr<ast::Node> BuildMoreBracedStatements(
    absl::Span<std::unique_ptr<ast::Node>> nodes,
    diagnostic::DiagnosticConsumer &diag) {
  std::unique_ptr<Statements> stmts = move_as<Statements>(nodes[1]);
  stmts->append(std::move(nodes[2]));
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
  auto [callee, args] = std::move(nodes[0]->as<ast::Call>()).extract();

  std::vector<ast::BlockNode> blocks;
  blocks.push_back(std::move(nodes[1]->as<ast::BlockNode>()));
  return std::make_unique<ast::ScopeNode>(range, std::move(callee),
                                          std::move(args), std::move(blocks));
}

std::unique_ptr<ast::Node> BuildBlockNode(
    absl::Span<std::unique_ptr<ast::Node>> nodes,
    diagnostic::DiagnosticConsumer &diag) {
  SourceRange range(nodes.front()->range().begin(),
                    nodes.back()->range().end());

  auto stmts = ExtractStatements(std::move(nodes.back()));
  if (auto *id = nodes.front()->if_as<ast::Identifier>()) {
    if (nodes.size() == 5) {
      std::vector<std::unique_ptr<ast::Declaration>> params =
          ExtractIfCommaList<ast::Declaration>(std::move(nodes[2]), true);

      return std::make_unique<ast::BlockNode>(
          range, std::string{id->name()}, id->range().end(), std::move(params),
          std::move(stmts));
    } else {
      return std::make_unique<ast::BlockNode>(
          range, std::string{id->name()}, id->range().end(), std::move(stmts));
    }
  } else {
    diag.Consume(TodoDiagnostic{.range = range});
    return std::make_unique<ast::BlockNode>(
        range, "", nodes.front()->range().end(), std::move(stmts));
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
      ast::BlockNode(range, std::string{nodes[1]->as<ast::Identifier>().name()},
                     nodes[1]->range().end(), std::move(block_stmt_nodes)),
      updated_last_scope_node);
  return std::move(nodes[0]);
}

std::unique_ptr<ast::Node> BuildDeclarationInitialization(
    absl::Span<std::unique_ptr<ast::Node>> nodes,
    diagnostic::DiagnosticConsumer &diag) {
  SourceRange range(nodes[0]->range().begin(), nodes[2]->range().end());

  auto decl = move_as<ast::Declaration>(nodes[0]);
  if (not decl->type_expr()) {
    // NOTE: It might be that this was supposed to be a bool ==? How can we
    // give a good error message if that's what is intended?
    diag.Consume(TodoDiagnostic{.range = range});
    return move_as<ast::Declaration>(nodes[0]);
  }

  decl->set_initial_value(move_as<ast::Expression>(nodes[2]));
  return decl;
}

std::unique_ptr<ast::Node> BuildAssignment(
    absl::Span<std::unique_ptr<ast::Node>> nodes,
    diagnostic::DiagnosticConsumer &diag) {
  SourceRange range(nodes[0]->range().begin(), nodes[2]->range().end());
  auto lhs = ExtractIfCommaList<ast::Expression>(std::move(nodes[0]), true);
  auto rhs = ExtractIfCommaList<ast::Expression>(std::move(nodes[2]), true);
  return std::make_unique<ast::Assignment>(range, std::move(lhs),
                                           std::move(rhs));
}

std::unique_ptr<ast::Node> BuildTickCall(
    absl::Span<std::unique_ptr<ast::Node>> nodes,
    diagnostic::DiagnosticConsumer &diag) {
  SourceRange range(nodes.front()->range().begin(),
                    nodes.back()->range().end());
  std::unique_ptr<ast::Expression> callee =
      move_as<ast::Expression>(nodes.back());

  if (callee->is<ast::Declaration>()) {
    diag.Consume(CallingDeclaration{.range = callee->range()});
  }

  std::vector<ast::Call::Argument> args;

  if (nodes.size() != 2) { MergeIntoArgs(args, std::move(nodes[0]), diag); }
  size_t num_args = args.size();

  return std::make_unique<ast::Call>(range, std::move(callee), std::move(args),
                                     num_args);
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

  if (tk == "~") {
    return std::make_unique<ast::PatternMatch>(
        move_as<ast::Expression>(nodes[0]), move_as<ast::Expression>(nodes[2]));
  }

  if (tk == "as") {
    SourceRange range(nodes[0]->range().begin(), nodes[2]->range().end());
    return std::make_unique<ast::Cast>(range,
                                       move_as<ast::Expression>(nodes[0]),
                                       move_as<ast::Expression>(nodes[2]));
  }

  static base::Global kSymbols =
      absl::flat_hash_map<std::string_view, Operator>{
          {"|=", Operator::SymbolOrEq},  {"&=", Operator::SymbolAndEq},
          {"^=", Operator::SymbolXorEq}, {"+=", Operator::AddEq},
          {"-=", Operator::SubEq},       {"*=", Operator::MulEq},
          {"/=", Operator::DivEq},       {"%=", Operator::ModEq},
          {"+", Operator::Add},          {"-", Operator::Sub},
          {"*", Operator::Mul},          {"/", Operator::Div},
          {"%", Operator::Mod},          {"xor", Operator::Xor},
          {"and", Operator::And},        {"or", Operator::Or},
          {"^", Operator::SymbolXor},    {"&", Operator::SymbolAnd},
          {"|", Operator::SymbolOr}};
  return std::make_unique<ast::BinaryOperator>(
      move_as<ast::Expression>(nodes[0]), kSymbols->find(tk)->second,
      move_as<ast::Expression>(nodes[2]));
}

std::unique_ptr<ast::Node> BuildFunctionExpression(
    absl::Span<std::unique_ptr<ast::Node>> nodes,
    diagnostic::DiagnosticConsumer &diag) {
  SourceRange range(nodes.front()->range().begin(),
                    nodes.back()->range().end());
  auto params = ExtractIfCommaList<ast::Expression>(std::move(nodes[0]), true);
  auto outs   = ExtractIfCommaList<ast::Expression>(std::move(nodes[2]), true);
  return std::make_unique<ast::FunctionType>(range, std::move(params),
                                             std::move(outs));
}

std::unique_ptr<ast::Node> BuildEnumOrFlagLiteral(
    absl::Span<std::unique_ptr<ast::Node>> nodes, ast::EnumLiteral::Kind kind,
    diagnostic::DiagnosticConsumer &diag) {
  SourceRange range(nodes[0]->range().begin(), nodes[1]->range().end());
  std::vector<std::string> enumerators;
  absl::flat_hash_map<std::string, std::unique_ptr<ast::Expression>> values;
  auto stmts = ExtractStatements(std::move(nodes[1]));

  // TODO: if you want these values to depend on compile-time parameters,
  // you'll need to actually build the AST nodes.
  for (auto &stmt : stmts) {
    if (auto *id = stmt->if_as<ast::Identifier>()) {
      enumerators.push_back(std::move(*id).extract());
    } else if (auto *decl = stmt->if_as<ast::Declaration>()) {
      if (not(decl->flags() & ast::Declaration::f_IsConst)) {
        diag.Consume(TodoDiagnostic{.range = range});
      }
      auto [ids, type_expr, init_val] = std::move(*decl).extract();
      // TODO: Use the type expression?
      for (auto &id : ids) {
        auto [name, range] = std::move(id).extract();
        enumerators.push_back(std::move(name));
        // TODO: Support multiple declarations
        values.emplace(enumerators.back(), std::move(init_val));
      }
    } else {
      LOG("", "%s", stmt->DebugString());
      diag.Consume(TodoDiagnostic{.range = range});
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
      diag.Consume(NonDeclarationInScope{
          .error_range   = stmt->range(),
          .context_range = range,
      });
    }
  }
  return std::make_unique<ast::ScopeLiteral>(range, std::move(state_type),
                                             std::move(decls));
}

std::unique_ptr<ast::Node> BuildBlock(std::unique_ptr<Statements> stmts,
                                      SourceRange const &range,
                                      diagnostic::DiagnosticConsumer &diag) {
  std::vector<std::unique_ptr<ast::Declaration>> before, after;
  for (auto &stmt : stmts->content_) {
    if (auto *decl = stmt->if_as<ast::Declaration>()) {
      // TODO: Support multiple declarations for "before" and "after".
      if (decl->ids()[0].name() == "before") {
        before.push_back(move_as<ast::Declaration>(stmt));
      } else if (decl->ids()[0].name() == "after") {
        after.push_back(move_as<ast::Declaration>(stmt));
      } else {
        diag.Consume(UnknownDeclarationInBlock{
            .error_range   = decl->ids()[0].range(),
            .context_range = range,
        });
      }
    } else {
      diag.Consume(NonDeclarationInBlock{
          .error_range   = stmt->range(),
          .context_range = range,
      });
    }
  }
  return std::make_unique<ast::BlockLiteral>(range, std::move(before),
                                             std::move(after));
}

std::unique_ptr<ast::StructLiteral> BuildStructLiteral(
    std::vector<std::unique_ptr<ast::Node>> &&stmts, SourceRange range,
    diagnostic::DiagnosticConsumer &diag) {
  std::vector<ast::Declaration> fields;
  fields.reserve(stmts.size());
  for (auto &stmt : stmts) {
    if (auto *decl = stmt->if_as<ast::Declaration>()) {
      fields.push_back(std::move(*decl));
    } else {
      diag.Consume(NonDeclarationInStruct{
          .error_range   = stmt->range(),
          .context_range = range,
      });
    }
  }

  return std::make_unique<ast::StructLiteral>(range, std::move(fields));
}

std::unique_ptr<ast::InterfaceLiteral> BuildInterfaceLiteral(
    Statements &&stmts, SourceRange range,
    diagnostic::DiagnosticConsumer &diag) {
  std::vector<std::unique_ptr<ast::Node>> node_stmts =
      std::move(stmts).extract();

  std::vector<std::pair<std::unique_ptr<ast::Expression>,
                        std::unique_ptr<ast::Expression>>>
      exprs;
  for (auto &stmt : node_stmts) {
    if (auto *decl = stmt->if_as<ast::Declaration>()) {
      auto [names, type_expr, init_expr] = std::move(*decl).extract();
      ASSERT(names.size() == 1u);
      ASSERT(init_expr == nullptr);
      exprs.emplace_back(std::make_unique<ast::Declaration::Id>(names[0]),
                         std::move(type_expr));
    } else {
      diag.Consume(NonDeclarationInInterface{
          .error_range   = stmt->range(),
          .context_range = range,
      });
    }
  }

  return std::make_unique<ast::InterfaceLiteral>(range, std::move(exprs));
}

std::unique_ptr<ast::Node> BuildStatefulJump(
    absl::Span<std::unique_ptr<ast::Node>> nodes,
    diagnostic::DiagnosticConsumer &diag) {
  auto const &tk = nodes[0]->as<Token>().token;
  if (tk != "jump") {
    diag.Consume(TodoDiagnostic{.range = nodes[0]->range()});
    return nullptr;
  }

  SourceRange range(nodes.front()->range().begin(),
                    nodes.back()->range().end());
  std::vector<std::unique_ptr<ast::Declaration>> params;
  if (nodes.size() == 6) {
    if (nodes[4]->is<CommaList>()) {
      for (auto &expr : nodes[4]->as<CommaList>().nodes_) {
        auto decl = move_as<ast::Declaration>(expr);
        decl->flags() |= ast::Declaration::f_IsFnParam;
        params.push_back(std::move(decl));
      }
    } else {
      auto decl = move_as<ast::Declaration>(nodes[4]);
      decl->flags() |= ast::Declaration::f_IsFnParam;
      params.push_back(std::move(decl));
    }
  }

  std::vector<std::unique_ptr<ast::Expression>> state_exprs =
      ExtractIfCommaList<ast::Expression>(std::move(nodes[2]));

  if (not state_exprs[0]->is<ast::Declaration>()) {
    diag.Consume(TodoDiagnostic{.range = range});
    return nullptr;
  }

  return std::make_unique<ast::Jump>(
      range, move_as<ast::Declaration>(state_exprs[0]), std::move(params),
      std::move(nodes.back()->as<Statements>()).extract());
}

std::unique_ptr<ast::Node> BuildParameterizedKeywordScope(
    absl::Span<std::unique_ptr<ast::Node>> nodes,
    diagnostic::DiagnosticConsumer &diag) {
  // TODO: should probably not do this with a token but some sort of
  // enumerator so we can ensure coverage/safety.
  auto const &tk = nodes[0]->as<Token>().token;
  SourceRange range(nodes.front()->range().begin(),
                    nodes.back()->range().end());
  if (tk == "jump") {
    auto stmts = ExtractStatements(std::move(nodes.back()));

    std::vector<std::unique_ptr<ast::Declaration>> params =
        ExtractIfCommaList<ast::Declaration>(std::move(nodes[1]), true);
    for (auto &p : params) { p->flags() |= ast::Declaration::f_IsFnParam; }

    return std::make_unique<ast::Jump>(range, nullptr, std::move(params),
                                       std::move(stmts));

  } else if (tk == "scope") {
    return BuildScopeLiteral(move_as<ast::Expression>(nodes[1]),
                             move_as<Statements>(nodes.back()), range, diag);

  } else if (tk == "struct") {
    auto stmts = ExtractStatements(std::move(nodes.back()));

    std::vector<ast::Declaration> fields;
    for (auto &stmt : stmts) {
      if (auto *decl = stmt->if_as<ast::Declaration>()) {
        fields.push_back(std::move(*decl));
      } else {
        diag.Consume(NonDeclarationInStruct{
            .error_range   = stmt->range(),
            .context_range = range,
        });
      }
    }
    auto inputs =
        ExtractIfCommaList<ast::Declaration>(std::move(nodes[1]), true);
    std::vector<std::unique_ptr<ast::Declaration>> params;
    for (auto &expr : inputs) {
      if (expr->is<ast::Declaration>()) {
        params.push_back(move_as<ast::Declaration>(expr));
      } else {
        diag.Consume(TodoDiagnostic{.range = expr->range()});
      }
    }

    return std::make_unique<ast::ParameterizedStructLiteral>(
        range, std::move(params), std::move(fields));
  } else {
    UNREACHABLE();
  }
}

std::unique_ptr<ast::Node> BuildKWBlock(
    absl::Span<std::unique_ptr<ast::Node>> nodes,
    diagnostic::DiagnosticConsumer &diag) {
  if (nodes[0]->is<Token>()) {
    std::string const &tk = nodes[0]->as<Token>().token;
    SourceRange range(nodes.front()->range().begin(),
                      nodes.back()->range().end());

    if (bool is_enum = (tk == "enum"); is_enum or tk == "flags") {
      return BuildEnumOrFlagLiteral(std::move(nodes),
                                    is_enum ? ast::EnumLiteral::Kind::Enum
                                            : ast::EnumLiteral::Kind::Flags,
                                    diag);

    } else if (tk == "struct") {
      std::vector<std::unique_ptr<ast::Node>> stmts;
      if (nodes[1]->is<Statements>()) {
        stmts = std::move(nodes[1]->as<Statements>()).extract();
      } else {
        stmts.push_back(std::move(nodes[1]));
      }
      return BuildStructLiteral(std::move(stmts), range, diag);

    } else if (tk == "interface") {
      return BuildInterfaceLiteral(std::move(nodes[1]->as<Statements>()), range,
                                   diag);

    } else if (tk == "scope") {
      return BuildScopeLiteral(nullptr, move_as<Statements>(nodes[1]), range,
                               diag);

    } else if (tk == "block") {
      return BuildBlock(move_as<Statements>(nodes[1]), range, diag);
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
  auto result = move_as<ast::Expression>(nodes[1]);
  result->wrap_parentheses(frontend::SourceRange(nodes.front()->range().begin(),
                                                 nodes.back()->range().end()));
  return result;
}

template <size_t N>
std::unique_ptr<ast::Node> KeepOnly(
    absl::Span<std::unique_ptr<ast::Node>> nodes,
    diagnostic::DiagnosticConsumer &) {
  return std::move(nodes[N]);
}

std::unique_ptr<ast::Node> CombineColonEq(
    absl::Span<std::unique_ptr<ast::Node>> nodes,
    diagnostic::DiagnosticConsumer &diag) {
  auto *tk_node = &nodes[0]->as<Token>();
  tk_node->token += "=";  // Change : to := and :: to ::=
  tk_node->op = Operator::ColonEq;
  return KeepOnly<0>(std::move(nodes), diag);
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

constexpr uint64_t OP_B = op_b | tick | dot | colon | eq | colon_eq | rocket;
constexpr uint64_t FN_CALL_EXPR = paren_call_expr | full_call_expr;
constexpr uint64_t EXPR         = expr | fn_expr | scope_expr | FN_CALL_EXPR |
                          paren_expr | bracket_expr | empty_brackets;
constexpr uint64_t STMTS = stmt | stmt_list;
// Used in error productions only!
constexpr uint64_t RESERVED = kw_struct | kw_block_head | op_lt;
constexpr uint64_t KW_BLOCK = kw_struct | kw_block_head | kw_block;
// Here are the definitions for all rules in the langugae. For a rule to be
// applied, the node types on the top of the stack must match those given in
// the list (second line of each rule). If so, then the function given in the
// third line of each rule is applied, replacing the matched nodes. Lastly,
// the new nodes type is set to the given type in the first line.

using rule_t = Rule<6>;

static base::Global kRules = std::array{
    // Array types
    rule_t{.match   = {l_bracket, EXPR | expr_list, semicolon, EXPR, r_bracket},
           .output  = expr,
           .execute = BuildArrayType},
    rule_t{
        .match  = {l_bracket, EXPR | expr_list, semicolon, RESERVED, r_bracket},
        .output = expr,
        .execute = ReservedKeywords<0, 3>},
    rule_t{.match   = {l_bracket, RESERVED, semicolon, EXPR, r_bracket},
           .output  = expr,
           .execute = ReservedKeywords<0, 1>},
    rule_t{.match   = {l_bracket, RESERVED, semicolon, RESERVED, r_bracket},
           .output  = expr,
           .execute = ReservedKeywords<0, 1, 3>},

    // Declarations and assignments
    rule_t{.match   = {EXPR, colon, EXPR},
           .output  = decl,
           .execute = BuildDeclaration},
    rule_t{.match   = {EXPR, colon_eq, EXPR},
           .output  = decl,
           .execute = BuildDeclaration},
    rule_t{.match = {colon, eq}, .output = colon_eq, .execute = CombineColonEq},
    rule_t{.match  = {decl, eq, EXPR},
           .output = decl,
           // TODO: We could split the declaration part off of this function.
           .execute = BuildDeclarationInitialization},
    rule_t{.match  = {EXPR, eq, EXPR},
           .output = assignment,
           // TODO: We could split the declaration part off of this function.
           .execute = BuildAssignment},

    // Bracket-indexing
    rule_t{.match   = {EXPR, l_bracket, EXPR, r_bracket},
           .output  = expr,
           .execute = BuildIndexOperator},
    rule_t{.match   = {EXPR, l_bracket, RESERVED, r_bracket},
           .output  = expr,
           .execute = ReservedKeywords<0, 2>},

    // Slices
    rule_t{
        .match = {empty_brackets, EXPR}, .output = expr, .execute = BuildSlice},
    rule_t{.match   = {empty_brackets, RESERVED},
           .output  = expr,
           .execute = ReservedKeywords<0, 1>},

    // Array literals
    rule_t{.match   = {l_bracket, r_bracket},
           .output  = empty_brackets,
           .execute = BuildEmptyArray},
    rule_t{.match   = {l_bracket, EXPR | expr_list, r_bracket},
           .output  = bracket_expr,
           .execute = BuildArrayLiteral},
    rule_t{.match   = {l_bracket, RESERVED, r_bracket},
           .output  = bracket_expr,
           .execute = ReservedKeywords<1, 1>},

    // Statements
    rule_t{.match   = {label, yield, EXPR},
           .output  = stmt,
           .execute = BuildLabeledYield},
    rule_t{
        .match = {label, yield}, .output = stmt, .execute = BuildLabeledYield},
    rule_t{
        .match = {yield, EXPR}, .output = stmt, .execute = BuildUnlabeledYield},

    // Function expressions `a -> b`
    rule_t{.match   = {EXPR | paren_decl_list | empty_parens, fn_arrow,
                     EXPR | empty_parens},
           .output  = fn_expr,
           .execute = BuildFunctionExpression},
    rule_t{.match   = {EXPR | paren_decl_list, fn_arrow, RESERVED},
           .output  = fn_expr,
           .execute = ReservedKeywords<0, 2>},
    rule_t{.match   = {RESERVED, fn_arrow, EXPR},
           .output  = fn_expr,
           .execute = ReservedKeywords<0, 0>},
    rule_t{.match   = {RESERVED, fn_arrow, RESERVED},
           .output  = fn_expr,
           .execute = ReservedKeywords<0, 0, 2>},

    rule_t{.match   = {fn_expr, braced_stmts},
           .output  = expr,
           .execute = BuildNormalFunctionLiteral},
    rule_t{.match   = {paren_decl_list | empty_parens, fn_arrow, braced_stmts},
           .output  = expr,
           .execute = BuildInferredFunctionLiteral},

    // Structs, etc.
    rule_t{.match   = {kw_struct, empty_parens | paren_expr | paren_decl_list,
                     braced_stmts},
           .output  = expr,
           .execute = BuildParameterizedKeywordScope},
    rule_t{.match   = {kw_struct, l_bracket, decl | decl_list, r_bracket,
                     empty_parens | paren_expr | paren_decl_list, braced_stmts},
           .output  = expr,
           .execute = BuildStatefulJump},
    rule_t{.match   = {KW_BLOCK, braced_stmts},
           .output  = expr,
           .execute = BuildKWBlock},

    // Function call expressions
    rule_t{.match   = {EXPR, tick, EXPR, paren_expr | empty_parens},
           .output  = full_call_expr,
           .execute = BuildFullCall},
    rule_t{.match   = {RESERVED, tick, EXPR, paren_expr | empty_parens},
           .output  = full_call_expr,
           .execute = ReservedKeywords<1, 0>},
    rule_t{.match   = {EXPR, tick, RESERVED, paren_expr | empty_parens},
           .output  = full_call_expr,
           .execute = ReservedKeywords<1, 2>},
    rule_t{.match   = {RESERVED, tick, RESERVED, paren_expr | empty_parens},
           .output  = full_call_expr,
           .execute = ReservedKeywords<1, 0, 2>},
    rule_t{.match = {EXPR, paren_expr | empty_parens | paren_decl_list},
           // TODO: Remove the paren_decl_list and have that be it's own
           // interface building node.
           .output  = paren_call_expr,
           .execute = BuildParenCall},
    rule_t{.match   = {RESERVED, paren_expr | empty_parens},
           .output  = paren_call_expr,
           .execute = ReservedKeywords<0, 0>},

    // Braced statements
    rule_t{.match   = {l_brace, r_brace},
           .output  = braced_stmts,
           .execute = EmptyBraces},
    rule_t{.match   = {l_brace, STMTS | EXPR | assignment | decl, r_brace},
           .output  = braced_stmts,
           .execute = KeepOnly<1>},
    rule_t{.match   = {l_brace, STMTS, EXPR | assignment | decl, r_brace},
           .output  = braced_stmts,
           .execute = BuildMoreBracedStatements},
    rule_t{.match   = {l_brace, RESERVED, r_brace},
           .output  = braced_stmts,
           .execute = ReservedKeywords<1, 1>},

    // Block nodes
    rule_t{.match   = {expr, braced_stmts},
           .output  = block_expr,
           .execute = BuildBlockNode},
    rule_t{
        .match   = {expr, l_bracket, decl | decl_list, r_bracket, braced_stmts},
        .output  = block_expr,
        .execute = BuildBlockNode},

    // Scope nodes
    rule_t{.match   = {FN_CALL_EXPR, block_expr},
           .output  = scope_expr,
           .execute = BuildScopeNode},
    rule_t{.match   = {scope_expr, block_expr},
           .output  = scope_expr,
           .execute = ExtendScopeNode},
    rule_t{.match   = {scope_expr, expr, scope_expr},
           .output  = scope_expr,
           .execute = SugaredExtendScopeNode},
    rule_t{.match   = {label, scope_expr},
           .output  = scope_expr,
           .execute = LabelScopeNode},

    // Operators
    rule_t{.match   = {paren_decl_list | empty_parens | EXPR, rocket, EXPR},
           .output  = expr,
           .execute = BuildShortFunctionLiteral},
    rule_t{.match = {EXPR, dot, EXPR}, .output = expr, .execute = BuildAccess},
    rule_t{
        .match = {EXPR, tick, EXPR}, .output = expr, .execute = BuildTickCall},
    rule_t{.match   = {EXPR, op_bl | OP_B, EXPR},
           .output  = expr,
           .execute = BuildBinaryOperator},
    rule_t{.match   = {EXPR, op_bl | OP_B, RESERVED},
           .output  = expr,
           .execute = ReservedKeywords<1, 2>},
    rule_t{.match   = {RESERVED, op_bl | OP_B, EXPR},
           .output  = expr,
           .execute = ReservedKeywords<1, 0>},
    rule_t{.match   = {RESERVED, op_bl | OP_B, RESERVED},
           .output  = expr,
           .execute = ReservedKeywords<1, 0, 2>},
    rule_t{.match   = {paren_decl_list | empty_parens, rocket, RESERVED},
           .output  = expr,
           .execute = ReservedKeywords<1, 2>},
    rule_t{.match = {tick, EXPR}, .output = expr, .execute = BuildTickCall},
    rule_t{.match   = {op_l | op_bl | op_lt, EXPR},
           .output  = expr,
           .execute = BuildLeftUnop},
    rule_t{.match   = {tick | op_l | op_bl | op_lt, RESERVED},
           .output  = expr,
           .execute = ReservedKeywords<0, 1>},
    rule_t{.match = {EXPR, op_r}, .output = expr, .execute = BuildRightUnop},
    rule_t{.match   = {RESERVED, op_r},
           .output  = expr,
           .execute = ReservedKeywords<1, 0>},
    rule_t{.match   = {sop_lt | sop_l, EXPR | expr_list},
           .output  = stmt,
           .execute = BuildStatementLeftUnop},
    rule_t{.match   = {sop_lt | sop_l, RESERVED},
           .output  = stmt,
           .execute = ReservedKeywords<0, 1>},
    rule_t{.match   = {RESERVED, (OP_B | yield | op_bl), EXPR},
           .output  = expr,
           .execute = ReservedKeywords<1, 0>},

    // Parentheses
    rule_t{.match   = {l_paren, r_paren},
           .output  = empty_parens,
           .execute = BuildEmptyCommaList},
    rule_t{.match   = {l_paren, assignment, r_paren},
           .output  = paren_expr,
           .execute = KeepOnly<1>},
    rule_t{.match   = {l_paren, EXPR | expr_list | assignment_list, r_paren},
           .output  = paren_expr,
           .execute = Parenthesize},
    rule_t{.match   = {l_paren, decl | decl_list, r_paren},
           .output  = paren_decl_list,
           .execute = Parenthesize},
    rule_t{.match   = {l_paren, RESERVED, r_paren},
           .output  = paren_expr,
           .execute = ReservedKeywords<1, 1>},

    // Statements
    rule_t{.match   = {STMTS, stmt},
           .output  = stmt_list,
           .execute = BuildMoreStatements},
    rule_t{.match   = {decl | EXPR, newline | eof},
           .output  = stmt,
           .execute = BuildOneStatement},

    // Miscellaneous
    rule_t{.match   = {EXPR | assignment | expr_list, comma, EXPR | assignment},
           .output  = expr_list,
           .execute = BuildCommaList},
    rule_t{.match   = {EXPR | expr_list, comma, decl},
           .output  = decl_list,
           .execute = BuildCommaList},
    rule_t{.match   = {decl | decl_list, comma, decl | EXPR},
           .output  = decl_list,
           .execute = BuildCommaList},
    rule_t{.match   = {l_paren, op_l | op_b | eq | op_bl, r_paren},
           .output  = expr,
           .execute = BuildOperatorIdentifier},
    rule_t{.match   = {EXPR, dot, braced_stmts},
           .output  = expr,
           .execute = BuildDesignatedInitializer},
    rule_t{.match   = {paren_decl_list | empty_parens, rocket, braced_stmts},
           .output  = expr,
           .execute = HandleBracedShortFunctionLiteral},
    rule_t{.match = {hashtag, EXPR}, .output = expr, .execute = AddHashtag},
    rule_t{.match = {hashtag, decl}, .output = decl, .execute = AddHashtag},
    rule_t{.match = {STMTS, eof}, .output = stmt_list, .execute = KeepOnly<0>},
};

static base::Global kMoreRules = std::array{
    // Backups.
    //
    // Barring any other successful reductions, these rules should be applied.
    // They need to be last, because they change the tag type, making it one
    // step more general, but only match exactly one node.
    //
    // TODO: does this rule prevent chained scope blocks on new lines or is it
    // preceeded by a shift rule that eats newlines after a right-brace?
    rule_t{.match = {op_lt}, .output = stmt, .execute = BuildControlHandler},
    rule_t{.match   = {EXPR | decl | assignment},
           .output  = stmt,
           .execute = BuildOneStatement},
    rule_t{
        .match = {RESERVED}, .output = expr, .execute = ReservedKeywords<0, 0>},

};

enum class ShiftState { NeedMore, MustReduce, ReduceHarder };
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
    // TODO: Rather than repeatedly checking for empty, which can only happen
    // on the first iteration, just unroll that iteration.
    if (node_stack_.empty()) { return ShiftState::NeedMore; }

    const auto &ahead = Next();

    switch (get_type<1>()) {
      case empty_brackets:
        return ahead.tag_ == newline ? ShiftState::MustReduce
                                     : ShiftState::NeedMore;
      case stmt:
      case newline: return ShiftState::MustReduce;
      default: break;
    }

    switch (ahead.tag_) {
      case colon:
        return get_type<1>() == r_paren ? ShiftState::MustReduce
                                        : ShiftState::NeedMore;
      case l_paren: {
        if (get_type<1>() & (kw_struct | kw_block_head)) {
          return ShiftState::NeedMore;
        } else if (node_stack_.size() >= 2 and (get_type<1>() & EXPR) and
                   get_type<2>() & (sop_l | sop_lt)) {
          return ShiftState::NeedMore;
        }
      } break;
      case l_brace:
        if (get_type<1>() & fn_expr) { return ShiftState::NeedMore; }
        if ((get_type<1>() & paren_expr) and (get_type<2>() & EXPR)) {
          return ShiftState::MustReduce;
        }

        if (get_type<1>() &
            (kw_struct | empty_parens | paren_expr | kw_block_head)) {
          if (get_type<2>() == fn_arrow) {
            return ShiftState::MustReduce;
          } else {
            return ShiftState::NeedMore;
          }
        } else {
          return ShiftState::MustReduce;
        }
      case r_brace: return ShiftState::MustReduce;
      case newline: return ShiftState::ReduceHarder;
      default: break;
    }

    if (node_stack_.size() == 1) { return ShiftState::NeedMore; }

    if (get_type<1>() & (op_lt | yield) and ahead.tag_ != newline) {
      return ShiftState::NeedMore;
    }

    constexpr uint64_t OP = hashtag | op_r | op_l | op_b | dot | colon | eq |
                            tick | colon_eq | dot | comma | op_bl | op_lt |
                            fn_arrow | yield | sop_l | sop_lt | rocket;
    if (get_type<2>() & OP) {
      auto left_prec = precedence(get<2>()->as<Token>().op);

      if (left_prec == precedence(Operator::Call) and ahead.tag_ == l_paren) {
        return ShiftState::NeedMore;
      }

      size_t right_prec;
      if (ahead.tag_ & OP) {
        right_prec = precedence(ahead.node_->as<Token>().op);
      } else if (ahead.tag_ == l_bracket) {
        right_prec = precedence(Operator::Index);

      } else if (ahead.tag_ == l_paren) {
        // TODO: this might be a hack. To get the following example to
        // parse correctly:
        //
        //    #tag
        //    (+) ::= ...
        //
        // As it stands we're assuming there's some expression being called
        // between the tag and the paren where the newline actually is. We
        // can get around this here by just explicitly checking that case,
        // but perhaps we should actually just lex "(+)" as it's own symbol
        // with it's own tag type. That might be more robust.
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
    } else {
      return ShiftState::MustReduce;
    }
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

  // We actually don't care about mathing braces because we are only using
  // this to determine for the REPL if we should prompt for further input. If
  // it's wrong, we won't be able to to parse anyway, so it only needs to be
  // the correct value when the braces match.
  int brace_count = 0;
  diagnostic::DiagnosticConsumer &diag_;
};

// Print out the debug information for the parse stack, and pause.
void Debug(ParseState *ps) {
  // Clear the screen
  fputs("\033[2J\033[1;1H\n", stderr);
  for (auto x : ps->tag_stack_) {
    std::stringstream ss;
    ss << x;
    absl::FPrintF(stderr, "%s, ", ss.str());
  }
  std::stringstream ss;
  ss << ps->Next().tag_;
  absl::FPrintF(stderr, " -> %s\n", ss.str());

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

template <auto &RuleSet>
bool Reduce(ParseState *ps) {
  LOG("parse", "reducing");
  for (auto const &rule : *RuleSet) {
    if (rule.match(ps->tag_stack_)) {
      auto nodes_to_reduce = absl::MakeSpan(
          std::addressof(*(ps->node_stack_.end() - rule.match.size())),
          rule.match.size());
      auto result     = rule.execute(nodes_to_reduce, ps->diag_);
      size_t new_size = ps->node_stack_.size() - rule.match.size() + 1;
      ps->tag_stack_.resize(new_size);
      ps->node_stack_.resize(new_size);
      ps->node_stack_.back() = std::move(result);
      ps->tag_stack_.back()  = rule.output;
      return true;
    }
  }

  // If there are no good rules to match, look for some defaults. We could
  // encode these in `*RuleSet` as well, but typically these do strange things
  // like preserving the tag type, so we'd have to encode it many times if it
  // were in `*RuleSet`.
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

void CleanUpReduction(ParseState *state) {
  // Reduce what you can
  while (Reduce<kRules>(state)) {
    if (absl::GetFlag(FLAGS_debug_parser)) { Debug(state); }
  }

  Shift(state);

  // Reduce what you can again
  while (Reduce<kRules>(state)) {
    if (absl::GetFlag(FLAGS_debug_parser)) { Debug(state); }
  }
  if (absl::GetFlag(FLAGS_debug_parser)) { Debug(state); }
}
}  // namespace

std::vector<std::unique_ptr<ast::Node>> Parse(
    SourceBuffer &buffer, diagnostic::DiagnosticConsumer &diag, size_t chunk) {
  auto nodes = Lex(buffer, diag, chunk);
  // If lexing failed, don't bother trying to parse.
  if (diag.num_consumed() > 0) { return {}; }
  // TODO: Shouldn't need this protection.
  if (nodes.size() == 1) { return {}; }
  ParseState state(std::move(nodes), diag);

  while (state.Next().tag_ != eof) {
    ASSERT(state.tag_stack_.size() == state.node_stack_.size());
    // Shift if you are supposed to, or if you are unable to reduce.
    switch (state.shift_state()) {
      case ShiftState::ReduceHarder:
        if (Reduce<kRules>(&state)) break;
        if (Reduce<kMoreRules>(&state)) break;
        Shift(&state);
        break;
      case ShiftState::MustReduce:
        if (Reduce<kRules>(&state)) break;
        [[fallthrough]];
      case ShiftState::NeedMore: Shift(&state); break;
    }

    if (absl::GetFlag(FLAGS_debug_parser)) { Debug(&state); }
  }

  // Cleanup
  CleanUpReduction(&state);

  // end()
  switch (state.node_stack_.size()) {
    case 0: UNREACHABLE();
    case 1:
      // TODO: log an error
      if (diag.num_consumed() > 0) { return {}; }
      if (state.tag_stack_.back() & eof) { return {}; }
      return std::move(move_as<Statements>(state.node_stack_.back())->content_);

    default: {
      std::vector<SourceRange> lines;
      for (size_t i = 0; i < state.node_stack_.size(); ++i) {
        if (not(state.tag_stack_[i] & stmt_list)) {
          lines.push_back(state.node_stack_[i]->range());
        }
      }
      if (lines.empty()) {
        // We really have no idea what happened, just shove all the lines in.
        for (const auto &ns : state.node_stack_) {
          lines.push_back(ns->range());
        }
        diag.Consume(ExceedinglyCrappyParseError{.lines = std::move(lines)});
      } else {
        diag.Consume(UnknownParseError{.lines = std::move(lines)});
      }
      return {};
    }
  }
}

}  // namespace frontend

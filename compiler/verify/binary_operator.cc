#include "compiler/compiler.h"
#include "type/overload_set.h"
#include "type/pointer.h"
#include "type/primitive.h"
#include "type/qual_type.h"

namespace compiler {
namespace {

struct UnexpandedBinaryOperatorArgument {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName = "unexpanded-binary-operator-argument";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Binary operator argument expands to %u values. Each "
                         "operand must expand to exactly 1 value.",
                         num_arguments),
        diagnostic::SourceQuote(src).Highlighted(
            range, diagnostic::Style::ErrorText()));
  }

  size_t num_arguments;
  frontend::SourceRange range;
};

struct InvalidBinaryOperatorOverload {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName = "invalid-binary-operator-overload";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("No valid operator overload for (%s)", op),
        diagnostic::SourceQuote(src).Highlighted(
            range, diagnostic::Style::ErrorText()));
  }

  std::string op;
  frontend::SourceRange range;
};

struct LogicalAssignmentNeedsBoolOrFlags {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName =
      "logical-assignment-needs-bool-or-flags";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Operator '%s' must take boolean or flags arguments.",
                         OperatorToString(kind)),
        diagnostic::SourceQuote(src).Highlighted(
            range, diagnostic::Style::ErrorText()));
  }

  ast::BinaryOperator::Kind kind;
  frontend::SourceRange range;

 private:
  static std::string_view OperatorToString(ast::BinaryOperator::Kind kind) {
    switch (kind) {
      case ast::BinaryOperator::Kind::SymbolXorEq: return "^=";
      case ast::BinaryOperator::Kind::SymbolAndEq: return "&=";
      case ast::BinaryOperator::Kind::SymbolOrEq: return "|=";
      default: UNREACHABLE();
    }
  }
};

// TODO: +=, etc should really be treated as assignments.
struct InvalidAssignmentOperatorLhsValueCategory {
  static constexpr std::string_view kCategory = "value-category-error";
  static constexpr std::string_view kName =
      "invalid-assignment-lhs-value-category";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Lefthand-side of binary logical assignment operator "
                         "must not be constant."),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style::ErrorText()));
  }

  frontend::SourceRange range;
};

struct BinaryOperatorTypeMismatch {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "binary-operator-type-mismatch";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Mismatched types `%s` and `%s` in binary operator.",
                         lhs_type, rhs_type),
        diagnostic::SourceQuote(src).Highlighted(
            range, diagnostic::Style::ErrorText()));
  }

  type::Type lhs_type;
  type::Type rhs_type;
  frontend::SourceRange range;
};

struct NoMatchingBinaryOperator {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "no-matching-binary-operator";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("No matching binary operator for types `%s` and `%s`.",
                         lhs_type, rhs_type),
        diagnostic::SourceQuote(src).Highlighted(
            range, diagnostic::Style::ErrorText()));
  }

  type::Type lhs_type;
  type::Type rhs_type;
  frontend::SourceRange range;
};

type::QualType VerifyOperatorOverload(
    Compiler &c, ast::BinaryOperator const *node,
    type::Typed<ir::CompleteResultRef> const &lhs,
    type::Typed<ir::CompleteResultRef> const &rhs) {
  absl::flat_hash_set<type::Callable const *> member_types;

  ast::OverloadSet os(node->scope(), ast::BinaryOperator::Symbol(node->kind()));
  if (os.members().empty()) { return type::QualType::Error(); }
  for (auto const *member : os.members()) {
    ASSIGN_OR(continue, auto qt, c.context().qual_types(member)[0]);
    // Must be callable because we're looking at overloads for operators which
    // have previously been type-checked to ensure callability.
    auto &c = qt.type().as<type::Callable>();
    member_types.insert(&c);
  }

  c.context().SetViableOverloads(node, std::move(os));

  std::vector<type::Typed<ir::CompleteResultRef>> pos_args;
  pos_args.emplace_back(lhs);
  pos_args.emplace_back(rhs);
  // TODO: Check that we only have one return type on each of these overloads.

  return type::QualType(
      type::MakeOverloadSet(std::move(member_types))
          ->return_types(core::Arguments<type::Typed<ir::CompleteResultRef>>(
              std::move(pos_args), {}))[0],
      type::Quals::Unqualified());
}

absl::Span<type::QualType const> VerifyLogicalOperator(
    Compiler &c, ast::BinaryOperator const *node, type::QualType lhs_qual_type,
    type::QualType rhs_qual_type, type::Type return_type) {
  auto quals =
      (lhs_qual_type.quals() & rhs_qual_type.quals() & ~type::Quals::Ref());
  if (lhs_qual_type.type() == type::Bool and
      rhs_qual_type.type() == type::Bool) {
    return c.context().set_qual_type(node, type::QualType(return_type, quals));
  } else {
    // TODO: Calling with constants?
    auto qt = VerifyOperatorOverload(
        c, node,
        type::Typed<ir::CompleteResultRef>(ir::CompleteResultRef(),
                                           lhs_qual_type.type()),
        type::Typed<ir::CompleteResultRef>(ir::CompleteResultRef(),
                                           rhs_qual_type.type()));
    if (not qt.ok()) {
      c.diag().Consume(InvalidBinaryOperatorOverload{
          .op    = std::string(ast::BinaryOperator::Symbol(node->kind())),
          .range = frontend::SourceRange(node->lhs().range().end(),
                                         node->rhs().range().begin()),
      });
    }
    return c.context().set_qual_type(node, qt);
  }
}

absl::Span<type::QualType const> VerifyFlagsOperator(
    Compiler &c, ast::BinaryOperator const *node, type::QualType lhs_qual_type,
    type::QualType rhs_qual_type, type::Type return_type) {
  auto quals =
      (lhs_qual_type.quals() & rhs_qual_type.quals() & ~type::Quals::Ref());
  if (lhs_qual_type.type().is<type::Flags>() and
      rhs_qual_type.type().is<type::Flags>()) {
    if (lhs_qual_type.type() == rhs_qual_type.type()) {
      return c.context().set_qual_type(node,
                                       type::QualType(return_type, quals));
    } else {
      c.diag().Consume(BinaryOperatorTypeMismatch{
          .lhs_type = lhs_qual_type.type(),
          .rhs_type = rhs_qual_type.type(),
          .range    = frontend::SourceRange(node->lhs().range().end(),
                                         node->rhs().range().begin()),
      });
      return c.context().set_qual_type(node, type::QualType::Error());
    }
  } else {
    // TODO: Calling with constants?
    auto qt = VerifyOperatorOverload(
        c, node,
        type::Typed<ir::CompleteResultRef>(ir::CompleteResultRef(),
                                           lhs_qual_type.type()),
        type::Typed<ir::CompleteResultRef>(ir::CompleteResultRef(),
                                           rhs_qual_type.type()));
    if (not qt.ok()) {
      c.diag().Consume(InvalidBinaryOperatorOverload{
          .op    = std::string(ast::BinaryOperator::Symbol(node->kind())),
          .range = frontend::SourceRange(node->lhs().range().end(),
                                         node->rhs().range().begin()),
      });
    }
    return c.context().set_qual_type(node, qt);
  }
}

absl::Span<type::QualType const> VerifyArithmeticOperator(
    Compiler &c, ast::BinaryOperator const *node, type::QualType lhs_qual_type,
    type::QualType rhs_qual_type, type::Type return_type) {
  auto quals =
      (lhs_qual_type.quals() & rhs_qual_type.quals() & ~type::Quals::Ref());
  bool check_user_overload = not(lhs_qual_type.type().is<type::Primitive>() or
                                 lhs_qual_type.type().is<type::Pointer>()) or
                             not(rhs_qual_type.type().is<type::Primitive>() or
                                 rhs_qual_type.type().is<type::Pointer>());
  if (check_user_overload) {
    // TODO: Calling with constants?
    auto qt = VerifyOperatorOverload(
        c, node,
        type::Typed<ir::CompleteResultRef>(ir::CompleteResultRef(),
                                           lhs_qual_type.type()),
        type::Typed<ir::CompleteResultRef>(ir::CompleteResultRef(),
                                           rhs_qual_type.type()));
    if (not qt.ok()) {
      c.diag().Consume(InvalidBinaryOperatorOverload{
          .op    = std::string(ast::BinaryOperator::Symbol(node->kind())),
          .range = frontend::SourceRange(node->lhs().range().end(),
                                         node->rhs().range().begin()),
      });
    }
    return c.context().set_qual_type(node, qt);
  } else if (type::IsNumeric(lhs_qual_type.type()) and
             type::IsNumeric(rhs_qual_type.type())) {
    // TODO: This check makes sense for assignment versions of operators.
    auto common_type = type::Meet(rhs_qual_type.type(), lhs_qual_type.type());
    if (common_type) {
      return c.context().set_qual_type(node,
                                        type::QualType(return_type, quals));
    } else {
      c.diag().Consume(BinaryOperatorTypeMismatch{
          .lhs_type = lhs_qual_type.type(),
          .rhs_type = rhs_qual_type.type(),
          .range    = frontend::SourceRange(node->lhs().range().end(),
                                         node->rhs().range().begin()),
      });
      return c.context().set_qual_type(node,type::QualType::Error());
    }
  } else if ((node->kind() == ast::BinaryOperator::Kind::Add or
              node->kind() == ast::BinaryOperator::Kind::AddEq) and
             (lhs_qual_type.type().is<type::BufferPointer>() and
              type::IsIntegral(rhs_qual_type.type()))) {
    return c.context().set_qual_type(node, lhs_qual_type);
  } else if (node->kind() == ast::BinaryOperator::Kind::Add and
             (rhs_qual_type.type().is<type::BufferPointer>() and
              type::IsIntegral(lhs_qual_type.type()))) {
    // TODO: This one isn't actually allowed if the operator is +=. This
    // code-reuse only makes sense for operators that work symmetrically on the
    // types.
    return c.context().set_qual_type(node, rhs_qual_type);
  } else if ((node->kind() == ast::BinaryOperator::Kind::Sub or
              node->kind() == ast::BinaryOperator::Kind::SubEq) and
             lhs_qual_type.type().is<type::BufferPointer>() and
             type::IsIntegral(rhs_qual_type.type())) {
    return c.context().set_qual_type(node, lhs_qual_type);
  } else if (node->kind() == ast::BinaryOperator::Kind::Sub and
             lhs_qual_type.type().is<type::BufferPointer>() and
             lhs_qual_type.type() == rhs_qual_type.type()) {
    return c.context().set_qual_type(node, type::QualType(type::I64, quals));

  } else {
    c.diag().Consume(NoMatchingBinaryOperator{
        .lhs_type = lhs_qual_type.type(),
        .rhs_type = rhs_qual_type.type(),
        .range    = frontend::SourceRange(node->lhs().range().end(),
                                       node->rhs().range().begin()),
    });
    return c.context().set_qual_type(node,type::QualType::Error());
  }
}

absl::Span<type::QualType const> VerifyArithmeticAssignmentOperator(
    Compiler &c, ast::BinaryOperator const *node, type::QualType lhs_qual_type,
    type::QualType rhs_qual_type, type::Type return_type) {
  if (lhs_qual_type.quals() >= type::Quals::Const() or
      not(lhs_qual_type.quals() >= type::Quals::Ref())) {
    c.diag().Consume(InvalidAssignmentOperatorLhsValueCategory{
        .range = node->lhs().range(),
    });
  }
  return VerifyArithmeticOperator(c, node, lhs_qual_type, rhs_qual_type,
                                  type::Void);
}

std::optional<std::pair<type::QualType, type::QualType>> VerifyOperands(
    Compiler &c, ast::BinaryOperator const *node) {
  auto lhs_qts = c.VerifyType(&node->lhs());
  auto rhs_qts = c.VerifyType(&node->rhs());

  bool error = false;
  if (lhs_qts.size() != 1) {
    c.diag().Consume(UnexpandedBinaryOperatorArgument{
        .num_arguments = lhs_qts.size(),
        .range         = node->lhs().range(),
    });
    error = true;
  }

  if (rhs_qts.size() != 1) {
    c.diag().Consume(UnexpandedBinaryOperatorArgument{
        .num_arguments = rhs_qts.size(),
        .range         = node->rhs().range(),
    });
    error = true;
  }

  if (error or not lhs_qts[0].ok() or not rhs_qts[0].ok()) {
    return std::nullopt;
  }

  return std::make_pair(lhs_qts[0], rhs_qts[0]);
}

}  // namespace

absl::Span<type::QualType const> Compiler::VerifyType(
    ast::BinaryOperator const *node) {
  auto result = VerifyOperands(*this, node);
  if (not result) {
    return context().set_qual_type(node, type::QualType::Error());
  }
  auto [lhs_qual_type, rhs_qual_type] = *result;

  switch (node->kind()) {
    case ast::BinaryOperator::Kind::SymbolXorEq:
    case ast::BinaryOperator::Kind::SymbolAndEq:
    case ast::BinaryOperator::Kind::SymbolOrEq: {
      if (lhs_qual_type.quals() >= type::Quals::Const() or
          not(lhs_qual_type.quals() >= type::Quals::Ref())) {
        diag().Consume(InvalidAssignmentOperatorLhsValueCategory{
            .range = node->lhs().range(),
        });
      }
      if (lhs_qual_type.type() == rhs_qual_type.type() and
          (lhs_qual_type.type() == type::Bool or
           lhs_qual_type.type().is<type::Flags>())) {
        return context().set_qual_type(node, lhs_qual_type);
      } else {
        diag().Consume(LogicalAssignmentNeedsBoolOrFlags{
            .kind  = node->kind(),
            .range = frontend::SourceRange(node->lhs().range().end(),
                                           node->rhs().range().begin()),
        });
        return context().set_qual_type(node, type::QualType::Error());
      }
    } break;
    case ast::BinaryOperator::Kind::Xor:
    case ast::BinaryOperator::Kind::And:
    case ast::BinaryOperator::Kind::Or:
      return VerifyLogicalOperator(
          *this, node, lhs_qual_type, rhs_qual_type,
          type::Meet(lhs_qual_type.type(), rhs_qual_type.type()));
    case ast::BinaryOperator::Kind::SymbolXor:
    case ast::BinaryOperator::Kind::SymbolAnd:
    case ast::BinaryOperator::Kind::SymbolOr:
      return VerifyFlagsOperator(
          *this, node, lhs_qual_type, rhs_qual_type,
          type::Meet(lhs_qual_type.type(), rhs_qual_type.type()));
    case ast::BinaryOperator::Kind::Add:
    case ast::BinaryOperator::Kind::Sub:
    case ast::BinaryOperator::Kind::Mul:
    case ast::BinaryOperator::Kind::Div:
    case ast::BinaryOperator::Kind::Mod:
      return VerifyArithmeticOperator(
          *this, node, lhs_qual_type, rhs_qual_type,
          type::Meet(lhs_qual_type.type(), rhs_qual_type.type()));
    case ast::BinaryOperator::Kind::AddEq:
    case ast::BinaryOperator::Kind::SubEq:
    case ast::BinaryOperator::Kind::MulEq:
    case ast::BinaryOperator::Kind::DivEq:
    case ast::BinaryOperator::Kind::ModEq:
      return VerifyArithmeticAssignmentOperator(*this, node, lhs_qual_type,
                                                rhs_qual_type, type::Void);

    default: UNREACHABLE();
  }
  UNREACHABLE(node->kind());
}

bool Compiler::VerifyPatternType(ast::BinaryOperator const *node,
                                 type::Type t) {
  context().set_qual_type(node, type::QualType::Constant(t));
  switch (node->kind()) {
    case ast::BinaryOperator::Kind::Add:
    case ast::BinaryOperator::Kind::Sub:
    case ast::BinaryOperator::Kind::Mul: {
      // TODO: Support non-builtin types.
      if (node->rhs().covers_binding()) {
        if (node->lhs().covers_binding()) {
          NOT_YET();
        } else {
          VerifyType(&node->lhs());
          EnqueueVerifyPatternMatchType(&node->rhs(), t);
        }
      }

      if (node->lhs().covers_binding()) {
        if (node->lhs().covers_binding()) {
          VerifyType(&node->rhs());
          EnqueueVerifyPatternMatchType(&node->lhs(), t);
        } else {
          NOT_YET();
        }
      }
    } break;

    default: NOT_YET(node->DebugString());
  }
  return true;
}

}  // namespace compiler

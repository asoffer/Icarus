#include "compiler/compiler.h"
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

absl::Span<type::QualType const> VerifyLogicalOperator(
    Compiler *c, std::string_view op, ast::BinaryOperator const *node,
    type::QualType lhs_qual_type, type::QualType rhs_qual_type,
    type::Type return_type) {
  auto quals =
      (lhs_qual_type.quals() & rhs_qual_type.quals() & ~type::Quals::Ref());
  if (lhs_qual_type.type() == type::Bool and
      rhs_qual_type.type() == type::Bool) {
    return c->context().set_qual_type(node, type::QualType(return_type, quals));
  } else {
    // TODO: Calling with constants?
    auto qt = c->VerifyBinaryOverload(
        op, node,
        type::Typed<ir::CompleteResultRef>(ir::CompleteResultRef(),
                                           lhs_qual_type.type()),
        type::Typed<ir::CompleteResultRef>(ir::CompleteResultRef(),
                                           rhs_qual_type.type()));
    if (not qt.ok()) {
      c->diag().Consume(InvalidBinaryOperatorOverload{
          .op    = std::string(op),
          .range = frontend::SourceRange(node->lhs().range().end(),
                                         node->rhs().range().begin()),
      });
    }
    return c->context().set_qual_type(node, qt);
  }
}

absl::Span<type::QualType const> VerifyFlagsOperator(
    Compiler *c, std::string_view op, ast::BinaryOperator const *node,
    type::QualType lhs_qual_type, type::QualType rhs_qual_type,
    type::Type return_type) {
  auto quals =
      (lhs_qual_type.quals() & rhs_qual_type.quals() & ~type::Quals::Ref());
  if (lhs_qual_type.type().is<type::Flags>() and
      rhs_qual_type.type().is<type::Flags>()) {
    if (lhs_qual_type.type() == rhs_qual_type.type()) {
      return c->context().set_qual_type(node,
                                        type::QualType(return_type, quals));
    } else {
      c->diag().Consume(BinaryOperatorTypeMismatch{
          .lhs_type = lhs_qual_type.type(),
          .rhs_type = rhs_qual_type.type(),
          .range    = frontend::SourceRange(node->lhs().range().end(),
                                         node->rhs().range().begin()),
      });
      return c->context().set_qual_type(node, type::QualType::Error());
    }
  } else {
    // TODO: Calling with constants?
    auto qt = c->VerifyBinaryOverload(
        op, node,
        type::Typed<ir::CompleteResultRef>(ir::CompleteResultRef(),
                                           lhs_qual_type.type()),
        type::Typed<ir::CompleteResultRef>(ir::CompleteResultRef(),
                                           rhs_qual_type.type()));
    if (not qt.ok()) {
      c->diag().Consume(InvalidBinaryOperatorOverload{
          .op    = std::string(op),
          .range = frontend::SourceRange(node->lhs().range().end(),
                                         node->rhs().range().begin()),
      });
    }
    return c->context().set_qual_type(node, qt);
  }
}

absl::Span<type::QualType const> VerifyArithmeticOperator(
    Compiler *c, std::string_view op, ast::BinaryOperator const *node,
    type::QualType lhs_qual_type, type::QualType rhs_qual_type,
    type::Type return_type) {
  auto quals =
      (lhs_qual_type.quals() & rhs_qual_type.quals() & ~type::Quals::Ref());
  bool check_user_overload = not(lhs_qual_type.type().is<type::Primitive>() or
                                 lhs_qual_type.type().is<type::Pointer>()) or
                             not(rhs_qual_type.type().is<type::Primitive>() or
                                 rhs_qual_type.type().is<type::Pointer>());
  if (check_user_overload) {
    // TODO: Calling with constants?
    auto qt = c->VerifyBinaryOverload(
        op, node,
        type::Typed<ir::CompleteResultRef>(ir::CompleteResultRef(),
                                           lhs_qual_type.type()),
        type::Typed<ir::CompleteResultRef>(ir::CompleteResultRef(),
                                           rhs_qual_type.type()));
    if (not qt.ok()) {
      c->diag().Consume(InvalidBinaryOperatorOverload{
          .op    = std::string(op),
          .range = frontend::SourceRange(node->lhs().range().end(),
                                         node->rhs().range().begin()),
      });
    }
    return c->context().set_qual_type(node, qt);
  } else if (type::IsNumeric(lhs_qual_type.type()) and
             type::IsNumeric(rhs_qual_type.type())) {
    // TODO: This check makes sense for assignment versions of operators.
    auto common_type = type::Meet(rhs_qual_type.type(), lhs_qual_type.type());
    if (common_type) {
      return c->context().set_qual_type(node,
                                        type::QualType(return_type, quals));
    } else {
      c->diag().Consume(BinaryOperatorTypeMismatch{
          .lhs_type = lhs_qual_type.type(),
          .rhs_type = rhs_qual_type.type(),
          .range    = frontend::SourceRange(node->lhs().range().end(),
                                         node->rhs().range().begin()),
      });
      return c->context().set_qual_type(node,type::QualType::Error());
    }
  } else if (op == "+" and (lhs_qual_type.type().is<type::BufferPointer>() and
                            type::IsIntegral(rhs_qual_type.type()))) {
    return c->context().set_qual_type(node, lhs_qual_type);
  } else if (op == "+" and (rhs_qual_type.type().is<type::BufferPointer>() and
                            type::IsIntegral(lhs_qual_type.type()))) {
    // TODO: This one isn't actually allowed if the operator is +=. This
    // code-reuse only makes sense for operators that work symmetrically on the
    // types.
    return c->context().set_qual_type(node, rhs_qual_type);
  } else if (op == "-" and lhs_qual_type.type().is<type::BufferPointer>() and
             type::IsIntegral(rhs_qual_type.type())) {
    return c->context().set_qual_type(node, lhs_qual_type);
  } else if (op == "-" and lhs_qual_type.type().is<type::BufferPointer>() and
             lhs_qual_type.type() == rhs_qual_type.type()) {
    return c->context().set_qual_type(node, type::QualType(type::I64, quals));

  } else {
    c->diag().Consume(NoMatchingBinaryOperator{
        .lhs_type = lhs_qual_type.type(),
        .rhs_type = rhs_qual_type.type(),
        .range    = frontend::SourceRange(node->lhs().range().end(),
                                       node->rhs().range().begin()),
    });
    return c->context().set_qual_type(node,type::QualType::Error());
  }
}

absl::Span<type::QualType const> VerifyArithmeticAssignmentOperator(
    Compiler *c, std::string_view op, ast::BinaryOperator const *node,
    type::QualType lhs_qual_type, type::QualType rhs_qual_type,
    type::Type return_type) {
  if (lhs_qual_type.quals() >= type::Quals::Const() or
      not(lhs_qual_type.quals() >= type::Quals::Ref())) {
    c->diag().Consume(InvalidAssignmentOperatorLhsValueCategory{
        .range = node->lhs().range(),
    });
  }
  return VerifyArithmeticOperator(c, op, node, lhs_qual_type, rhs_qual_type,
                                  type::Void);
}

}  // namespace

absl::Span<type::QualType const> Compiler::VerifyType(ast::BinaryOperator const *node) {
  auto lhs_qts = VerifyType(&node->lhs());
  auto rhs_qts = VerifyType(&node->rhs());

  bool error = false;
  if (lhs_qts.size() != 1) {
    diag().Consume(UnexpandedBinaryOperatorArgument{
        .num_arguments = lhs_qts.size(),
        .range         = node->lhs().range(),
    });
    error = true;
  }

  if (rhs_qts.size() != 1) {
    diag().Consume(UnexpandedBinaryOperatorArgument{
        .num_arguments = rhs_qts.size(),
        .range         = node->rhs().range(),
    });
    error = true;
  }

  if (error) { return context().set_qual_type(node, type::QualType::Error()); }

  auto lhs_qual_type = lhs_qts[0];
  auto rhs_qual_type = rhs_qts[0];

  if (not lhs_qual_type.ok() or not rhs_qual_type.ok()) {
    return context().set_qual_type(node, type::QualType::Error());
  }

  switch (node->kind()) {
    using frontend::Operator;
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
      return VerifyLogicalOperator(
          this, "xor", node, lhs_qual_type, rhs_qual_type,
          type::Meet(lhs_qual_type.type(), rhs_qual_type.type()));
    case ast::BinaryOperator::Kind::And:
      return VerifyLogicalOperator(
          this, "and", node, lhs_qual_type, rhs_qual_type,
          type::Meet(lhs_qual_type.type(), rhs_qual_type.type()));
    case ast::BinaryOperator::Kind::Or: {
      // Note: Block pipes are extracted in the parser so there's no need to
      // type-check them here. They will never be expressed in the syntax tree
      // as a binary operator.
      return VerifyLogicalOperator(
          this, "or", node, lhs_qual_type, rhs_qual_type,
          type::Meet(lhs_qual_type.type(), rhs_qual_type.type()));
    }
    case ast::BinaryOperator::Kind::SymbolXor:
      return VerifyFlagsOperator(
          this, "^", node, lhs_qual_type, rhs_qual_type,
          type::Meet(lhs_qual_type.type(), rhs_qual_type.type()));
    case ast::BinaryOperator::Kind::SymbolAnd:
      return VerifyFlagsOperator(
          this, "&", node, lhs_qual_type, rhs_qual_type,
          type::Meet(lhs_qual_type.type(), rhs_qual_type.type()));
    case ast::BinaryOperator::Kind::SymbolOr: {
      // Note: Block pipes are extracted in the parser so there's no need to
      // type-check them here. They will never be expressed in the syntax tree
      // as a binary operator.
      return VerifyFlagsOperator(
          this, "|", node, lhs_qual_type, rhs_qual_type,
          type::Meet(lhs_qual_type.type(), rhs_qual_type.type()));
    }
    case ast::BinaryOperator::Kind::Add:
      return VerifyArithmeticOperator(
          this, "+", node, lhs_qual_type, rhs_qual_type,
          type::Meet(lhs_qual_type.type(), rhs_qual_type.type()));
    case ast::BinaryOperator::Kind::Sub:
      return VerifyArithmeticOperator(
          this, "-", node, lhs_qual_type, rhs_qual_type,
          type::Meet(lhs_qual_type.type(), rhs_qual_type.type()));
    case ast::BinaryOperator::Kind::Mul:
      return VerifyArithmeticOperator(
          this, "*", node, lhs_qual_type, rhs_qual_type,
          type::Meet(lhs_qual_type.type(), rhs_qual_type.type()));
    case ast::BinaryOperator::Kind::Div:
      return VerifyArithmeticOperator(
          this, "/", node, lhs_qual_type, rhs_qual_type,
          type::Meet(lhs_qual_type.type(), rhs_qual_type.type()));
    case ast::BinaryOperator::Kind::Mod:
      return VerifyArithmeticOperator(
          this, "%", node, lhs_qual_type, rhs_qual_type,
          type::Meet(lhs_qual_type.type(), rhs_qual_type.type()));
    case ast::BinaryOperator::Kind::AddEq:
      return VerifyArithmeticAssignmentOperator(this, "+", node, lhs_qual_type,
                                                rhs_qual_type, type::Void);
    case ast::BinaryOperator::Kind::SubEq:
      return VerifyArithmeticAssignmentOperator(this, "-", node, lhs_qual_type,
                                                rhs_qual_type, type::Void);
    case ast::BinaryOperator::Kind::MulEq:
      return VerifyArithmeticAssignmentOperator(this, "*", node, lhs_qual_type,
                                                rhs_qual_type, type::Void);
    case ast::BinaryOperator::Kind::DivEq:
      return VerifyArithmeticAssignmentOperator(this, "/", node, lhs_qual_type,
                                                rhs_qual_type, type::Void);
    case ast::BinaryOperator::Kind::ModEq:
      return VerifyArithmeticAssignmentOperator(this, "%", node, lhs_qual_type,
                                                rhs_qual_type, type::Void);

    default: UNREACHABLE();
  }
  UNREACHABLE(node->kind());
}

bool Compiler::VerifyPatternType(ast::BinaryOperator const *node,
                                 type::Type t) {
  context().set_qual_type(node, type::QualType::Constant(t));
  switch (node->kind()) {
    using frontend::Operator;
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

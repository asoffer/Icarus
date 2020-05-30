#include "compiler/compiler.h"
#include "diagnostic/errors.h"
#include "type/pointer.h"
#include "type/primitive.h"
#include "type/qual_type.h"

namespace compiler {
namespace {

struct InvalidBinaryOperatorOverload {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName = "invalid-binary-operator-overload";

   diagnostic:: DiagnosticMessage ToMessage(frontend::Source const *src) const {
     return diagnostic::DiagnosticMessage(
         diagnostic::Text("No valid operator overload for (%s)"),
         diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
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
                         OperatorToString(op)),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  frontend::Operator op;
  frontend::SourceRange range;

  private:
   static std::string_view OperatorToString(frontend::Operator op) {
     if (op == frontend::Operator::XorEq) { return "^="; }
     if (op == frontend::Operator::AndEq) { return "&="; }
     if (op == frontend::Operator::OrEq) { return "|="; }
     UNREACHABLE();
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
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  frontend::SourceRange range;
};

struct ArithmeticBinaryOperatorTypeMismatch {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName =
      "arithmetic-binary-operator-type-mismatch";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Mismatched types `%s` and `%s` in binary operator.",
                         lhs_type->to_string(), rhs_type->to_string()),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  type::Type const *lhs_type;
  type::Type const *rhs_type;
  frontend::SourceRange range;
};

struct NoMatchingBinaryOperator {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "no-matching-binary-operator";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("No matching binary operator for types %s and %s."),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  type::Type const *lhs_type;
  type::Type const *rhs_type;
  frontend::SourceRange range;
};

type::QualType VerifyArithmeticOperator(Compiler *c, std::string_view op,
                                        ast::BinaryOperator const *node,
                                        type::QualType lhs_qual_type,
                                        type::QualType rhs_qual_type,
                                        type::Type const *return_type) {
  auto quals =
      (lhs_qual_type.quals() & rhs_qual_type.quals() & ~type::Quals::Ref());
  bool check_user_overload = not lhs_qual_type.type()->is<type::Primitive>() and
                             not rhs_qual_type.type()->is<type::Primitive>();
  if (check_user_overload) {
    auto qt = c->VerifyBinaryOverload(op, node, lhs_qual_type.type(),
                                      rhs_qual_type.type());
    if (not qt.ok()) {
      c->diag().Consume(InvalidBinaryOperatorOverload{
          .op    = std::string(op),
          .range = node->range(),
      });
    }
    return qt;
  } else if (type::IsNumeric(lhs_qual_type.type()) and
             type::IsNumeric(rhs_qual_type.type())) {
    if (lhs_qual_type.type() == rhs_qual_type.type()) {
      return c->data().set_qual_type(node, type::QualType(return_type, quals));
    } else {
      c->diag().Consume(ArithmeticBinaryOperatorTypeMismatch{
          .lhs_type = lhs_qual_type.type(),
          .rhs_type = rhs_qual_type.type(),
          .range    = node->range(),
      });
      return type::QualType::Error();
    }
  } else {
    c->diag().Consume(NoMatchingBinaryOperator{
        .lhs_type = lhs_qual_type.type(),
        .rhs_type = rhs_qual_type.type(),
        .range    = node->range(),
    });
    return type::QualType::Error();
  }
}

type::QualType VerifyArithmeticAssignmentOperator(
    Compiler *c, std::string_view op, ast::BinaryOperator const *node,
    type::QualType lhs_qual_type, type::QualType rhs_qual_type,
    type::Type const *return_type) {
  if (lhs_qual_type.quals() >= type::Quals::Const() or
      not(lhs_qual_type.quals() >= type::Quals::Ref())) {
    c->diag().Consume(InvalidAssignmentOperatorLhsValueCategory{
        .range = node->lhs()->range(),
    });
  }
  return VerifyArithmeticOperator(c, op, node, lhs_qual_type, rhs_qual_type,
                                  type::Void());
}

}  // namespace

type::QualType Compiler::VerifyType(ast::BinaryOperator const *node) {
  auto lhs_qual_type = VerifyType(node->lhs());
  auto rhs_qual_type = VerifyType(node->rhs());
  if (not lhs_qual_type.ok() or not rhs_qual_type.ok()) {
    return type::QualType::Error();
  }

  switch (node->op()) {
    using frontend::Operator;
    case Operator::XorEq:
    case Operator::AndEq:
    case Operator::OrEq: {
      if (lhs_qual_type.quals() >= type::Quals::Const() or
          not(lhs_qual_type.quals() >= type::Quals::Ref())) {
        diag().Consume(InvalidAssignmentOperatorLhsValueCategory{
            .range = node->lhs()->range(),
        });
      }
      if (lhs_qual_type.type() == rhs_qual_type.type() and
          (lhs_qual_type.type() == type::Bool or
           lhs_qual_type.type()->is<type::Flags>())) {
        return data().set_qual_type(node, lhs_qual_type);
      } else {
        diag().Consume(LogicalAssignmentNeedsBoolOrFlags{
            .op    = node->op(),
            .range = node->range(),
        });
        return type::QualType::Error();
      }
    } break;
    case Operator::Add:
      return VerifyArithmeticOperator(this, "+", node, lhs_qual_type,
                                      rhs_qual_type, lhs_qual_type.type());
    case Operator::Sub:
      return VerifyArithmeticOperator(this, "-", node, lhs_qual_type,
                                      rhs_qual_type, lhs_qual_type.type());
    case Operator::Mul:
      return VerifyArithmeticOperator(this, "*", node, lhs_qual_type,
                                      rhs_qual_type, lhs_qual_type.type());
    case Operator::Div:
      return VerifyArithmeticOperator(this, "/", node, lhs_qual_type,
                                      rhs_qual_type, lhs_qual_type.type());
    case Operator::Mod:
      return VerifyArithmeticOperator(this, "%", node, lhs_qual_type,
                                      rhs_qual_type, lhs_qual_type.type());
    case Operator::AddEq:
      return VerifyArithmeticAssignmentOperator(this, "+", node, lhs_qual_type,
                                                rhs_qual_type, type::Void());
    case Operator::SubEq:
      return VerifyArithmeticAssignmentOperator(this, "-", node, lhs_qual_type,
                                                rhs_qual_type, type::Void());
    case Operator::MulEq:
      return VerifyArithmeticAssignmentOperator(this, "*", node, lhs_qual_type,
                                                rhs_qual_type, type::Void());
    case Operator::DivEq:
      return VerifyArithmeticAssignmentOperator(this, "/", node, lhs_qual_type,
                                                rhs_qual_type, type::Void());
    case Operator::ModEq:
      return VerifyArithmeticAssignmentOperator(this, "%", node, lhs_qual_type,
                                                rhs_qual_type, type::Void());

    default: UNREACHABLE();
  }
  UNREACHABLE(stringify(node->op()));
}

}  // namespace compiler
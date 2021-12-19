#include <functional>

#include "compiler/compiler.h"
#include "compiler/module.h"
#include "compiler/type_for_diagnostic.h"
#include "compiler/verify/common.h"
#include "type/cast.h"
#include "type/overload_set.h"
#include "type/pointer.h"
#include "type/primitive.h"
#include "type/qual_type.h"

namespace compiler {
namespace {

struct UnexpandedBinaryOperatorArgument {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName =
      "unexpanded-binary-operator-argument";

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

struct LogicalBinaryOperatorNeedsBool {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName =
      "logical-binary-operator-needs-bool";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Operator '%s' must be called with boolean arguments.",
                         ast::BinaryOperator::Symbol(kind)),
        diagnostic::SourceQuote(src).Highlighted(
            range, diagnostic::Style::ErrorText()));
  }

  ast::BinaryOperator::Kind kind;
  frontend::SourceRange range;
};

struct InvalidAssignmentOperatorLhsValueCategory {
  static constexpr std::string_view kCategory = "value-category-error";
  static constexpr std::string_view kName =
      "invalid-assignment-lhs-value-category";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Lefthand-side of binary logical assignment operator "
                         "must not be constant."),
        diagnostic::SourceQuote(src).Highlighted(
            range, diagnostic::Style::ErrorText()));
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
        diagnostic::Text(
            "No matching binary operator (%s) for types `%s` and `%s`.", op,
            lhs_type, rhs_type),
        diagnostic::SourceQuote(src).Highlighted(
            range, diagnostic::Style::ErrorText()));
  }

  std::string op;
  std::string lhs_type;
  std::string rhs_type;
  frontend::SourceRange range;
};

template <
    base::one_of<ast::BinaryOperator, ast::BinaryAssignmentOperator> NodeType>
type::QualType VerifyOperatorOverload(
    Compiler &c, ast::BinaryOperator const *node,
    type::Typed<ir::CompleteResultRef> const &lhs,
    type::Typed<ir::CompleteResultRef> const &rhs) {
  absl::flat_hash_set<type::Function const *> member_types;
  absl::flat_hash_set<ast::Declaration::Id const *> members;

  auto symbol = NodeType::Symbol(node->kind());
  ast::OverloadSet os;

  auto get_ids = [&](Context const &ctx, ast::Declaration::Id const *id) {
    members.insert(id);
    member_types.insert(&ctx.qual_types(id)[0].type().as<type::Function>());
  };

  for (auto const &t : {lhs.type(), rhs.type()}) {
    // TODO: Checking defining_module only when this is a struct is wrong. We
    // should also handle pointers to structs, ec
    if (auto const *dm = DefiningModule(t)) {
      if (c.resources().module == dm) { continue; }
      dm->scope().ForEachDeclIdTowardsRoot(
          symbol, [&](ast::Declaration::Id const *id) {
            if (id->declaration().hashtags.contains(ir::Hashtag::Export)) {
              get_ids(dm->as<CompiledModule>().context(), id);
            }
            return true;
          });
    }
  }
  node->scope()->ForEachDeclIdTowardsRoot(symbol,
                                          [&](ast::Declaration::Id const *id) {
                                            get_ids(c.context(), id);
                                            return true;
                                          });

  for (auto const *id : members) { os.insert(id); }

  if (os.members().empty()) { return type::QualType::Error(); }
  for (auto const *member : os.members()) {
    if (auto qts = c.context().maybe_qual_type(member); not qts.empty()) {
      ASSIGN_OR(continue, auto qt, qts[0]);
      // Must be callable because we're looking at overloads for operators which
      // have previously been type-checked to ensure callability.
      auto &c = qt.type().as<type::Function>();
      member_types.insert(&c);
    }
  }

  c.context().SetViableOverloads(node, std::move(os));

  ASSERT(member_types.size() == 1u);

  return type::QualType((*member_types.begin())->return_types()[0],
                        type::Quals::Unqualified());
}

template <char C>
absl::Span<type::QualType const> VerifyArithmeticOperator(
    Compiler &c, ast::BinaryOperator const *node, type::QualType lhs_qt,
    type::QualType rhs_qt) {
  if (type::IsNumeric(lhs_qt.type()) and type::IsNumeric(rhs_qt.type())) {
    if (auto t = type::Meet(rhs_qt.type(), lhs_qt.type())) {
      auto quals = (lhs_qt.quals() & rhs_qt.quals() & ~type::Quals::Ref());
      return c.context().set_qual_type(node, type::QualType(t, quals));
    } else {
      c.diag().Consume(BinaryOperatorTypeMismatch{
          .lhs_type = lhs_qt.type(),
          .rhs_type = rhs_qt.type(),
          .range    = frontend::SourceRange(node->lhs().range().end(),
                                         node->rhs().range().begin()),
      });
      return c.context().set_qual_type(node, type::QualType::Error());
    }
  }

  if constexpr (C == '+') {
    if (lhs_qt.type().is<type::BufferPointer>() and
        type::IsIntegral(rhs_qt.type())) {
      return c.context().set_qual_type(node, lhs_qt);
    } else if (rhs_qt.type().is<type::BufferPointer>() and
               type::IsIntegral(lhs_qt.type())) {
      return c.context().set_qual_type(node, rhs_qt);
    }
  }

  if constexpr (C == '-') {
    if (lhs_qt.type().is<type::BufferPointer>()) {
      if (type::IsIntegral(rhs_qt.type())) {
        return c.context().set_qual_type(node, lhs_qt);
      } else if (lhs_qt.type() == rhs_qt.type()) {
        auto quals = (lhs_qt.quals() & rhs_qt.quals() & ~type::Quals::Ref());
        return c.context().set_qual_type(
            node, type::QualType(
                      type::PointerDifferenceType(c.resources().architecture),
                      quals));
      }
    }
  }

  // TODO: Calling with constants?
  auto qt = VerifyOperatorOverload<ast::BinaryOperator>(
      c, node,
      type::Typed<ir::CompleteResultRef>(ir::CompleteResultRef(),
                                         lhs_qt.type()),
      type::Typed<ir::CompleteResultRef>(ir::CompleteResultRef(),
                                         rhs_qt.type()));
  if (not qt.ok()) {
    c.diag().Consume(NoMatchingBinaryOperator{
        .op       = std::string(ast::BinaryOperator::Symbol(node->kind())),
        .lhs_type = TypeForDiagnostic(&node->lhs(), c.context()),
        .rhs_type = TypeForDiagnostic(&node->rhs(), c.context()),
        .range    = frontend::SourceRange(node->lhs().range().end(),
                                       node->rhs().range().begin()),
    });
  }
  return c.context().set_qual_type(node, qt);
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

template <char C>
absl::Span<type::QualType const> VerifyArithmeticAssignment(
    Compiler &c, ast::BinaryAssignmentOperator const *node,
    type::QualType lhs_qt, type::QualType rhs_qt) {
  if (type::IsNumeric(lhs_qt.type()) and type::IsNumeric(rhs_qt.type())) {
    if (type::CanCastImplicitly(rhs_qt.type(), lhs_qt.type())) {
      return c.context().set_qual_types(node, {});
    } else {
      c.diag().Consume(BinaryOperatorTypeMismatch{
          .lhs_type = lhs_qt.type(),
          .rhs_type = rhs_qt.type(),
          .range    = frontend::SourceRange(node->lhs().range().end(),
                                         node->rhs().range().begin()),
      });
      return c.context().set_qual_type(node, type::QualType::Error());
    }
  }

  if constexpr (C == '+') {
    if (lhs_qt.type().is<type::BufferPointer>() and
        type::IsIntegral(rhs_qt.type())) {
      return c.context().set_qual_types(node, {});
    }
  }

  if constexpr (C == '-') {
    if (lhs_qt.type().is<type::BufferPointer>() and
        type::IsIntegral(rhs_qt.type())) {
      return c.context().set_qual_types(node, {});
    }
  }

  // TODO: Calling with constants?
  auto qt = VerifyOperatorOverload<ast::BinaryAssignmentOperator>(
      c, node,
      type::Typed<ir::CompleteResultRef>(ir::CompleteResultRef(),
                                         lhs_qt.type()),
      type::Typed<ir::CompleteResultRef>(ir::CompleteResultRef(),
                                         rhs_qt.type()));
  if (not qt.ok()) {
    c.diag().Consume(NoMatchingBinaryOperator{
        .op = std::string(ast::BinaryAssignmentOperator::Symbol(node->kind())),
        .lhs_type = TypeForDiagnostic(&node->lhs(), c.context()),
        .rhs_type = TypeForDiagnostic(&node->rhs(), c.context()),
        .range    = frontend::SourceRange(node->lhs().range().end(),
                                       node->rhs().range().begin()),
    });
  }

  return c.context().set_qual_type(node, type::QualType::Error());
}

}  // namespace

absl::Span<type::QualType const> Compiler::VerifyType(
    ast::BinaryOperator const *node) {
  auto result = VerifyOperands(*this, node);
  if (not result) {
    return context().set_qual_type(node, type::QualType::Error());
  }
  auto [lhs_qt, rhs_qt] = *result;

  switch (node->kind()) {
    case ast::BinaryOperator::Kind::Xor:
    case ast::BinaryOperator::Kind::And:
    case ast::BinaryOperator::Kind::Or: {
      if (lhs_qt.type() == type::Bool and rhs_qt.type() == type::Bool) {
        auto quals = (lhs_qt.quals() & rhs_qt.quals() & ~type::Quals::Ref());
        type::QualType qt(type::Meet(lhs_qt.type(), rhs_qt.type()), quals);
        return context().set_qual_type(node, qt);
      } else {
        // `and`, `or`, and `xor` cannot be overloaded.

        // TODO: Get an actual range for the operator.
        diag().Consume(LogicalBinaryOperatorNeedsBool{
            .kind  = node->kind(),
            .range = frontend::SourceRange(node->lhs().range().end(),
                                           node->rhs().range().begin()),
        });
      }
      auto qt = type::QualType::NonConstant(type::Bool);
      qt.MarkError();
      return context().set_qual_type(node, qt);
    }
    case ast::BinaryOperator::Kind::SymbolXor:
    case ast::BinaryOperator::Kind::SymbolAnd:
    case ast::BinaryOperator::Kind::SymbolOr: {
      if (lhs_qt.type().is<type::Flags>() and rhs_qt.type().is<type::Flags>()) {
        if (auto t = type::Meet(rhs_qt.type(), lhs_qt.type())) {
          auto quals = (lhs_qt.quals() & rhs_qt.quals() & ~type::Quals::Ref());
          return context().set_qual_type(node, type::QualType(t, quals));
        } else {
          diag().Consume(BinaryOperatorTypeMismatch{
              .lhs_type = lhs_qt.type(),
              .rhs_type = rhs_qt.type(),
              .range    = frontend::SourceRange(node->lhs().range().end(),
                                             node->rhs().range().begin()),
          });
          return context().set_qual_type(node, type::QualType::Error());
        }
      } else {
        auto qt = VerifyOperatorOverload<ast::BinaryOperator>(
            *this, node,
            type::Typed<ir::CompleteResultRef>(ir::CompleteResultRef(),
                                               lhs_qt.type()),
            type::Typed<ir::CompleteResultRef>(ir::CompleteResultRef(),
                                               rhs_qt.type()));
        if (not qt.ok()) {
          diag().Consume(NoMatchingBinaryOperator{
              .op = std::string(ast::BinaryOperator::Symbol(node->kind())),
              .lhs_type = TypeForDiagnostic(&node->lhs(), context()),
              .rhs_type = TypeForDiagnostic(&node->rhs(), context()),
              .range    = frontend::SourceRange(node->lhs().range().end(),
                                             node->rhs().range().begin()),
          });
        }
        return context().set_qual_type(node, qt);
      }
    }
    case ast::BinaryOperator::Kind::Add:
      return VerifyArithmeticOperator<'+'>(*this, node, lhs_qt, rhs_qt);
    case ast::BinaryOperator::Kind::Sub:
      return VerifyArithmeticOperator<'-'>(*this, node, lhs_qt, rhs_qt);
    case ast::BinaryOperator::Kind::Mul:
      return VerifyArithmeticOperator<'*'>(*this, node, lhs_qt, rhs_qt);
    case ast::BinaryOperator::Kind::Div:
      return VerifyArithmeticOperator<'/'>(*this, node, lhs_qt, rhs_qt);
    case ast::BinaryOperator::Kind::Mod:
      return VerifyArithmeticOperator<'%'>(*this, node, lhs_qt, rhs_qt);
  }
}

absl::Span<type::QualType const> Compiler::VerifyType(
    ast::BinaryAssignmentOperator const *node) {
  auto result = VerifyOperands(*this, node);
  if (not result) {
    return context().set_qual_type(node, type::QualType::Error());
  }
  auto [lhs_qt, rhs_qt] = *result;

  if (lhs_qt.quals() >= type::Quals::Const() or
      not(lhs_qt.quals() >= type::Quals::Ref())) {
    diag().Consume(InvalidAssignmentOperatorLhsValueCategory{
        .range = node->lhs().range(),
    });
  }

  switch (node->kind()) {
    case ast::BinaryOperator::Kind::SymbolXor:
    case ast::BinaryOperator::Kind::SymbolAnd:
    case ast::BinaryOperator::Kind::SymbolOr: {
      if (lhs_qt.type() == rhs_qt.type() and lhs_qt.type().is<type::Flags>()) {
        return context().set_qual_type(node, lhs_qt);
      } else {
        auto qt = VerifyOperatorOverload<ast::BinaryOperator>(
            *this, node,
            type::Typed<ir::CompleteResultRef>(ir::CompleteResultRef(),
                                               lhs_qt.type()),
            type::Typed<ir::CompleteResultRef>(ir::CompleteResultRef(),
                                               rhs_qt.type()));
        if (not qt.ok()) {
          diag().Consume(NoMatchingBinaryOperator{
              .op = std::string(ast::BinaryOperator::Symbol(node->kind())),
              .lhs_type = TypeForDiagnostic(&node->lhs(), context()),
              .rhs_type = TypeForDiagnostic(&node->rhs(), context()),
              .range    = frontend::SourceRange(node->lhs().range().end(),
                                             node->rhs().range().begin()),
          });
        }
        return context().set_qual_type(node, qt);
      }
    } break;
    case ast::BinaryOperator::Kind::Add:
      return VerifyArithmeticAssignment<'+'>(*this, node, lhs_qt, rhs_qt);
    case ast::BinaryOperator::Kind::Sub:
      return VerifyArithmeticAssignment<'-'>(*this, node, lhs_qt, rhs_qt);
    case ast::BinaryOperator::Kind::Mul:
      return VerifyArithmeticAssignment<'*'>(*this, node, lhs_qt, rhs_qt);
    case ast::BinaryOperator::Kind::Div:
      return VerifyArithmeticAssignment<'/'>(*this, node, lhs_qt, rhs_qt);
    case ast::BinaryOperator::Kind::Mod:
      return VerifyArithmeticAssignment<'%'>(*this, node, lhs_qt, rhs_qt);
    default: UNREACHABLE(node->kind()); break;
  }
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

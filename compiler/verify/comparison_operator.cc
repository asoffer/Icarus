#include "compiler/common.h"
#include "compiler/verify/common.h"
#include "compiler/verify/verify.h"
#include "type/array.h"
#include "type/cast.h"
#include "type/pointer.h"
#include "type/primitive.h"
#include "type/qual_type.h"
#include "type/struct.h"

namespace compiler {
namespace {

struct ComparingIncomparables {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "comparing-incomparables";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "Values of type `%s` and `%s` are being compared but no such "
            "comparison is allowed:",
            lhs, rhs),
        diagnostic::SourceQuote(&view.buffer())
            .Highlighted(view.range(), diagnostic::Style::ErrorText()));
  }

  type::Type lhs;
  type::Type rhs;
  frontend::SourceView view;
};

// TODO: Consider combining this with unary and binary overloads.
struct InvalidComparisonOperatorOverload {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName =
      "invalid-comparison-operator-overload";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("No valid operator overload for (%s)", op),
        diagnostic::SourceQuote(&view.buffer())
            .Highlighted(view.range(), diagnostic::Style::ErrorText()));
  }

  std::string op;
  frontend::SourceView view;
};

struct NoMatchingComparisonOperator {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName = "no-matching-comparison-operator";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("No matching comparison operator for types %s and %s.",
                         lhs, rhs),
        diagnostic::SourceQuote(&view.buffer())
            .Highlighted(view.range(), diagnostic::Style::ErrorText()));
  }

  type::Type lhs;
  type::Type rhs;
  frontend::SourceView view;
};

// NOTE: the order of these enumerators is meaningful and relied upon! They are
// ordered from strongest relation to weakest.
enum class ComparisonKind {
  Order,     // All operators <, <=, ==, !=, >=, > are allowed.
  Equality,  // Only == and != are allowed.
  None       // No comparison is allowed.
};

ComparisonKind Comparator(type::Type t) {
  using cmp_t = std::underlying_type_t<ComparisonKind>;
  if (auto const *p = t.if_as<type::BufferPointer>()) {
    return ComparisonKind::Order;
  }

  if (auto const *p = t.if_as<type::Pointer>()) {
    return ComparisonKind::Equality;
  }
  if (auto const *a = t.if_as<type::Array>()) {
    return static_cast<ComparisonKind>(
        std::min(static_cast<cmp_t>(Comparator(a->data_type())),
                 static_cast<cmp_t>(ComparisonKind::Equality)));
  } else if (auto *p = t.if_as<type::Primitive>()) {
    return type::IsNumeric(p) ? ComparisonKind::Order
                              : ComparisonKind::Equality;
  } else if (t.is<type::Flags>() or t.is<type::BufferPointer>()) {
    return ComparisonKind::Order;
  } else if (t.is<type::Enum>() or t.is<type::Pointer>()) {
    return ComparisonKind::Equality;
  } else {
    return ComparisonKind::None;
  }
}

type::QualType VerifyBinaryOverload(
    Context &context, std::string_view symbol, ast::Expression const *node,
    type::Typed<ir::CompleteResultRef> const &lhs,
    type::Typed<ir::CompleteResultRef> const &rhs) {
  absl::flat_hash_set<type::Function const *> member_types;

  ForEachDeclIdTowardsRoot(
      node->scope(), symbol, [&](ast::Declaration::Id const *id) {
        ASSIGN_OR(return false, auto qt, context.qual_types(id)[0]);
        // Must be callable because we're looking at overloads for operators
        // which have previously been type-checked to ensure callability.
        auto &c = qt.type().as<type::Function>();
        member_types.insert(&c);
        return true;
      });

  if (member_types.empty()) { return type::QualType::Error(); }

  ASSERT(member_types.size() == 1u);
  // TODO: Check that we only have one return type on each of these overloads.
  return type::QualType((*member_types.begin())->return_types()[0],
                        type::Quals::Unqualified());
}

}  // namespace

absl::Span<type::QualType const> TypeVerifier::VerifyType(
    ast::ComparisonOperator const *node) {
  std::vector<type::QualType> expr_qts;
  expr_qts.reserve(node->exprs().size());
  type::Quals quals = ~type::Quals::Ref();
  for (auto *expr : node->exprs()) {
    expr_qts.push_back(VerifyType(expr)[0]);
    quals &= expr_qts.back().quals();
  }

  type::QualType qt(type::Bool, quals);
  for (auto expr_qt : expr_qts) {
    if (not expr_qt.ok()) {
      qt.MarkError();
      return context().set_qual_type(node, qt);
    }
  }

  ASSERT(node->exprs().size() >= 2u);

  size_t i      = 0;
  auto lhs_iter = expr_qts.begin();
  auto rhs_iter = std::next(lhs_iter);
  auto op_iter  = node->ops().begin();
  for (; rhs_iter != expr_qts.end(); ++lhs_iter, ++rhs_iter, ++op_iter, ++i) {
    type::QualType const &lhs_qual_type = *lhs_iter;
    type::QualType const &rhs_qual_type = *rhs_iter;
    auto op                             = *op_iter;

    if (lhs_qual_type.type().is<type::Struct>() or
        lhs_qual_type.type().is<type::Struct>()) {
      // TODO: struct is wrong. generally user-defined (could be array of
      // struct too, or perhaps a variant containing a struct?) need to
      // figure out the details here.

      const char *token = nullptr;
      switch (op) {
        case frontend::Operator::Lt: token = "<"; break;
        case frontend::Operator::Le: token = "<="; break;
        case frontend::Operator::Eq: token = "=="; break;
        case frontend::Operator::Ne: token = "!="; break;
        case frontend::Operator::Ge: token = ">="; break;
        case frontend::Operator::Gt: token = ">"; break;
        default: UNREACHABLE(node->DebugString());
      }
      // TODO: Calling with constants?
      auto result = VerifyBinaryOverload(
          context(), token, node,
          type::Typed<ir::CompleteResultRef>(ir::CompleteResultRef(),
                                             lhs_qual_type.type()),
          type::Typed<ir::CompleteResultRef>(ir::CompleteResultRef(),
                                             rhs_qual_type.type()));
      qt.remove_constant();

      if (not result.ok()) {
        diag().Consume(InvalidComparisonOperatorOverload{
            .op   = token,
            .view = frontend::SourceView(SourceBufferFor(node),
                                         node->binary_range(i)),
        });
        qt.MarkError();
      } else if (result.type() != type::Bool) {
        // Return-types for comparison operator overloads are required to be
        // bools. `VerifyBinaryOverload` is responsible for emitting diagnostics
        // if this is not the case.
        qt.MarkError();
      }
    } else if (type::Type common_type =
                   type::Meet(lhs_qual_type.type(), rhs_qual_type.type())) {
      auto cmp = Comparator(lhs_qual_type.type());

      switch (op) {
        case frontend::Operator::Eq:
        case frontend::Operator::Ne: {
          switch (cmp) {
            case ComparisonKind::Order:
            case ComparisonKind::Equality: continue;
            case ComparisonKind::None:
              diag().Consume(ComparingIncomparables{
                  .lhs  = lhs_qual_type.type(),
                  .rhs  = rhs_qual_type.type(),
                  .view = frontend::SourceView(SourceBufferFor(node),
                                               node->binary_range(i)),
              });
              qt.MarkError();
              continue;
          }
        } break;
        case frontend::Operator::Lt:
        case frontend::Operator::Le:
        case frontend::Operator::Ge:
        case frontend::Operator::Gt: {
          switch (cmp) {
            case ComparisonKind::Order: continue;
            case ComparisonKind::Equality:
            case ComparisonKind::None:
              diag().Consume(ComparingIncomparables{
                  .lhs  = lhs_qual_type.type(),
                  .rhs  = rhs_qual_type.type(),
                  .view = frontend::SourceView(SourceBufferFor(node),
                                               node->binary_range(i)),
              });
              qt.MarkError();
              continue;
          }
        } break;
        default: UNREACHABLE("Expecting a ComparisonOperator operator type.");
      }
    } else {
      diag().Consume(NoMatchingComparisonOperator{
          .lhs  = lhs_qual_type.type(),
          .rhs  = rhs_qual_type.type(),
          .view = frontend::SourceView(SourceBufferFor(node),
                                       node->binary_range(i)),
      });

      qt.MarkError();
      continue;
    }
  }

  return context().set_qual_type(node, qt);
}

}  // namespace compiler

#include <optional>
#include <string_view>

#include "compiler/compiler.h"
#include "core/fn_args.h"
#include "diagnostic/errors.h"
#include "ir/value/value.h"
#include "type/callable.h"
#include "type/overload_set.h"
#include "type/typed_value.h"

namespace compiler {
namespace {

type::Typed<ir::Value> EvaluateIfConstant(Compiler &c,
                                          ast::Expression const *expr,
                                          type::QualType qt) {
  if (qt.constant()) {
    DEBUG_LOG("EvaluateIfConstant")
    ("Evaluating constant: ", expr->DebugString());
    auto maybe_val = c.Evaluate(type::Typed(expr, qt.type()));
    if (maybe_val) { return type::Typed<ir::Value>(*maybe_val, qt.type()); }
    c.diag().Consume(diagnostic::EvaluationFailure{
        .failure = maybe_val.error(),
        .range   = expr->range(),
    });
  }
  return type::Typed<ir::Value>(ir::Value(), qt.type());
}

}  // namespace

std::optional<core::FnArgs<type::Typed<ir::Value>, std::string_view>>
Compiler::VerifyFnArgs(
    core::FnArgs<ast::Expression const *, std::string_view> const &args) {
  bool err      = false;
  auto arg_vals = args.Transform([&](ast::Expression const *expr) {
    auto expr_qual_type = VerifyType(expr);
    err |= not expr_qual_type.ok();
    if (err) {
      DEBUG_LOG("VerifyFnArgs")("Error with: ", expr->DebugString());
      return type::Typed<ir::Value>(ir::Value(), nullptr);
    }
    DEBUG_LOG("VerifyFnArgs")("constant: ", expr->DebugString());
    return EvaluateIfConstant(*this, expr, expr_qual_type);
  });

  if (err) { return std::nullopt; }
  return arg_vals;
}

// TODO: Replace `symbol` with an enum.
type::QualType Compiler::VerifyUnaryOverload(char const *symbol,
                                             ast::Expression const *node,
                                             type::Type const *operand_type) {
  type::Quals quals = type::Quals::All() & ~type::Quals::Const();
  absl::flat_hash_set<type::Callable const *> member_types;

  module::ForEachDeclTowardsRoot(
      node->scope(), symbol, [&](ast::Expression const *expr) {
        ASSIGN_OR(return false, auto qt, qual_type_of(expr));
        // Must be callable because we're looking at overloads for operators
        // which have previously been type-checked to ensure callability.
        auto &c = qt.type()->as<type::Callable>();
        quals &= qt.quals();
        member_types.insert(&c);
        return true;
      });

  std::vector<type::Typed<ir::Value>> pos_args;
  pos_args.emplace_back(ir::Value(), operand_type);

  if (member_types.empty()) { return type::QualType::Error(); }
  return type::QualType(type::MakeOverloadSet(std::move(member_types))
                            ->return_types(core::FnArgs<type::Typed<ir::Value>>(
                                std::move(pos_args), {})),
                        quals);
}

type::QualType Compiler::VerifyBinaryOverload(char const *symbol,
                                              ast::Expression const *node,
                                              type::Type const *lhs_type,
                                              type::Type const *rhs_type) {
  type::Quals quals = type::Quals::All();
  absl::flat_hash_set<type::Callable const *> member_types;

  module::ForEachDeclTowardsRoot(
      node->scope(), symbol, [&](ast::Expression const *expr) {
        ASSIGN_OR(return false, auto qt, qual_type_of(expr));
        // Must be callable because we're looking at overloads for operators
        // which have previously been type-checked to ensure callability.
        auto &c = qt.type()->as<type::Callable>();
        quals &= qt.quals();
        member_types.insert(&c);
        return true;
      });

  std::vector<type::Typed<ir::Value>> pos_args;
  pos_args.emplace_back(ir::Value(), lhs_type);
  pos_args.emplace_back(ir::Value(), rhs_type);

  return data().set_qual_type(
      node,
      type::QualType(type::MakeOverloadSet(std::move(member_types))
                         ->return_types(core::FnArgs<type::Typed<ir::Value>>(
                             std::move(pos_args), {})),
                     quals));
}

}  // namespace compiler

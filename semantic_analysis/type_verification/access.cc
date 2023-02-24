#include "ast/ast.h"
#include "semantic_analysis/type_system.h"
#include "semantic_analysis/type_verification/verify.h"

namespace semantic_analysis {

VerificationTask TypeVerifier::VerifyType(TypeVerifier &tv,
                                          ast::Access const *node) {
  std::span operand_qts = co_await VerifyTypeOf(node->operand());
  if (operand_qts.size() != 1) { NOT_YET("log an error"); }
  QualifiedType qt = operand_qts[0];
  if (qt.type() == Module) {
    auto index = tv.EvaluateAs<serialization::ModuleIndex>(node->operand());
    auto &m    = tv.resources().module(index);
    std::span symbols = m.LoadSymbols(node->member_name());

    absl::flat_hash_map<core::ParameterType, Context::CallableIdentifier>
        parameters_options;

    if (symbols.empty()) { NOT_YET(); }
    core::Type t;
    for (auto const &symbol : symbols) {
      module::Symbol s = tv.resources().TranslateToPrimary(index, symbol);
      t                = s.type();
      if (auto fn_type = t.get_if<core::FunctionType>(tv.type_system())) {
        parameters_options.emplace(
            fn_type->parameter_type(),
            Context::CallableIdentifier(s.as<module::TypedFunction>()));
      }
    }
    if (not parameters_options.empty()) {
      co_yield tv.ParametersOf(node, std::move(parameters_options));
    }

    // TODO: This is a gross hack where we just pick one of the symbols types.
    // We should really add support for overload sets. However, this is correct
    // if there is exactly one overload, and we never look at the value if there
    // are multiple overloads. This will likely break if the return type for the
    // overloads is different.
    co_return tv.TypeOf(node, Constant(t));
  } else if (qt.type().is<SliceType>(tv.type_system())) {
    if (node->member_name() == "data") {
      co_return tv.TypeOf(
          node, QualifiedType(BufferPointerType(tv.type_system(), Char),
                              qt.qualifiers()));
    } else if (node->member_name() == "length") {
      co_return tv.TypeOf(node, QualifiedType(U(64), qt.qualifiers()));
    } else {
      NOT_YET("Log an error: ", node->member_name());
    }
  } else {
    NOT_YET(DebugQualifiedType(qt, tv.type_system()));
  }
}

}  // namespace semantic_analysis

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
    switch (symbols.size()) {
      case 0: {
        NOT_YET();
      } break;
      case 1: {
        module::Symbol s = tv.resources().TranslateToPrimary(index, symbols[0]);
        core::Type t = s.type();
        if (auto fn_type = t.get_if<core::FunctionType>(tv.type_system())) {
          co_yield tv.ParametersOf(
              node, absl::flat_hash_map<core::ParameterType,
                                        Context::CallableIdentifier>{
                        {fn_type->parameter_type(),
                         Context::CallableIdentifier(
                             s.as<module::TypedFunction>())}});
        }
        co_return tv.TypeOf(node, Constant(t));
      } break;
      default: {
        NOT_YET();
      } break;
    }
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

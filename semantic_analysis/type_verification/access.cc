#include "ast/ast.h"
#include "semantic_analysis/type_system.h"
#include "semantic_analysis/type_verification/verify.h"

namespace semantic_analysis {
namespace {

struct MissingConstantMember {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "missing-constant-member";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("No member named `%s` in this %s.", member, type),
        diagnostic::SourceQuote()
            .Highlighted(expr_view, diagnostic::Style{})
            .Highlighted(member_view, diagnostic::Style::ErrorText()));
  }

  std::string_view expr_view;
  std::string_view member_view;
  std::string member;
  std::string type;
};

}  // namespace

VerificationTask TypeVerifier::VerifyType(ast::Access const *node) {
  std::span operand_qts = co_await VerifyTypeOf(node->operand());
  if (operand_qts.size() != 1) { NOT_YET("log an error"); }
  QualifiedType qt = operand_qts[0];
  if (qt.type() == Module) {
    if (not(qt.qualifiers() >= Qualifiers::Constant())) { NOT_YET(); }
    auto index        = EvaluateAs<serialization::ModuleIndex>(node->operand());
    auto &m           = resources().module(index);
    std::span symbols = m.LoadSymbols(node->member_name());

    absl::flat_hash_map<core::ParameterType, Context::CallableIdentifier>
        parameters_options;

    if (symbols.empty()) {
      ConsumeDiagnostic(MissingConstantMember{
          .expr_view   = node->operand()->range(),
          .member_view = node->member_range(),
          .member      = std::string{node->member_name()},
          .type        = "module",
      });
      co_return TypeOf(node, Error());
    }
    core::Type t;
    for (auto const &symbol : symbols) {
      module::Symbol s = resources().TranslateToPrimary(index, symbol);
      t                = s.type();
      if (auto fn_type = t.get_if<core::FunctionType>(type_system())) {
        parameters_options.emplace(
            fn_type->parameter_type(),
            Context::CallableIdentifier(s.as<module::TypedFunction>()));
      }
    }
    if (not parameters_options.empty()) {
      co_yield ParametersOf(node, std::move(parameters_options));
    }

    // TODO: This is a gross hack where we just pick one of the symbols types.
    // We should really add support for overload sets. However, this is correct
    // if there is exactly one overload, and we never look at the value if there
    // are multiple overloads. This will likely break if the return type for the
    // overloads is different.
    co_return TypeOf(node, Constant(t));
  } else if (qt.type().is<SliceType>(type_system())) {
    if (node->member_name() == "data") {
      co_return TypeOf(node,
                       QualifiedType(BufferPointerType(type_system(), Char),
                                     qt.qualifiers()));
    } else if (node->member_name() == "length") {
      co_return TypeOf(node, QualifiedType(U(64), qt.qualifiers()));
    } else {
      NOT_YET("Log an error: ", node->member_name());
    }
  } else if (qt.type() == Type) {
    if (not(qt.qualifiers() >= Qualifiers::Constant())) { NOT_YET(); }
    core::Type t = EvaluateAs<core::Type>(node->operand());
    if (auto e = t.get_if<EnumType>(type_system())) {
      if (e->has_member(node->member_name())) {
        co_return TypeOf(node, Constant(t));
      } else {
        ConsumeDiagnostic(MissingConstantMember{
            .expr_view   = node->operand()->range(),
            .member_view = node->member_range(),
            .member      = std::string{node->member_name()},
            .type        = "enum",
        });
        co_return TypeOf(node, Error(Constant(t)));
      }
    } else {
      NOT_YET(DebugType(t, type_system()));
    }
  } else {
    NOT_YET(DebugQualifiedType(qt, type_system()));
  }
}

}  // namespace semantic_analysis

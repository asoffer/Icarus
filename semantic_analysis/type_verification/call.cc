#include "core/call.h"

#include "absl/cleanup/cleanup.h"
#include "ast/ast.h"
#include "semantic_analysis/type_system.h"
#include "semantic_analysis/type_verification/casting.h"
#include "semantic_analysis/type_verification/verify.h"

namespace semantic_analysis {
namespace {

struct UncallableWithArguments {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "uncallable-with-arguments";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Uncallable with the given agruments."));
  }

  core::Arguments<std::string> arguments;
  std::string_view view;
};

}  // namespace

VerificationTask TypeVerifier::VerifyType(ast::Call const *node) {
  // TODO: Remove this hack. We haven't yet figured out precisely how to
  // represent compile-time-only, or functions whose return type is dependent on
  // a compile-time parameter, but supporting `builtin.foreign` is an important
  // bootstrapping step, so we simply hard-code type-checking for it. We're not
  // even checking for it robustly.
  if (ast::Access const *access = node->callee()->if_as<ast::Access>();
      access and access->member_name() == "foreign") {
    NTH_ASSERT(node->arguments().size() == 2);
    std::span name_argument_qts =
        co_await VerifyTypeOf(&node->arguments()[0].expr());
    std::span type_argument_qts =
        co_await VerifyTypeOf(&node->arguments()[1].expr());
    NTH_ASSERT(name_argument_qts.size() == 1);
    NTH_ASSERT(type_argument_qts.size() == 1);
    QualifiedType name_qt = name_argument_qts[0];
    QualifiedType type_qt = type_argument_qts[0];
    NTH_ASSERT(name_qt.type() == SliceType(type_system(), Char));
    NTH_ASSERT(name_qt.qualifiers() >= Qualifiers::Constant());
    NTH_ASSERT(type_qt.type() == Type);
    NTH_ASSERT(type_qt.qualifiers() >= Qualifiers::Constant());
    core::Type t = EvaluateAs<core::Type>(&node->arguments()[1].expr());
    if (auto fn_type = t.get_if<core::FunctionType>(type_system())) {
      absl::flat_hash_map<core::ParameterType, Context::CallableIdentifier>
          parameter_types{{fn_type->parameter_type(),
                           Context::CallableIdentifier(node->callee())}};
      co_yield ParametersOf(node, std::move(parameter_types));
      co_return TypeOf(node, Constant(t));
    } else if (t.is<core::PointerType>(type_system()) or
               t.is<BufferPointerType>(type_system())) {
      co_return TypeOf(node, QualifiedType(t));
    } else {
      NTH_UNIMPLEMENTED("{}") <<= {DebugType(t, type_system())};
    }
  } else if (ast::Access const *access = node->callee()->if_as<ast::Access>();
             access and access->member_name() == "underlying") {
    NTH_ASSERT(node->arguments().size() == 1);
    std::span enum_argument_qts =
        co_await VerifyTypeOf(&node->arguments()[0].expr());
    NTH_ASSERT(enum_argument_qts.size() == 1);
    QualifiedType enum_qt = enum_argument_qts[0];
    NTH_ASSERT(enum_qt.type().is<EnumType>(type_system()));
    co_return TypeOf(node, QualifiedType(U(64)));
  }

  std::span<absl::flat_hash_map<core::ParameterType,
                                Context::CallableIdentifier> const>
      callee_parameter_types = co_await VerifyParametersOf(node->callee());
  NTH_ASSERT(callee_parameter_types.size() == 1);

  core::Arguments<QualifiedType> arguments;
  bool has_error   = false;
  bool is_constant = true;
  for (auto const &argument : node->arguments()) {
    std::span argument_qts = co_await VerifyTypeOf(&argument.expr());
    if (argument_qts.size() != 1) {
      NTH_UNIMPLEMENTED("Log an error");
      has_error = true;
    } else if (argument_qts[0].qualifiers() >= Qualifiers::Error()) {
      has_error = true;
    } else {
      is_constant &= (argument_qts[0].qualifiers() >= Qualifiers::Constant());
      if (argument.named()) {
        arguments.named_emplace(argument.name(), argument_qts[0]);
      } else {
        arguments.pos_emplace(argument_qts[0]);
      }
    }
  }

  if (has_error) { co_return TypeOf(node, Error()); }

  std::vector<
      std::pair<core::ParameterType const, Context::CallableIdentifier> const *>
      parameter_types;

  size_t index = 0;
  for (auto const &callee_parameter_type : callee_parameter_types[0]) {
    absl::Cleanup c = [&] { ++index; };

    auto callability_result = core::Callability(
        callee_parameter_type.first.value(), arguments,
        [&](QualifiedType argument_type, core::Type parameter_type) {
          switch (CanCast(argument_type, parameter_type, type_system())) {
            case CastKind::None:
            case CastKind::Explicit: return false;
            case CastKind::Implicit:
            case CastKind::InPlace: return true;
          }
        });
    if (not callability_result.ok()) { continue; }
    parameter_types.push_back(&callee_parameter_type);
  }

  switch (parameter_types.size()) {
    case 0:
      ConsumeDiagnostic(UncallableWithArguments{});
      co_return TypeOf(node, Error());
    case 1: {
      auto const &[parameter_type, callable_identifier] = *parameter_types[0];

      std::span<core::Type const> return_types;
      if (auto const *expr = callable_identifier.expression()) {
        std::span callee_qts = co_await VerifyTypeOf(expr);
        if (callee_qts.size() != 1) { NTH_UNIMPLEMENTED(); }
        return_types = callee_qts[0]
                           .type()
                           .get<core::FunctionType>(type_system())
                           .returns();

      } else {
        auto [type, fn] = callable_identifier.function();
        return_types    = type.get<core::FunctionType>(type_system()).returns();
      }
      std::vector<QualifiedType> qts;
      qts.reserve(return_types.size());
      context().set_callee(node, &parameter_types.back()->second);
      if (is_constant) {
        for (core::Type t : return_types) { qts.push_back(Constant(t)); }
      } else {
        for (core::Type t : return_types) { qts.emplace_back(t); }
      }
      co_return TypeOf(node, std::move(qts));
    }
    default: NTH_UNIMPLEMENTED("Log an error");
  }
}

}  // namespace semantic_analysis

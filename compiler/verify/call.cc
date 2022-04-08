#include <string_view>
#include <utility>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "absl/strings/str_cat.h"
#include "absl/strings/str_format.h"
#include "absl/strings/str_join.h"
#include "ast/ast.h"
#include "compiler/common.h"
#include "compiler/common_diagnostics.h"
#include "compiler/type_for_diagnostic.h"
#include "compiler/verify/common.h"
#include "ir/value/fn.h"
#include "ir/value/slice.h"
#include "type/callable.h"
#include "type/cast.h"
#include "type/pointer.h"
#include "type/provenance.h"
#include "type/qual_type.h"
#include "type/slice.h"

namespace compiler {
namespace {

struct BuiltinError {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "builtin-error";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("%s", message),
        diagnostic::SourceQuote().Highlighted(view, diagnostic::Style{}));
  }
  std::string_view view;
  std::string message;
};

struct UserDefinedError {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "user-defined-error";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("%s", message),
        diagnostic::SourceQuote().Highlighted(view, diagnostic::Style{}));
  }
  std::string_view view;
  std::string message;
};

type::QualType VerifySliceCall(
    CompilationDataReference data, std::string_view view,
    core::Arguments<type::Typed<ir::CompleteResultRef>> const &arg_vals) {
  bool error = false;
  if (not arg_vals.named().empty()) {
    data.diag().Consume(BuiltinError{
        .view    = view,
        .message = "Built-in function `slice` cannot be called with named "
                   "arguments.",
    });
    error = true;
  }

  size_t size = arg_vals.size();
  if (size != 2u) {
    data.diag().Consume(BuiltinError{
        .view    = view,
        .message = absl::StrCat("Built-in function `slice` takes exactly two "
                                "arguments (You provided ",
                                size, ")."),
    });
    error = true;
  }

  if (error) { return type::QualType::Error(); }

  if (not arg_vals[0].type().is<type::BufferPointer>()) {
    data.diag().Consume(BuiltinError{
        .view    = view,
        .message = absl::StrCat("First argument to `slice` must be a buffer "
                                "pointer (You provided a(n) ",
                                arg_vals[0].type().to_string(), ")."),
    });
    error = true;
  }

  if (!type::CanCastImplicitly(arg_vals[1].type(), type::U64)) {
    data.diag().Consume(BuiltinError{
        .view    = view,
        .message = absl::StrCat("Second argument to `slice` must be "
                                "implicitly convertible to `u64` (You "
                                "provided `",
                                arg_vals[1].type().to_string(), "`)."),
    });
    error = true;
  }

  if (error) { return type::QualType::Error(); }

  return type::QualType::NonConstant(
      type::Slc(arg_vals[0].type().as<type::BufferPointer>().pointee()));
}

type::QualType VerifyCompilationErrorCall(
    CompilationDataReference data, std::string_view view,
    core::Arguments<type::Typed<ir::CompleteResultRef>> const &arg_vals) {
  bool error = false;
  if (not arg_vals.named().empty()) {
    data.diag().Consume(BuiltinError{
        .view    = view,
        .message = "Built-in function `compilation_error` cannot be called "
                   "with named arguments.",
    });
    error = true;
  }

  size_t size = arg_vals.size();
  if (size != 2u) {
    data.diag().Consume(BuiltinError{
        .view    = view,
        .message = absl::StrCat("Built-in function `compilation_error` takes "
                                "exactly two arguments (You provided ",
                                size, ")."),
    });
    error = true;
  }

  if (error) { return type::QualType::Error(); }

  if (arg_vals[0].type() != type::Type_) {
    data.diag().Consume(BuiltinError{
        .view    = view,
        .message = absl::StrCat("First argument to `compilation_error` must be "
                                "a type (You provided a(n) ",
                                arg_vals[0].type().to_string(), ")."),
    });
    error = true;
  }

  if (arg_vals[0]->empty()) {
    data.diag().Consume(BuiltinError{
        .view = view,
        .message =
            "First argument to `compilation_error` must be a constant."});
    error = true;
  }

  if (arg_vals[1].type() != type::Slc(type::Char)) {
    data.diag().Consume(BuiltinError{
        .view    = view,
        .message = absl::StrCat("Second argument to `compilation_error` must "
                                "be a []char (You provided a(n) ",
                                arg_vals[0].type().to_string(), ").")});
    error = true;
  }

  if (arg_vals[1]->empty()) {
    data.diag().Consume(BuiltinError{
        .view = view,
        .message =
            "Second argument to `compilation_error` must be a constant."});
    error = true;
  }

  if (error) { return type::QualType::Error(); }

  std::string_view error_text = arg_vals[1]->get<ir::Slice>();
  data.diag().Consume(
      UserDefinedError{.view = view, .message = std::string(error_text)});
  return type::QualType::NonConstant(arg_vals[0]->get<type::Type>());
}

type::QualType VerifyForeignCall(
    CompilationDataReference data, std::string_view view,
    core::Arguments<type::Typed<ir::CompleteResultRef>> const &arg_vals) {
  bool error = false;
  if (not arg_vals.named().empty()) {
    data.diag().Consume(BuiltinError{
        .view    = view,
        .message = "Built-in function `foreign` cannot be called with named "
                   "arguments.",
    });
    error = true;
  }

  size_t size = arg_vals.size();
  if (size != 2u) {
    data.diag().Consume(BuiltinError{
        .view    = view,
        .message = absl::StrCat("Built-in function `foreign` takes exactly two "
                                "arguments (You provided ",
                                size, ")."),
    });
    error = true;
  }

  if (error) { return type::QualType::Error(); }

  if (arg_vals[0].type() != type::Slc(type::Char)) {
    data.diag().Consume(BuiltinError{
        .view    = view,
        .message = absl::StrCat("First argument to `foreign` must be a "
                                "[]char (You provided a(n) ",
                                arg_vals[0].type().to_string(), ")."),
    });
    error = true;
  }

  if (arg_vals[0]->empty()) {
    data.diag().Consume(BuiltinError{
        .view    = view,
        .message = "First argument to `foreign` must be a constant."});
    error = true;
  }

  if (arg_vals[1].type() != type::Type_) {
    data.diag().Consume(BuiltinError{
        .view    = view,
        .message = absl::StrCat(
            "Second argument to `foreign` must be a type (You provided a(n) ",
            arg_vals[0].type().to_string(), ").")});
    error = true;
  }

  if (arg_vals[1]->empty()) {
    data.diag().Consume(BuiltinError{
        .view    = view,
        .message = "Second argument to `foreign` must be a constant."});
    error = true;
  }

  if (error) { return type::QualType::Error(); }

  type::Type foreign_type = arg_vals[1]->get<type::Type>();
  if (not foreign_type.is<type::Function>() and
      not foreign_type.is<type::Pointer>()) {
    data.diag().Consume(BuiltinError{
        .view    = view,
        .message = "Builtin `foreign` may only be called when the second "
                   "argument is a pointer or a function type.",
    });
    return type::QualType::Error();
  }

  return type::QualType::Constant(foreign_type);
}

}  // namespace

absl::Span<type::QualType const> TypeVerifier::VerifyType(
    ast::Call const *node) {
  LOG("Call", "Verifying %s", node->DebugString());

  ir::CompleteResultBuffer buffer;
  ASSIGN_OR(return type::QualType::ErrorSpan(),  //
                   auto arg_vals,
                   VerifyArguments(*this, node->arguments(), buffer));
  if (auto const *id = node->callee()->if_as<ast::Identifier>()) {
    absl::flat_hash_set<module::Module *> lookup_modules;
    for (auto const &arg : node->prefix_arguments()) {
      if (auto const *mod =
              type::Provenance(context().qual_types(&arg.expr())[0].type(),
                               shared_context().module_table())) {
        // TODO: Remove const_cast. Propagate through Provenance.
        lookup_modules.insert(const_cast<module::Module *>(mod));
      }
    }
    CallMetadata metadata(id->name(), node->scope(), std::move(lookup_modules));
    if (metadata.overloads().empty()) {
      diag().Consume(UndeclaredIdentifier{
          .id   = id->name(),
          .view = node->callee()->range(),
      });
      return context().set_qual_type(node, type::QualType::Error());
    }
    context().SetCallMetadata(node, std::move(metadata));
  } else {
    if (auto const *access = node->callee()->if_as<ast::Access>()) {
      auto operand_qt = VerifyType(access->operand())[0];
      if (operand_qt == type::QualType::Constant(type::Module)) {
        ASSIGN_OR(return context().set_qual_type(node, type::QualType::Error()),
                         ir::ModuleId mod_id,
                         EvaluateOrDiagnoseAs<ir::ModuleId>(access->operand()));

        if (mod_id == ir::ModuleId::Builtin()) {
          if (access->member_name() == "slice") {
            context().SetCallMetadata(
                node, CallMetadata(
                          absl::flat_hash_set<CallMetadata::callee_locator_t>{
                              static_cast<ast::Expression const *>(node)}));
            return context().set_qual_type(
                node, VerifySliceCall(*this, access->range(), arg_vals));
          }

          if (access->member_name() == "foreign") {
            context().SetCallMetadata(
                node, CallMetadata(
                          absl::flat_hash_set<CallMetadata::callee_locator_t>{
                              static_cast<ast::Expression const *>(node)}));
            return context().set_qual_type(
                node, VerifyForeignCall(*this, access->range(), arg_vals));
          }

          if (access->member_name() == "compilation_error") {
            return context().set_qual_type(
                node,
                VerifyCompilationErrorCall(*this, access->range(), arg_vals));
          }

          if (access->member_name() == "debug_ir") {
            return context().set_qual_type(
                node, type::QualType::Constant(type::Void));
          }
        } else {
          auto callee_qt = VerifyType(node->callee())[0];

          LOG("Call", "Callee's qual-type is %s", callee_qt);
          if (not callee_qt.ok()) {
            return context().set_qual_type(node, type::QualType::Error());
          }
        }

        context().SetCallMetadata(
            node, CallMetadata(access->member_name(), &importer().get(mod_id)));
      } else {
        NOT_YET();
      }
    } else {
      auto callee_qt = VerifyType(node->callee())[0];

      LOG("Call", "Callee's qual-type is %s", callee_qt);
      if (not callee_qt.ok()) {
        return context().set_qual_type(node, type::QualType::Error());
      }

      context().SetCallMetadata(
          node,
          CallMetadata(absl::flat_hash_set<CallMetadata::callee_locator_t>{
              node->callee()}));
    }
  }

  auto qts_or_errors = VerifyReturningCall(
      *this, {.call = node, .callee = node->callee(), .arguments = arg_vals});
  if (auto *errors = std::get_if<
          absl::flat_hash_map<type::Callable const *, core::CallabilityResult>>(
          &qts_or_errors)) {
    diag().Consume(UncallableError(context(), node->callee(), node->arguments(),
                                   std::move(*errors)));
    return context().set_qual_type(node, type::QualType::Error());
  }
  auto &qual_type = std::get<std::vector<type::QualType>>(qts_or_errors);
  LOG("Call", "Call qual-type is %s on %p", qual_type, &context());
  // TODO: under what circumstances can we prove that the implementation
  // doesn't need to be run at runtime?
  return context().set_qual_types(node, std::move(qual_type));
}

bool PatternTypeVerifier::VerifyPatternType(ast::Call const *node,
                                            type::Type t) {
  context().set_qual_type(node, type::QualType::Constant(t));

  // Note that the type here cannot use ADL. Maybe we could inspect the
  // expression arguments and use ADL for anything we see has a module set? But
  // that seems like a bad idea.
  ASSIGN_OR(return false,  //
                   auto qt, VerifyType(*this, node->callee())[0]);
  if (auto const *gs = qt.type().if_as<type::Generic<type::Struct>>()) {
    for (auto const &arg : node->arguments()) {
      // TODO: Having these always be types is problematic, but for now we don't
      // have a way to deduce another possibility.
      state().EnqueueVerifyPatternMatchType(&arg.expr(), type::Type_);
    }

    return true;
  } else {
    return false;
  }
}

}  // namespace compiler

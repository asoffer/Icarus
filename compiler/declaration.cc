#include "absl/container/flat_hash_map.h"
#include "ast/ast.h"
#include "compiler/common_diagnostics.h"
#include "compiler/compiler.h"
#include "jasmin/instructions/arithmetic.h"

namespace compiler {
namespace {

void EmitDefaultInitialization(core::Type t, FunctionData data, Compiler &c) {
  auto &ts = c.type_system();
  if (std::optional i = t.get_if<core::SizedIntegerType>(ts)) {
    if (i->bits() == 64) {
      if (i->is_signed()) {
        data.function().AppendConstruct<int64_t>(0);
      } else {
        data.function().AppendConstruct<uint64_t>(0);
      }
    } else if (i->bits() == 32) {
      if (i->is_signed()) {
        data.function().AppendConstruct<int32_t>(0);
      } else {
        data.function().AppendConstruct<uint32_t>(0);
      }
    } else if (i->bits() == 16) {
      if (i->is_signed()) {
        data.function().AppendConstruct<int16_t>(0);
      } else {
        data.function().AppendConstruct<uint16_t>(0);
      }
    } else if (i->bits() == 8) {
      if (i->is_signed()) {
        data.function().AppendConstruct<int8_t>(0);
      } else {
        data.function().AppendConstruct<uint8_t>(0);
      }
    } else {
      NOT_YET();
    }
  } else if (t == Bool) {
    data.function().AppendConstruct<bool>(false);
  } else if (t == Char) {
    data.function().AppendConstruct<data_types::Char>(data_types::Char('0'));
  } else if (t.is<core::PointerType>(ts) or t.is<BufferPointerType>(ts)) {
    data.function().AppendConstruct<data_types::addr_t>(nullptr);
  } else {
    NOT_YET(DebugType(t, ts));
  }
}

struct NoDefaultValue {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "no-default-value";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("There is no default value for the type `%s`.", type),
        diagnostic::SourceQuote().Highlighted(view, diagnostic::Style{}));
  }

  std::string type;
  std::string_view view;
};

struct InitializingConstantWithNonConstant {
  static constexpr std::string_view kCategory = "value-category-error";
  static constexpr std::string_view kName =
      "initializing-constant-with-nonconstant";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Cannot initialize a constant from a non-constant."),
        diagnostic::SourceQuote().Highlighted(view, diagnostic::Style{}));
  }

  std::string_view view;
};

struct NonConstantTypeInDeclaration {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName = "non-constant-type-in-declaration";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Non-constant type encountered in declaration."),
        diagnostic::SourceQuote().Highlighted(view, diagnostic::Style{}));
  }

  std::string_view view;
};

struct NoValidCast {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "no-valid-cast";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("No valid cast from a value of type `%s` to a value "
                         "of type `%s`.",
                         from, to),
        diagnostic::SourceQuote().Highlighted(view, diagnostic::Style{}));
  }

  std::string_view view;
  std::string from;
  std::string to;
};

struct OutOfBoundsConstantInteger {
  static constexpr std::string_view kCategory = "cast-error";
  static constexpr std::string_view kName = "out-of-bounds-constant-integer";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Cannot cast the integer value %s to the type `%s` "
                         "because the value would be out of range.",
                         value, type),
        diagnostic::SourceQuote().Highlighted(view, diagnostic::Style{}));
  }

  std::string_view view;
  std::string_view value;
  std::string type;
};

struct Reserved {
  static constexpr std::string_view kCategory = "name-error";
  static constexpr std::string_view kName     = "reserved";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("\"%s\" is a reserved keyword and cannot be the name "
                         "of a variable.",
                         name),
        diagnostic::SourceQuote().Highlighted(view, diagnostic::Style{}));
  }

  std::string_view name;
  std::string_view view;
};

Compiler::Task TaskForDefaultInitialization(ast::Declaration const *node,
                                            Compiler &c) {
  // Syntactically: `var: T`, or `var :: T`
  auto type_expr_task       = c.TaskFor(node->type_expr());
  std::vector type_expr_qts = type_expr_task.get<std::vector<QualifiedType>>();
  if (type_expr_qts.size() != 1) { NOT_YET("Log an error"); }
  auto type_expr_qt = type_expr_qts[0];
  if (type_expr_qt != Constant(Type)) {
    c.ConsumeDiagnostic(NonConstantTypeInDeclaration{
        .view = node->type_expr()->range(),
    });
    co_yield std::vector{Error()};
    co_return;
  }

  QualifiedType qt(c.EvaluateAs<core::Type>(type_expr_task));
  if (node->flags() & ast::Declaration::f_IsConst) { qt = Constant(qt); }

  co_yield std::vector{qt};

  auto data = co_await nth::type<FunctionData>;

  if (node->flags() & ast::Declaration::f_IsConst) {
    NOT_YET();
  } else {
    ASSERT(node->ids().size() == 1);  // TODO: Multiple identifiers.
    auto const &id = node->ids()[0];
    for (auto const &id : node->ids()) {
      data.function().AppendStackOffset(data.OffsetFor(&id));
      EmitDefaultInitialization(qt.type(), data, c);
    }
  }
}

}  // namespace

Compiler::Task Compiler::TaskFor(ast::Declaration const *node) {
  for (auto const &id : node->ids()) {
    if (id.name() == "builtin") {
      ConsumeDiagnostic(Reserved{
          .name = id.name(),
          .view = id.range(),
      });
      co_yield std::vector{Error()};
      co_return;
    }
  }

  switch (node->kind()) {
    case ast::Declaration::kDefaultInit: {
      co_await TaskForDefaultInitialization(node, *this);
    } break;
    case ast::Declaration::kInferred: {
      // Syntactically: `var := value`, or `var ::= value`
      NOT_YET();
    } break;
    case ast::Declaration::kCustomInit: {
      // Syntactically: `var: T = value`, or `var :: T = value`
      NOT_YET();
    } break;
    default: NOT_YET(node->DebugString());
  }
}

}  // namespace compiler

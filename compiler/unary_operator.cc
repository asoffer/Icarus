#include "absl/container/flat_hash_map.h"
#include "ast/ast.h"
#include "compiler/common_diagnostics.h"
#include "compiler/compiler.h"
#include "jasmin/instructions/arithmetic.h"

namespace compiler {
namespace {

struct UnexpandedUnaryOperatorArgument {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName =
      "unexpanded-unary-operator-argument";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Unary operator argument expands to %u values. Each "
                         "operand must expand to exactly 1 value.",
                         num_arguments),
        diagnostic::SourceQuote().Highlighted(view,
                                              diagnostic::Style::ErrorText()));
  }

  size_t num_arguments;
  std::string_view view;
};

struct UncopyableType {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "uncopyable-type";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Attempting to copy an uncopyable type `%s`.", from),
        diagnostic::SourceQuote().Highlighted(view,
                                              diagnostic::Style::ErrorText()));
  }

  std::string from;
  std::string_view view;
};

struct NonConstantInterface {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "non-constant-interface";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Non-constant type in interface constructor `~`"),
        diagnostic::SourceQuote().Highlighted(view,
                                              diagnostic::Style::ErrorText()));
  }

  std::string_view view;
};

struct InvalidUnaryOperatorOverload {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName = "invalid-unary-operator-overload";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("No valid operator overload for (%s)", op),
        diagnostic::SourceQuote().Highlighted(view,
                                              diagnostic::Style::ErrorText()));
  }

  char const *op;
  std::string_view view;
};

struct InvalidUnaryOperatorCall {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "invalid-unary-operator-call";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "Invalid call to unary operator (%s) with argument type `%s`", op,
            type),
        diagnostic::SourceQuote().Highlighted(view,
                                              diagnostic::Style::ErrorText()));
  }

  char const *op;
  std::string type;
  std::string_view view;
};

struct NegatingUnsignedInteger {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "negating-unsigned-integer";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "Attempting to negate an unsigned integer of type `%s`.", type),
        diagnostic::SourceQuote().Highlighted(view,
                                              diagnostic::Style::ErrorText()));
  }

  std::string type;
  std::string_view view;
};

struct NonAddressableExpression {
  static constexpr std::string_view kCategory = "value-category-error";
  static constexpr std::string_view kName     = "non-addressable-expression";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Expression is not addressable."),
        diagnostic::SourceQuote().Highlighted(view,
                                              diagnostic::Style::ErrorText()));
  }

  std::string_view view;
};

struct DereferencingNonPointer {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "dereferencing-non-pointer";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Attempting to dereference an object of type `%s` "
                         "which is not a pointer",
                         type),
        diagnostic::SourceQuote().Highlighted(view,
                                              diagnostic::Style::ErrorText()));
  }

  std::string type;
  std::string_view view;
};

Compiler::Task TaskForBufferPointer(ast::UnaryOperator const *node,
                                    Compiler::Task &operand_task,
                                    QualifiedType operand_qt, Compiler &c) {
  if (operand_qt.type() != Type) {
    c.ConsumeDiagnostic(NotAType{
        .view = node->operand()->range(),
        .type = c.TypeForDiagnostic(*node->operand()),
    });
    co_yield std::vector{Error(Constant(Type))};
    co_return;
  }
  co_yield std::vector{
      QualifiedType(Type, operand_qt.qualifiers() & ~Qualifiers::Buffer())};

  auto data = co_await nth::type<FunctionData>;
  operand_task.send<FunctionData>(data);
  operand_task.complete();

  switch (data.kind()) {
    case EmitKind::Statement: co_return;
    default: {
      data.function().AppendPush(&c.type_system());
      data.function().AppendMakeBufferPointerType();
    }
  };
}

Compiler::Task TaskForPointer(ast::UnaryOperator const *node,
                              Compiler::Task &operand_task,
                              QualifiedType operand_qt, Compiler &c) {
  core::Type operand_type       = operand_qt.type();
  Qualifiers operand_qualifiers = operand_qt.qualifiers();

  if (operand_type != Type) {
    c.ConsumeDiagnostic(NotAType{
        .view = node->operand()->range(),
        .type = c.TypeForDiagnostic(*node->operand()),
    });
    co_yield std::vector{Error()};
  }
  co_yield std::vector{
      QualifiedType(Type, operand_qualifiers & ~Qualifiers::Buffer())};

  auto data = co_await nth::type<FunctionData>;
  operand_task.send<FunctionData>(data);
  operand_task.complete();

  switch (data.kind()) {
    case EmitKind::Statement: co_return;
    default: {
      data.function().AppendPush(&c.type_system());
      data.function().AppendMakePointerType();
    }
  };
}

Compiler::Task TaskForAt(ast::UnaryOperator const *node,
                         Compiler::Task &operand_task, QualifiedType operand_qt,
                         Compiler &c) {
  QualifiedType qt;
  if (auto ptr_type =
          operand_qt.type().get_if<core::PointerType>(c.type_system())) {
    qt = Reference(ptr_type->pointee());
  } else if (auto ptr_type =
                 operand_qt.type().get_if<BufferPointerType>(c.type_system())) {
    qt = QualifiedType(ptr_type->pointee(), Qualifiers::Buffer());
  } else {
    c.ConsumeDiagnostic(DereferencingNonPointer{
        .type = c.TypeForDiagnostic(*node->operand()),
        .view = node->range(),
    });
    co_yield std::vector{Error()};
    co_return;
  }

  co_yield std::vector{qt};
  auto data = co_await nth::type<FunctionData>;
  switch (data.kind()) {
    case EmitKind::Statement: {
      KindModifier token(data, EmitKind::Value);
      operand_task.send<FunctionData>(data);
      operand_task.complete();
    } break;
    case EmitKind::Reference: {
      KindModifier token(data, EmitKind::Value);
      operand_task.send<FunctionData>(data);
      operand_task.complete();
    } break;
    default: {
      operand_task.send<FunctionData>(data);
      operand_task.complete();
      core::Bytes bytes_to_load = SizeOf(qt.type(), c.type_system());
      data.function().AppendLoad(bytes_to_load.value());
    } break;
  }
}

Compiler::Task TaskForAddress(ast::UnaryOperator const *node,
                              Compiler::Task &operand_task,
                              QualifiedType operand_qt, Compiler &c) {
  Qualifiers operand_qualifiers = operand_qt.qualifiers();
  QualifiedType qt;
  if (operand_qualifiers >= Qualifiers::Buffer()) {
    qt = QualifiedType(BufferPointerType(c.type_system(), operand_qt.type()));
  } else if (operand_qualifiers >= Qualifiers::Reference()) {
    qt = QualifiedType(core::PointerType(c.type_system(), operand_qt.type()));
  } else {
    c.ConsumeDiagnostic(NonAddressableExpression{.view = node->range()});
    co_yield std::vector{Error()};
    co_return;
  }

  co_yield std::vector{qt};
  auto data = co_await nth::type<FunctionData>;
  KindModifier token(data, EmitKind::Reference);
  operand_task.send<FunctionData>(data);
  operand_task.complete();
}

Compiler::Task TaskForNegate(ast::UnaryOperator const *node,
                             Compiler::Task &operand_task,
                             QualifiedType operand_qt, Compiler &c) {
  core::Type operand_type       = operand_qt.type();
  Qualifiers operand_qualifiers = operand_qt.qualifiers();

  if (operand_type == Integer) {
    co_yield std::vector{QualifiedType(
        operand_type, operand_qualifiers & Qualifiers::Constant())};
    auto data = co_await nth::type<FunctionData>;
    if (data.kind() == EmitKind::Statement) { co_return; }
    operand_task.send<FunctionData>(data);
    operand_task.complete();
    data.function().AppendNegate<data_types::IntegerHandle>();
  } else {
    if (operand_type == F32 or operand_type == F64) {
      co_yield std::vector{QualifiedType(
          operand_type, operand_qualifiers & Qualifiers::Constant())};
    } else if (auto i = operand_type.get_if<core::SizedIntegerType>(
                   c.type_system())) {
      if (i->is_signed()) {
        co_yield std::vector{QualifiedType(
            operand_type, operand_qualifiers & Qualifiers::Constant())};

      } else {
        c.ConsumeDiagnostic(NegatingUnsignedInteger{
            .type = c.TypeForDiagnostic(*node->operand()),
            .view = node->range(),
        });
        co_yield std::vector{
            Error(operand_qualifiers & Qualifiers::Constant())};
      }
    } else {
      c.ConsumeDiagnostic(InvalidUnaryOperatorCall{
          .op   = "-",
          .type = c.TypeForDiagnostic(*node->operand()),
          .view = node->range(),
      });
      co_yield std::vector{Error(operand_qualifiers & Qualifiers::Constant())};
      co_return;
    }

    auto data = co_await nth::type<FunctionData>;
    if (data.kind() == EmitKind::Statement) { co_return; }

    operand_task.send<FunctionData>(data);
    operand_task.complete();

    bool found = WithPrimitiveType(
        operand_type, [&]<jasmin::Negatable T>() requires(std::is_signed_v<T>) {
          data.function().AppendNegate<T>();
        });
    if (not found) { NTH_UNIMPLEMENTED("{}") <<= {DebugType(operand_type, c.type_system())}; }
  }
}

Compiler::Task TaskForNot(ast::UnaryOperator const *node,
                          Compiler::Task &operand_task,
                          QualifiedType operand_qt, Compiler &c) {
  Qualifiers operand_qualifiers = operand_qt.qualifiers();
  if (operand_qt.type() == Bool) {
    co_yield std::vector{
        QualifiedType(Bool, operand_qualifiers & Qualifiers::Constant())};
    auto data = co_await nth::type<FunctionData>;
    operand_task.send<FunctionData>(data);
    operand_task.complete();
    data.function().AppendNot();
  } else {
    c.ConsumeDiagnostic(InvalidUnaryOperatorCall{
        .op   = "not",
        .type = c.TypeForDiagnostic(*node->operand()),
        .view = node->range(),
    });
    co_yield std::vector{Error(operand_qualifiers & Qualifiers::Constant())};
  }
}

}  // namespace

Compiler::Task Compiler::TaskFor(ast::UnaryOperator const *node) {
  auto operand_task       = TaskFor(node->operand());
  std::vector operand_qts = operand_task.get<std::vector<QualifiedType>>();

  if (operand_qts.size() != 1) {
    ConsumeDiagnostic(UnexpandedUnaryOperatorArgument{
        .num_arguments = operand_qts.size(),
        .view          = node->operand()->range(),
    });

    co_yield std::vector{Error()};
    co_return;
  }

  if (operand_qts[0].qualifiers() >= Qualifiers::Error()) {
    co_yield std::vector{Error()};
    co_return;
  }

  QualifiedType operand_qt      = operand_qts[0];
  core::Type operand_type       = operand_qt.type();
  Qualifiers operand_qualifiers = operand_qt.qualifiers();

  switch (node->kind()) {
    case ast::UnaryOperator::Kind::BufferPointer:
      co_await TaskForBufferPointer(node, operand_task, operand_qt, *this);
      break;
    case ast::UnaryOperator::Kind::TypeOf:
      co_yield std::vector{Constant(Type)};
      break;
    case ast::UnaryOperator::Kind::At:
      co_await TaskForAt(node, operand_task, operand_qt, *this);
      break;
    case ast::UnaryOperator::Kind::Address:
      co_await TaskForAddress(node, operand_task, operand_qt, *this);
      break;
    case ast::UnaryOperator::Kind::Pointer:
      co_await TaskForPointer(node, operand_task, operand_qt, *this);
      break;
    case ast::UnaryOperator::Kind::Negate:
      co_await TaskForNegate(node, operand_task, operand_qt, *this);
      break;
    case ast::UnaryOperator::Kind::Not:
      co_await TaskForNot(node, operand_task, operand_qt, *this);
      break;
    default: NTH_UNREACHABLE("{}") <<= {node->DebugString()};
  }
}

}  // namespace compiler

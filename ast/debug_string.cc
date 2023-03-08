#include "absl/functional/bind_front.h"
#include "absl/strings/str_cat.h"
#include "absl/strings/str_join.h"
#include "ast/ast.h"
#include "core/type_system/type.h"
#include "data_types/char.h"
#include "nth/meta/type.h"

namespace ast {
namespace {
char const *OpStr(UnaryOperator::Kind op) {
  switch (op) {
    case UnaryOperator::Kind::Copy: return "copy ";
    case UnaryOperator::Kind::Init: return "init ";
    case UnaryOperator::Kind::Destroy: return "destroy ";
    case UnaryOperator::Kind::Move: return "move ";
    case UnaryOperator::Kind::Pointer: return "*";
    case UnaryOperator::Kind::BufferPointer: return "[*]";
    case UnaryOperator::Kind::Not: return "not";
    case UnaryOperator::Kind::Negate: return "-";
    case UnaryOperator::Kind::At: return "@";
    case UnaryOperator::Kind::Address: return "&";
    case UnaryOperator::Kind::BlockJump: return ">> ";
    case UnaryOperator::Kind::TypeOf: UNREACHABLE();
  }
}

char const *OpStr(frontend::Operator op) {
  switch (op) {
    case frontend::Operator::Add: return " + ";
    case frontend::Operator::Sub: return " - ";
    case frontend::Operator::Mul: return " * ";
    case frontend::Operator::Div: return " / ";
    case frontend::Operator::Mod: return " % ";
    case frontend::Operator::SymbolOrEq: return " |= ";
    case frontend::Operator::SymbolXorEq: return " ^= ";
    case frontend::Operator::SymbolAndEq: return " &= ";
    case frontend::Operator::AddEq: return " += ";
    case frontend::Operator::SubEq: return " -= ";
    case frontend::Operator::MulEq: return " *= ";
    case frontend::Operator::DivEq: return " /= ";
    case frontend::Operator::ModEq: return " %= ";
    case frontend::Operator::Or: return " or ";
    case frontend::Operator::Xor: return " xor ";
    case frontend::Operator::And: return " and ";
    case frontend::Operator::SymbolOr: return " | ";
    case frontend::Operator::SymbolXor: return " ^ ";
    case frontend::Operator::SymbolAnd: return " & ";
    case frontend::Operator::Lt: return " < ";
    case frontend::Operator::Le: return " <= ";
    case frontend::Operator::Eq: return " == ";
    case frontend::Operator::Ne: return " != ";
    case frontend::Operator::Ge: return " >= ";
    case frontend::Operator::Gt: return " > ";
    case frontend::Operator::Return: return "return ";
    case frontend::Operator::Yield: return "<< ";
    case frontend::Operator::Not: return "not";
    case frontend::Operator::At: return "@";
    case frontend::Operator::VariadicPack: return "..";
    case frontend::Operator::BufPtr: return "[*]";
    case frontend::Operator::Copy: return "copy ";
    case frontend::Operator::Init: return "init ";
    case frontend::Operator::Destroy: return "destroy ";
    case frontend::Operator::Move: return "move ";
    case frontend::Operator::Tilde: return "~";
    default: UNREACHABLE();
  }
}

template <typename T>
void Joiner(T &&node, std::string *out, size_t indent) {
  node->DebugStrAppend(out, indent);
}

template <typename EPtr, typename StrType>
void DumpArguments(std::string *out, size_t indent,
                   core::Arguments<EPtr, StrType> const &fnargs) {
  char const *sep = "";
  fnargs.ApplyWithIndex([&](auto &&index, EPtr const &expr) {
    absl::StrAppend(out, sep);
    if constexpr (nth::type<decltype(index)>.decayed() != nth::type<size_t>) {
      absl::StrAppend(out, index, " = ");
    }
    expr->DebugStrAppend(out, indent);
    sep = ", ";
  });
}

std::string indentation(size_t indent) { return std::string(2 * indent, ' '); }

std::string ParametersToString(core::Parameters<Declaration> const &parameters,
                               size_t indent) {
  return absl::StrJoin(parameters, ", ", [&](std::string *out, auto const &p) {
    p.value.DebugStrAppend(out, indent);
  });
}

}  // namespace

void Access::DebugStrAppend(std::string *out, size_t indent) const {
  if (operand()->is<Identifier>() or operand()->is<Index>()) {
    operand()->DebugStrAppend(out, indent);
  } else {
    absl::StrAppend(out, "(");
    operand()->DebugStrAppend(out, indent);
    absl::StrAppend(out, ")");
  }
  absl::StrAppend(out, ".", member_name());
}

void ArgumentType::DebugStrAppend(std::string *out, size_t indent) const {
  absl::StrAppend(out, "$", name());
}

void ArrayLiteral::DebugStrAppend(std::string *out, size_t indent) const {
  absl::StrAppend(out, "[",
                  absl::StrJoin(elements(), ", ",
                                [&](std::string *out, auto const &elem) {
                                  return Joiner(elem, out, indent);
                                }),
                  "]");
}

void ArrayType::DebugStrAppend(std::string *out, size_t indent) const {
  absl::StrAppend(out, "[",
                  absl::StrJoin(lengths(), ", ",
                                [&](std::string *out, auto const &elem) {
                                  return Joiner(elem, out, indent);
                                }),
                  "; ");
  data_type().DebugStrAppend(out, indent);
  absl::StrAppend(out, "]");
}

void Assignment::DebugStrAppend(std::string *out, size_t indent) const {
  absl::StrAppend(
      out,
      absl::StrJoin(lhs(), ", ",
                    [&](std::string *out, auto const &elem) {
                      return Joiner(elem, out, indent);
                    }),
      " = ",
      absl::StrJoin(rhs(), ", ", [&](std::string *out, auto const &elem) {
        return Joiner(elem, out, indent);
      }));
}

void BinaryOperator::DebugStrAppend(std::string *out, size_t indent) const {
  absl::StrAppend(out, "(");
  lhs().DebugStrAppend(out, indent);
  absl::StrAppend(out, " ", Symbol(kind()), " ");
  rhs().DebugStrAppend(out, indent);
  absl::StrAppend(out, ")");
}

void BinaryAssignmentOperator::DebugStrAppend(std::string *out,
                                              size_t indent) const {
  absl::StrAppend(out, "(");
  lhs().DebugStrAppend(out, indent);
  absl::StrAppend(out, " ", Symbol(kind()), " ");
  rhs().DebugStrAppend(out, indent);
  absl::StrAppend(out, ")");
}

void BindingDeclaration::DebugStrAppend(std::string *out, size_t indent) const {
  if (constraint()) { constraint()->DebugStrAppend(out, indent); }
  absl::StrAppend(out, "`", ids()[0].name());
}

void BlockNode::DebugStrAppend(std::string *out, size_t indent) const {
  absl::StrAppend(out, name());
  if (not parameters().empty()) {
    absl::StrAppend(out, " [", ParametersToString(parameters(), indent), "]");
  }
  absl::StrAppend(out, " {\n");
  for (auto *stmt : stmts()) {
    absl::StrAppend(out, "\n", indentation(indent));
    stmt->DebugStrAppend(out, indent + 1);
  }
  absl::StrAppend(out, indentation(indent), "}\n");
}

void AppendCallArgument(size_t indent, std::string *out,
                        Call::Argument const &arg) {
  if (arg.named()) { absl::StrAppend(out, arg.name(), " = "); }
  arg.expr().DebugStrAppend(out, indent);
}

void Call::DebugStrAppend(std::string *out, size_t indent) const {
  std::string_view sep = "";
  if (not prefix_arguments().empty()) {
    absl::StrAppend(out, "(",
                    absl::StrJoin(prefix_arguments(), ", ",
                                  absl::bind_front(AppendCallArgument, indent)),
                    ")'");
  }
  callee()->DebugStrAppend(out, indent);
  if (not postfix_arguments().empty() or arguments().empty()) {
    absl::StrAppend(out, "(",
                    absl::StrJoin(postfix_arguments(), ", ",
                                  absl::bind_front(AppendCallArgument, indent)),
                    ")");
  }
}

void Cast::DebugStrAppend(std::string *out, size_t indent) const {
  absl::StrAppend(out, "(");
  expr()->DebugStrAppend(out, indent);
  absl::StrAppend(out, ") as (");
  type()->DebugStrAppend(out, indent);
  absl::StrAppend(out, ")");
}

void ComparisonOperator::DebugStrAppend(std::string *out, size_t indent) const {
  absl::StrAppend(out, "(");
  for (size_t i = 0; i < ops().size(); ++i) {
    exprs()[i]->DebugStrAppend(out, indent);
    absl::StrAppend(out, OpStr(ops()[i]));
  }
  exprs().back()->DebugStrAppend(out, indent);
}

void Declaration::DebugStrAppend(std::string *out, size_t indent) const {
  absl::StrAppend(
      out, "(",
      absl::StrJoin(ids(), ", ",
                    [&](std::string *out, ast::Declaration::Id const &id) {
                      id.DebugStrAppend(out, indent);
                    }),
      ")");
  if (type_expr()) {
    absl::StrAppend(out, (flags() & Declaration::f_IsConst) ? " :: " : ": ");
    type_expr()->DebugStrAppend(out, indent);
    if (init_val()) {
      absl::StrAppend(out, " = ");
      init_val()->DebugStrAppend(out, indent);
    }
  } else {
    if (init_val()) {
      absl::StrAppend(out,
                      (flags() & Declaration::f_IsConst) ? " ::= " : " := ");
      init_val()->DebugStrAppend(out, indent);
    }
  }
}

void DesignatedInitializer::DebugStrAppend(std::string *out,
                                           size_t indent) const {
  type()->DebugStrAppend(out, indent);
  out->append(".{");
  for (auto const *assignment : assignments()) {
    absl::StrAppend(out, "(",
                    absl::StrJoin(assignment->lhs(), ", ",
                                  [&](std::string *out, auto const *expr) {
                                    expr->DebugStrAppend(out, indent + 1);
                                  }),
                    ") = ",
                    absl::StrJoin(assignment->rhs(), ", ",
                                  [&](std::string *out, auto const *expr) {
                                    expr->DebugStrAppend(out, indent + 1);
                                  }),
                    "\n", indentation(indent + 1));
  }
  out->append("}");
}

void EnumLiteral::DebugStrAppend(std::string *out, size_t indent) const {
  switch (kind()) {
    case EnumLiteral::Kind::Enum: absl::StrAppend(out, "enum {\n"); break;
    case EnumLiteral::Kind::Flags: absl::StrAppend(out, "flags {\n"); break;
  }
  for (std::string_view enumerator : enumerators()) {
    absl::StrAppendFormat(out, "%*s%s", 2 * indent, " ", enumerator);
    if (auto iter = specified_values().find(enumerator);
        iter != specified_values().end()) {
      absl::StrAppend(out, " ::= ");
      iter->second->DebugStrAppend(out, indent + 1);
    }
    absl::StrAppend(out, "\n");
  }
  absl::StrAppendFormat(out, "%*s}", 2 * indent, " ");
}

void FunctionLiteral::DebugStrAppend(std::string *out, size_t indent) const {
  absl::StrAppend(out, "(", ParametersToString(parameters(), indent), ") -> ");
  if (outputs()) {
    absl::StrAppend(out, "(",
                    absl::StrJoin(*outputs(), ", ",
                                  [&](std::string *out, auto const &elem) {
                                    return Joiner(elem, out, indent);
                                  }),
                    ")");
  }
  absl::StrAppend(out, "{");
  for (auto const *stmt : stmts()) {
    absl::StrAppend(out, "\n", indentation(indent));
    stmt->DebugStrAppend(out, indent + 1);
  }
  absl::StrAppend(out, "\n", indentation(indent), "}");
}

void FunctionType::DebugStrAppend(std::string *out, size_t indent) const {
  absl::StrAppend(out, "(",
                  absl::StrJoin(parameters(), ", ",
                                [&](std::string *out, auto const &p) {
                                  p->DebugStrAppend(out, indent);
                                }),
                  ") -> (",
                  absl::StrJoin(outputs(), ", ",
                                [&](std::string *out, auto const *elem) {
                                  elem->DebugStrAppend(out, indent);
                                }),
                  ")");
}

void Identifier::DebugStrAppend(std::string *out, size_t indent) const {
  absl::StrAppend(out, name());
}

void Import::DebugStrAppend(std::string *out, size_t indent) const {
  absl::StrAppend(out, "import ");
  operand()->DebugStrAppend(out, indent);
}

void Index::DebugStrAppend(std::string *out, size_t indent) const {
  lhs()->DebugStrAppend(out, indent);
  absl::StrAppend(out, "[");
  rhs()->DebugStrAppend(out, indent);
  absl::StrAppend(out, "]");
}

void Label::DebugStrAppend(std::string *out, size_t indent) const {
  absl::StrAppend(out, "#.", label_);
}

void PatternMatch::DebugStrAppend(std::string *out, size_t indent) const {
  if (is_binary()) { expr().DebugStrAppend(out, indent); }
  absl::StrAppend(out, "~");
  pattern().DebugStrAppend(out, indent);
}

void ReturnStmt::DebugStrAppend(std::string *out, size_t indent) const {
  absl::StrAppend(
      out, "return ",
      absl::StrJoin(exprs(), ", ", [&](std::string *out, auto const &elem) {
        return Joiner(elem, out, indent);
      }));
}

void YieldStmt::DebugStrAppend(std::string *out, size_t indent) const {
  absl::StrAppend(out, label_ ? std::string_view(*label_->value()) : "", "<< ",
                  absl::StrJoin(arguments(), ", ",
                                absl::bind_front(AppendCallArgument, indent)));
}

void ScopeLiteral::DebugStrAppend(std::string *out, size_t indent) const {
  absl::StrAppendFormat(out, "scope [%s] (%s) {", context().ids()[0].name(),
                        ParametersToString(parameters(), indent));
  for (auto const *stmt : stmts()) {
    absl::StrAppend(out, "\n", indentation(indent));
    stmt->DebugStrAppend(out, indent + 1);
  }
  absl::StrAppend(out, "\n", indentation(indent), "}");
}

void ScopeNode::DebugStrAppend(std::string *out, size_t indent) const {
  name()->DebugStrAppend(out, indent);
  absl::StrAppend(out, " (",
                  absl::StrJoin(arguments(), ", ",
                                absl::bind_front(AppendCallArgument, indent)),
                  ") ");
  for (auto const &block : blocks()) { block.DebugStrAppend(out, indent); }
}

void SliceType::DebugStrAppend(std::string *out, size_t indent) const {
  absl::StrAppend(out, "[]");
  data_type().DebugStrAppend(out, indent);
}

void ShortFunctionLiteral::DebugStrAppend(std::string *out,
                                          size_t indent) const {
  absl::StrAppend(out, "(", ParametersToString(parameters(), indent), ") => ");
  body()->DebugStrAppend(out, indent);
}

void StructLiteral::DebugStrAppend(std::string *out, size_t indent) const {
  absl::StrAppend(out, "struct {\n");
  for (auto const &f : fields()) {
    absl::StrAppend(out, indentation(indent));
    f.DebugStrAppend(out, indent + 1);
    absl::StrAppend(out, "\n");
  }
  absl::StrAppend(out, indentation(indent), "}");
}

void ParameterizedStructLiteral::DebugStrAppend(std::string *out,
                                                size_t indent) const {
  absl::StrAppend(out, "struct (", ParametersToString(parameters(), indent),
                  ") {\n");
  for (auto const &f : fields()) {
    absl::StrAppend(out, indentation(indent));
    f.DebugStrAppend(out, indent + 1);
    absl::StrAppend(out, "\n");
  }
  absl::StrAppend(out, indentation(indent), "}");
}

void Terminal::DebugStrAppend(std::string *out, size_t indent) const {
  if (type() == nth::type<std::string>) {
    out->append("\"");
    out->append(range());
    out->append("\"");
  } else {
    out->append(range());
  }
}

void UnaryOperator::DebugStrAppend(std::string *out, size_t indent) const {
  if (kind() == Kind::TypeOf) {
    absl::StrAppend(out, "(");
    operand()->DebugStrAppend(out, indent);
    absl::StrAppend(out, "):?");
  } else {
    absl::StrAppend(out, OpStr(kind()));
    operand()->DebugStrAppend(out, indent);
  }
}

void IfStmt::DebugStrAppend(std::string *out, size_t indent) const {
  absl::StrAppend(out, "if (");
  condition().DebugStrAppend(out, indent);
  absl::StrAppend(out, ") then {");
  for (auto const *stmt : true_block()) {
    absl::StrAppend(out, "\n", indentation(indent + 1));
    stmt->DebugStrAppend(out, indent + 1);
  }

  if (has_false_block()) {
    absl::StrAppend(out, "\n", indentation(indent), "} else {");
    for (auto const *stmt : false_block()) {
      absl::StrAppend(out, "\n", indentation(indent + 1));
      stmt->DebugStrAppend(out, indent + 1);
    }
  }
  absl::StrAppend(out, "\n", indentation(indent), "}");
}

void WhileStmt::DebugStrAppend(std::string *out, size_t indent) const {
  absl::StrAppend(out, "while (");
  condition().DebugStrAppend(out, indent);
  absl::StrAppend(out, ") do {");
  for (auto const *stmt : body()) {
    absl::StrAppend(out, "\n", indentation(indent + 1));
    stmt->DebugStrAppend(out, indent + 1);
  }
  absl::StrAppend(out, "\n", indentation(indent), "}");
}

void InterfaceLiteral::DebugStrAppend(std::string *out, size_t indent) const {
  absl::StrAppend(out, "interface [", context().ids()[0].name(), "] {\n");
  for (auto const &[name, expr] : members()) {
    absl::StrAppend(out, indentation(indent + 1), name, " :: ");
    expr->DebugStrAppend(out, indent + 1);
    absl::StrAppend(out, "\n");
  }
  absl::StrAppend(out, indentation(indent), "}");
}

}  // namespace ast

#include "absl/strings/str_cat.h"
#include "absl/strings/str_join.h"
#include "ast/ast.h"
#include "base/stringify.h"
#include "ir/results.h"

namespace ast {
namespace {
char const *OpStr(frontend::Operator op) {
  switch (op) {
    case frontend::Operator::Arrow: return " -> ";
    case frontend::Operator::Add: return " + ";
    case frontend::Operator::Sub: return " - ";
    case frontend::Operator::Mul: return " * ";
    case frontend::Operator::Div: return " / ";
    case frontend::Operator::Mod: return " % ";
    case frontend::Operator::Assign: return " = ";
    case frontend::Operator::OrEq: return " |= ";
    case frontend::Operator::XorEq: return " ^= ";
    case frontend::Operator::AndEq: return " &= ";
    case frontend::Operator::AddEq: return " += ";
    case frontend::Operator::SubEq: return " -= ";
    case frontend::Operator::MulEq: return " *= ";
    case frontend::Operator::DivEq: return " /= ";
    case frontend::Operator::ModEq: return " %= ";
    case frontend::Operator::When: return " when ";
    case frontend::Operator::Or: return " | ";
    case frontend::Operator::Xor: return " ^ ";
    case frontend::Operator::And: return " & ";
    case frontend::Operator::Lt: return " < ";
    case frontend::Operator::Le: return " <= ";
    case frontend::Operator::Eq: return " == ";
    case frontend::Operator::Ne: return " != ";
    case frontend::Operator::Ge: return " >= ";
    case frontend::Operator::Gt: return " > ";
    case frontend::Operator::Goto: return "goto ";
    case frontend::Operator::Return: return "return ";
    case frontend::Operator::Yield: return "yield ";
    case frontend::Operator::Print: return "print ";
    case frontend::Operator::Which: return "which ";
    case frontend::Operator::Not: return "!";
    case frontend::Operator::At: return "@";
    case frontend::Operator::Eval: return "$";
    case frontend::Operator::Needs: return "needs ";
    case frontend::Operator::Ensure: return "ensure ";
    case frontend::Operator::Expand: return "<< ";
    case frontend::Operator::VariadicPack: return "..";
    case frontend::Operator::BufPtr: return "[*]";
    case frontend::Operator::Copy: return "copy ";
    case frontend::Operator::Move: return "move ";
    default: UNREACHABLE();
  }
}

template <typename T>
void Joiner(T &&node, std::string *out, size_t indent) {
  node->DebugStrAppend(out, indent);
}

template <typename EPtr, typename StrType>
void DumpFnArgs(std::string *out, size_t indent,
                core::FnArgs<EPtr, StrType> const &fnargs) {
  char const *sep = "";
  fnargs.ApplyWithIndex([&](auto &&index, EPtr const &expr) {
    absl::StrAppend(out, sep);
    if constexpr (not std::is_same_v<std::decay_t<decltype(index)>, size_t>) {
      absl::StrAppend(out, index, " = ");
    }
    expr->DebugStrAppend(out, indent);
    sep = ", ";
  });
}

std::string indentation(size_t indent) { return std::string(2 * indent, ' '); }

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

void ArrayLiteral::DebugStrAppend(std::string *out, size_t indent) const {
  absl::StrAppend(out, "[",
                  absl::StrJoin(elems(), ", ",
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
  data_type()->DebugStrAppend(out, indent);
  absl::StrAppend(out, "]");
}

void Binop::DebugStrAppend(std::string *out, size_t indent) const {
  absl::StrAppend(out, "(");
  lhs()->DebugStrAppend(out, indent);
  absl::StrAppend(out, OpStr(op()));
  rhs()->DebugStrAppend(out, indent);
  absl::StrAppend(out, ")");
}

void BlockLiteral::DebugStrAppend(std::string *out, size_t indent) const {
  absl::StrAppend(out, "block {\n");

  for (auto const *b : before()) {
    absl::StrAppend(out, indentation(indent));
    b->DebugStrAppend(out, indent + 1);
    absl::StrAppend(out, "\n");
  }
  for (auto const *a : after()) {
    absl::StrAppend(out, indentation(indent));
    a->DebugStrAppend(out, indent + 1);
    absl::StrAppend(out, "\n");
  }
  absl::StrAppend(out, indentation(indent), "}\n");
}

void BlockNode::DebugStrAppend(std::string *out, size_t indent) const {
  absl::StrAppend(out, name());
  if (not args().empty()) {
    absl::StrAppend(out, " [",
                    absl::StrJoin(args(), ", ",
                                  [&](std::string *out, auto const &elem) {
                                    return Joiner(elem, out, indent);
                                  }),
                    "]");
  }
  absl::StrAppend(out, " {\n");
  for (auto *stmt : stmts()) {
    absl::StrAppend(out, "\n", indentation(indent));
    stmt->DebugStrAppend(out, indent + 1);
  }
  absl::StrAppend(out, indentation(indent), "}\n");
}

void Jump::DebugStrAppend(std::string *out, size_t indent) const {
  absl::StrAppend(out, "jump(",
                  absl::StrJoin(params(), ", ",
                                [&](std::string *out, auto const &p) {
                                  p.value->DebugStrAppend(out, indent);
                                }),
                  ")");

  for (auto *stmt : stmts()) {
    absl::StrAppend(out, "\n", indentation(indent));
    stmt->DebugStrAppend(out, indent + 1);
  }
  absl::StrAppend(out, "\n", indentation(indent), "}\n");
}

void BuiltinFn::DebugStrAppend(std::string *out, size_t indent) const {
  switch (value()) {
#define ICARUS_CORE_BUILTIN_X(enumerator, str, t)                              \
  case core::Builtin::enumerator:                                              \
    absl::StrAppend(out, str);                                                 \
    return;
#include "core/builtin.xmacro.h"
#undef ICARUS_CORE_BUILTIN_X
  }
  UNREACHABLE();
}

void Call::DebugStrAppend(std::string *out, size_t indent) const {
  callee()->DebugStrAppend(out, indent);
  absl::StrAppend(out, "(");
  DumpFnArgs(out, indent, args());
  absl::StrAppend(out, ")");
}

void Cast::DebugStrAppend(std::string *out, size_t indent) const {
  absl::StrAppend(out, "(");
  expr()->DebugStrAppend(out, indent);
  absl::StrAppend(out, ") as (");
  type()->DebugStrAppend(out, indent);
  absl::StrAppend(out, ")");
}

void ChainOp::DebugStrAppend(std::string *out, size_t indent) const {
  absl::StrAppend(out, "(");
  for (size_t i = 0; i < ops().size(); ++i) {
    exprs()[i]->DebugStrAppend(out, indent);
    absl::StrAppend(out, OpStr(ops()[i]));
  }
  exprs().back()->DebugStrAppend(out, indent);
}

void CommaList::DebugStrAppend(std::string *out, size_t indent) const {
  if (parenthesized_) {
    absl::StrAppend(out, "(",
                    absl::StrJoin(exprs_, ", ",
                                  [&](std::string *out, auto const &elem) {
                                    return Joiner(elem, out, indent);
                                  }),
                    ")");
  } else {
    absl::StrAppend(out, absl::StrJoin(exprs_, ", ",
                                       [&](std::string *out, auto const &elem) {
                                         return Joiner(elem, out, indent);
                                       }));
  }
}

void Declaration::DebugStrAppend(std::string *out, size_t indent) const {
  absl::StrAppend(out, id());
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

void EnumLiteral::DebugStrAppend(std::string *out, size_t indent) const {
  switch (kind()) {
    case EnumLiteral::Kind::Enum: absl::StrAppend(out, "enum {\n"); break;
    case EnumLiteral::Kind::Flags: absl::StrAppend(out, "flags {\n"); break;
  }
  for (auto const *elem : elems()) {
    absl::StrAppend(out, indentation(indent));
    elem->DebugStrAppend(out, indent + 1);
    absl::StrAppend(out, "\n");
  }
  absl::StrAppend(out, indentation(indent), "}");
}

void FunctionLiteral::DebugStrAppend(std::string *out, size_t indent) const {
  absl::StrAppend(out, "(",
                  absl::StrJoin(params(), ", ",
                                [&](std::string *out, auto const &p) {
                                  p.value->DebugStrAppend(out, indent);
                                }),
                  ")");

  if (is_short()) {
    absl::StrAppend(out, " => ");
    stmts()[0]->DebugStrAppend(out, indent);
  } else {
    absl::StrAppend(out, " -> ");
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
}

void Identifier::DebugStrAppend(std::string *out, size_t indent) const {
  absl::StrAppend(out, token());
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

void Goto::DebugStrAppend(std::string *out, size_t indent) const {
  absl::StrAppend(out, "goto ");
  for (auto const &opt : options()) {
    absl::StrAppend(out, opt.block(), "(");
    DumpFnArgs(out, indent, opt.args());
    absl::StrAppend(out, ")");
  }
}

void PrintStmt::DebugStrAppend(std::string *out, size_t indent) const {
  absl::StrAppend(
      out, "print ",
      absl::StrJoin(exprs(), ", ", [&](std::string *out, auto const &elem) {
        return Joiner(elem, out, indent);
      }));
}

void ReturnStmt::DebugStrAppend(std::string *out, size_t indent) const {
  absl::StrAppend(
      out, "return ",
      absl::StrJoin(exprs(), ", ", [&](std::string *out, auto const &elem) {
        return Joiner(elem, out, indent);
      }));
}

void YieldStmt::DebugStrAppend(std::string *out, size_t indent) const {
  absl::StrAppend(
      out, "yield ",
      absl::StrJoin(exprs(), ", ", [&](std::string *out, auto const &elem) {
        return Joiner(elem, out, indent);
      }));
}

void ScopeLiteral::DebugStrAppend(std::string *out, size_t indent) const {
  absl::StrAppend(out, "scope {\n");
  for (auto const *decl : decls()) {
    absl::StrAppend(out, indentation(indent));
    decl->DebugStrAppend(out, indent + 1);
    absl::StrAppend(out, "\n");
  }
  absl::StrAppend(out, indentation(indent), "}");
}

void ScopeNode::DebugStrAppend(std::string *out, size_t indent) const {
  name()->DebugStrAppend(out, indent);
  absl::StrAppend(out, " ");

  if (not args().empty()) {
    absl::StrAppend(out, "(");
    DumpFnArgs(out, indent, args());
    absl::StrAppend(out, ")");
  }
  for (auto const &block : blocks()) { block.DebugStrAppend(out, indent); }
}

void StructLiteral::DebugStrAppend(std::string *out, size_t indent) const {
  absl::StrAppend(out, "struct (",
                  absl::StrJoin(args_, ", ",
                                [&](std::string *out, Declaration const &d) {
                                  d.DebugStrAppend(out, indent);
                                }),
                  ") {\n");
  for (const auto &f : fields_) {
    absl::StrAppend(out, indentation(indent));
    f.DebugStrAppend(out, indent + 1);
    absl::StrAppend(out, "\n");
  }
  absl::StrAppend(out, indentation(indent), "}");
}

void StructType::DebugStrAppend(std::string *out, size_t indent) const {
  absl::StrAppend(out, "[",
                  absl::StrJoin(args_, ", ",
                                [&](std::string *out, auto const &elem) {
                                  return Joiner(elem, out, indent);
                                }),
                  "; struct]");
}

void Switch::DebugStrAppend(std::string *out, size_t indent) const {
  absl::StrAppend(out, "switch ");
  if (expr_) {
    absl::StrAppend(out, "(");
    expr_.get()->DebugStrAppend(out, indent);
    absl::StrAppend(out, ")");
  }
  absl::StrAppend(out, "{\n");
  for (auto const & [ body, cond ] : cases_) {
    absl::StrAppend(out, indentation(indent));
    body.get()->DebugStrAppend(out, indent + 1);
    absl::StrAppend(out, " when ");
    cond.get()->DebugStrAppend(out, indent + 1);
    absl::StrAppend(out, "\n");
  }
  absl::StrAppend(out, indentation(indent), "}");
}

void Terminal::DebugStrAppend(std::string *out, size_t indent) const {
  switch (basic_type()) {
    case type::BasicType::Int8:
      absl::StrAppend(out, as<int8_t>(), "_i8");
      return;
    case type::BasicType::Nat8:
      absl::StrAppend(out, as<uint8_t>(), "_u8");
      return;
    case type::BasicType::Int16:
      absl::StrAppend(out, as<int16_t>(), "_i16");
      return;
    case type::BasicType::Nat16:
      absl::StrAppend(out, as<uint16_t>(), "_u16");
      return;
    case type::BasicType::Int32:
      absl::StrAppend(out, as<int32_t>(), "_i32");
      return;
    case type::BasicType::Nat32:
      absl::StrAppend(out, as<uint32_t>(), "_u32");
      return;
    case type::BasicType::Int64:
      absl::StrAppend(out, as<int64_t>(), "_i64");
      return;
    case type::BasicType::Nat64:
      absl::StrAppend(out, as<uint64_t>(), "_u64");
      return;
    case type::BasicType::Float32:
      absl::StrAppend(out, as<float>(), "_f32");
      return;
    case type::BasicType::Float64:
      absl::StrAppend(out, as<double>(), "_f64");
      return;
    case type::BasicType::Type_:
      // TODO make ast not depend on this so it can once again depend ont type.
      absl::StrAppend(out, "{{unknown-type}}");
      return;
    case type::BasicType::Bool:
      absl::StrAppend(out, as<bool>() ? "true" : "false");
      return;
    case type::BasicType::ByteView:
      absl::StrAppend(out, "\"", as<std::string_view>(), "\"");
      return;

    default:;
  }
  UNREACHABLE();
}

void Unop::DebugStrAppend(std::string *out, size_t indent) const {
  if (op() == frontend::Operator::TypeOf) {
    absl::StrAppend(out, "(");
    operand()->DebugStrAppend(out, indent);
    absl::StrAppend(out, "):?");
  }
  absl::StrAppend(out, OpStr(op()));
  operand()->DebugStrAppend(out, indent);
}

}  // namespace ast

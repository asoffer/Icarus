#include "ast/methods/dump.h"

#include "absl/strings/str_cat.h"
#include "absl/strings/str_join.h"
#include "ast/ast.h"
#include "base/stringify.h"
#include "frontend/token.h"
#include "ir/results.h"
#include "type/type.h"

namespace ast {
std::string Dump::ToString(ast::Node const *node) {
  std::string result;
  Dump d(&result);
  // TODO figure out what's going wrong here.
  if (auto *tok = node->if_as<frontend::Token>()) {
    d(tok);
  } else {
    node->Dump(&d);
  }
  return result;
}

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
    case frontend::Operator::Jump: return "jump ";
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

struct Joiner {
  constexpr Joiner(Dump *d) : d_(d) {}

  template <typename Node>
  void operator()(std::string *out, Node &&node) {
    std::string *old_out = std::exchange(d_->out_, out);
    node->Dump(d_);
    d_->out_ = old_out;
  }

  Dump *d_ = nullptr;
};

template <typename EPtr, typename StrType>
void DumpFnArgs(Dump *d, core::FnArgs<EPtr, StrType> const &fnargs) {
  bool seen_one = false;
  fnargs.ApplyWithIndex([&](auto &&index, EPtr const &expr) {
    absl::StrAppend(d->out_, seen_one ? ", " : "");
    if constexpr (not std::is_same_v<std::decay_t<decltype(index)>, size_t>) {
      absl::StrAppend(d->out_, index, " = ");
    }
    expr->Dump(d);
    seen_one = true;
  });
}

}  // namespace

void Dump::operator()(ast::Access const *node) {
  if (node->operand()->is<ast::Identifier>() or
      node->operand()->is<ast::Index>()) {
    node->operand()->Dump(this);
  } else {
    absl::StrAppend(out_, "(");
    node->operand()->Dump(this);
    absl::StrAppend(out_, ")");
  }
  absl::StrAppend(out_, ".", node->member_name());
}

void Dump::operator()(ast::ArrayLiteral const *node) {
  absl::StrAppend(out_, "[", absl::StrJoin(node->elems(), ", ", Joiner{this}),
                  "]");
}

void Dump::operator()(ast::ArrayType const *node) {
  absl::StrAppend(out_, "[", absl::StrJoin(node->lengths(), ", ", Joiner{this}),
                  "; ");
  node->data_type()->Dump(this);
  absl::StrAppend(out_, "]");
}

void Dump::operator()(ast::Binop const *node) {
  absl::StrAppend(out_, "(");
  node->lhs()->Dump(this);
  absl::StrAppend(out_, OpStr(node->op()));
  node->rhs()->Dump(this);
  absl::StrAppend(out_, ")");
}

void Dump::operator()(ast::BlockLiteral const *node) {
  absl::StrAppend(out_, "block {\n");

  ++indentation_;
  for (auto const *b : node->before()) {
    absl::StrAppend(out_, indent());
    b->Dump(this);
    absl::StrAppend(out_, "\n");
  }
  for (auto const *a : node->after()) {
    absl::StrAppend(out_, indent());
    a->Dump(this);
    absl::StrAppend(out_, "\n");
  }
  --indentation_;
  absl::StrAppend(out_, indent(), "}\n");
}

void Dump::operator()(ast::BlockNode const *node) {
  absl::StrAppend(out_, node->name());
  if (not node->args().empty()) {
    absl::StrAppend(out_, " [", absl::StrJoin(node->args(), ", ", Joiner{this}),
                    "]");
  }
  absl::StrAppend(out_, " {\n");
  ++indentation_;
  for (auto *stmt : node->stmts()) {
    absl::StrAppend(out_, "\n", indent());
    stmt->Dump(this);
  }
  --indentation_;
  absl::StrAppend(out_, indent(), "}\n");
}

void Dump::operator()(ast::JumpHandler const *node) {
  absl::StrAppend(out_, "jump_handler (",
                  absl::StrJoin(node->input(), ", ", Joiner{this}), ") {\n");
  ++indentation_;
  for (auto *stmt : node->stmts()) {
    absl::StrAppend(out_, "\n", indent());
    stmt->Dump(this);
  }
  --indentation_;
  absl::StrAppend(out_, indent(), "}\n");
}

void Dump::operator()(ast::BuiltinFn const *node) {
  switch (node->value()) {
#define ICARUS_CORE_BUILTIN_X(enumerator, str, t)                              \
  case core::Builtin::enumerator:                                              \
    absl::StrAppend(out_, str);                                                \
    return;
#include "core/builtin.xmacro.h"
#undef ICARUS_CORE_BUILTIN_X
  }
  UNREACHABLE();
}

void Dump::operator()(ast::Call const *node) {
  node->callee()->Dump(this);
  absl::StrAppend(out_, "(");
  DumpFnArgs(this, node->args());
  absl::StrAppend(out_, ")");
}

void Dump::operator()(ast::Cast const *node) {
  absl::StrAppend(out_, "(");
  node->expr()->Dump(this);
  absl::StrAppend(out_, ") as (");
  node->type()->Dump(this);
  absl::StrAppend(out_, ")");
}

void Dump::operator()(ast::ChainOp const *node) {
  absl::StrAppend(out_, "(");
  for (size_t i = 0; i < node->ops().size(); ++i) {
    node->exprs()[i]->Dump(this);
    absl::StrAppend(out_, OpStr(node->ops()[i]));
  }
  node->exprs().back()->Dump(this);
}

void Dump::operator()(ast::CommaList const *node) {
  if (node->parenthesized_) {
    absl::StrAppend(out_, "(", absl::StrJoin(node->exprs_, ", ", Joiner{this}),
                    ")");
  } else {
    absl::StrAppend(out_, absl::StrJoin(node->exprs_, ", ", Joiner{this}));
  }
}

void Dump::operator()(ast::Declaration const *node) {
  absl::StrAppend(out_, node->id());
  if (node->type_expr()) {
    absl::StrAppend(
        out_, (node->flags() & ast::Declaration::f_IsConst) ? " :: " : ": ");
    node->type_expr()->Dump(this);
    if (node->init_val()) {
      absl::StrAppend(out_, " = ");
      node->init_val()->Dump(this);
    }
  } else {
    if (node->init_val()) {
      absl::StrAppend(out_, (node->flags() & ast::Declaration::f_IsConst)
                                ? " ::= "
                                : " := ");
      node->init_val()->Dump(this);
    }
  }
}

void Dump::operator()(ast::EnumLiteral const *node) {
  switch (node->kind()) {
    case ast::EnumLiteral::Kind::Enum: absl::StrAppend(out_, "enum {\n"); break;
    case ast::EnumLiteral::Kind::Flags:
      absl::StrAppend(out_, "flags {\n");
      break;
  }
  ++indentation_;
  for (auto const *elem : node->elems()) {
    absl::StrAppend(out_, indent());
    elem->Dump(this);
    absl::StrAppend(out_, "\n");
  }
  --indentation_;
  absl::StrAppend(out_, indent(), "}");
}

void Dump::operator()(ast::FunctionLiteral const *node) {
  absl::StrAppend(
      out_, "(",
      absl::StrJoin(
          node->inputs_, ", ",
          [](std::string *out,
             core::Param<std::unique_ptr<ast::Declaration>> const &p) {
            Dump dump(out);
            p.value->Dump(&dump);
          }),
      ") -> ");
  if (node->outputs_) {
    absl::StrAppend(out_, "(",
                    absl::StrJoin(*node->outputs_, ", ", Joiner{this}), ")");
  }
  absl::StrAppend(out_, "{");
  ++indentation_;
  for (auto const &stmt : node->statements_) {
    absl::StrAppend(out_, "\n", indent());
    stmt->Dump(this);
  }
  --indentation_;
  absl::StrAppend(out_, "\n", indent(), "}");
}

void Dump::operator()(ast::Identifier const *node) {
  absl::StrAppend(out_, node->token());
}

void Dump::operator()(ast::Import const *node) {
  absl::StrAppend(out_, "import ");
  node->operand()->Dump(this);
}

void Dump::operator()(ast::Index const *node) {
  node->lhs()->Dump(this);
  absl::StrAppend(out_, "[");
  node->rhs()->Dump(this);
  absl::StrAppend(out_, "]");
}

void Dump::operator()(ast::Jump const *node) {
  absl::StrAppend(out_, "jump ");
  for (auto const &opt : node->options_) {
    absl::StrAppend(out_, opt.block, "(");
    DumpFnArgs(this, opt.args);
    absl::StrAppend(out_, ")");
  }
}

void Dump::operator()(ast::PrintStmt const *node) {
  absl::StrAppend(out_, "print ",
                  absl::StrJoin(node->exprs(), ", ", Joiner{this}));
}

void Dump::operator()(ast::ReturnStmt const *node) {
  absl::StrAppend(out_, "return ",
                  absl::StrJoin(node->exprs(), ", ", Joiner{this}));
}

void Dump::operator()(ast::YieldStmt const *node) {
  absl::StrAppend(out_, "yield ",
                  absl::StrJoin(node->exprs(), ", ", Joiner{this}));
}

void Dump::operator()(ast::ScopeLiteral const *node) {
  absl::StrAppend(out_, "scope {\n");
  ++indentation_;
  for (auto const *decl : node->decls()) {
    absl::StrAppend(out_, indent());
    decl->Dump(this);
    absl::StrAppend(out_, "\n");
  }
  --indentation_;
  absl::StrAppend(out_, indent(), "}");
}

void Dump::operator()(ast::ScopeNode const *node) {
  node->name()->Dump(this);
  absl::StrAppend(out_, " ");

  auto const &args = node->args();
  if (not args.empty()) {
    absl::StrAppend(out_, "(");
    DumpFnArgs(this, args);
    absl::StrAppend(out_, ")");
  }
  for (auto const &block : node->blocks()) { block.Dump(this); }
}

void Dump::operator()(ast::StructLiteral const *node) {
  absl::StrAppend(
      out_, "struct (",
      absl::StrJoin(node->args_, ", ",
                    [](std::string *out, ast::Declaration const &d) {
                      Dump dump(out);
                      d.Dump(&dump);
                    }),
      ") {\n");
  ++indentation_;
  for (const auto &f : node->fields_) {
    absl::StrAppend(out_, indent());
    f.Dump(this);
    absl::StrAppend(out_, "\n");
  }
  --indentation_;
  absl::StrAppend(out_, indent(), "}");
}

void Dump::operator()(ast::StructType const *node) {
  absl::StrAppend(out_, "[", absl::StrJoin(node->args_, ", ", Joiner{this}),
                  "; struct]");
}

void Dump::operator()(ast::Switch const *node) {
  absl::StrAppend(out_, "switch ");
  if (node->expr_) {
    absl::StrAppend(out_, "(");
    node->expr_->Dump(this);
    absl::StrAppend(out_, ")");
  }
  absl::StrAppend(out_, "{\n");
  ++indentation_;
  for (auto const &[body, cond] : node->cases_) {
    absl::StrAppend(out_, indent());
    body->Dump(this);
    absl::StrAppend(out_, " when ");
    cond->Dump(this);
    absl::StrAppend(out_, "\n");
  }
  --indentation_;
  absl::StrAppend(out_, indent(), "}");
}

void Dump::operator()(ast::Terminal const *node) {
  switch (node->basic_type()) {
    case type::BasicType::Int8:
      absl::StrAppend(out_, node->as<int8_t>(), "_i8");
      return;
    case type::BasicType::Nat8:
      absl::StrAppend(out_, node->as<uint8_t>(), "_u8");
      return;
    case type::BasicType::Int16:
      absl::StrAppend(out_, node->as<int16_t>(), "_i16");
      return;
    case type::BasicType::Nat16:
      absl::StrAppend(out_, node->as<uint16_t>(), "_u16");
      return;
    case type::BasicType::Int32:
      absl::StrAppend(out_, node->as<int32_t>(), "_i32");
      return;
    case type::BasicType::Nat32:
      absl::StrAppend(out_, node->as<uint32_t>(), "_u32");
      return;
    case type::BasicType::Int64:
      absl::StrAppend(out_, node->as<int64_t>(), "_i64");
      return;
    case type::BasicType::Nat64:
      absl::StrAppend(out_, node->as<uint64_t>(), "_u64");
      return;
    case type::BasicType::Float32:
      absl::StrAppend(out_, node->as<float>(), "_f32");
      return;
    case type::BasicType::Float64:
      absl::StrAppend(out_, node->as<double>(), "_f64");
      return;
    case type::BasicType::Type_:
      absl::StrAppend(out_,
                      type::Prim(node->as<type::BasicType>())->to_string());
      return;
    case type::BasicType::Bool:
      absl::StrAppend(out_, node->as<bool>() ? "true" : "false");
      return;
    default:;
  }
  UNREACHABLE();
}

void Dump::operator()(ast::Unop const *node) {
  if (node->op() == frontend::Operator::TypeOf) {
    absl::StrAppend(out_, "(");
    node->operand()->Dump(this);
    absl::StrAppend(out_, "):?");
  }
  absl::StrAppend(out_, OpStr(node->op()));
  node->operand()->Dump(this);
}

void Dump::operator()(frontend::Token const *node) {
  absl::StrAppend(out_, node->token);
}
}  // namespace ast

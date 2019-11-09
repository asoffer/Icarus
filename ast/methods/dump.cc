#include "ast/methods/dump.h"

#include "absl/strings/str_cat.h"
#include "absl/strings/str_join.h"
#include "ast/ast.h"
#include "base/stringify.h"
#include "frontend/token.h"
#include "ir/results.h"

namespace ast {
std::string Dump::ToString(ast::Node const *node) {
  std::string result;
  Dump d(&result);
  // TODO figure out what's going wrong here.
  if (auto *tok = node->if_as<frontend::Token>()) {
    d.Visit(tok);
  } else {
    d.Visit(node);
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
    d_->Visit(&*node);
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
    d->Visit(&*expr);
    seen_one = true;
  });
}

}  // namespace

void Dump::Visit(ast::Access const *node) {
  if (node->operand()->is<ast::Identifier>() or
      node->operand()->is<ast::Index>()) {
    Visit(node->operand());
  } else {
    absl::StrAppend(out_, "(");
    Visit(node->operand());
    absl::StrAppend(out_, ")");
  }
  absl::StrAppend(out_, ".", node->member_name());
}

void Dump::Visit(ast::ArrayLiteral const *node) {
  absl::StrAppend(out_, "[", absl::StrJoin(node->elems(), ", ", Joiner{this}),
                  "]");
}

void Dump::Visit(ast::ArrayType const *node) {
  absl::StrAppend(out_, "[", absl::StrJoin(node->lengths(), ", ", Joiner{this}),
                  "; ");
  Visit(node->data_type());
  absl::StrAppend(out_, "]");
}

void Dump::Visit(ast::Binop const *node) {
  absl::StrAppend(out_, "(");
  Visit(node->lhs());
  absl::StrAppend(out_, OpStr(node->op()));
  Visit(node->rhs());
  absl::StrAppend(out_, ")");
}

void Dump::Visit(ast::BlockLiteral const *node) {
  absl::StrAppend(out_, "block {\n");

  ++indentation_;
  for (auto const *b : node->before()) {
    absl::StrAppend(out_, indent());
    Visit(b);
    absl::StrAppend(out_, "\n");
  }
  for (auto const *a : node->after()) {
    absl::StrAppend(out_, indent());
    Visit(a);
    absl::StrAppend(out_, "\n");
  }
  --indentation_;
  absl::StrAppend(out_, indent(), "}\n");
}

void Dump::Visit(ast::BlockNode const *node) {
  absl::StrAppend(out_, node->name());
  if (not node->args().empty()) {
    absl::StrAppend(out_, " [", absl::StrJoin(node->args(), ", ", Joiner{this}),
                    "]");
  }
  absl::StrAppend(out_, " {\n");
  ++indentation_;
  for (auto *stmt : node->stmts()) {
    absl::StrAppend(out_, "\n", indent());
    Visit(stmt);
  }
  --indentation_;
  absl::StrAppend(out_, indent(), "}\n");
}

void Dump::Visit(ast::JumpHandler const *node) {
  absl::StrAppend(out_, "jump_handler (",
                  absl::StrJoin(node->input(), ", ", Joiner{this}), ") {\n");
  ++indentation_;
  for (auto *stmt : node->stmts()) {
    absl::StrAppend(out_, "\n", indent());
    Visit(stmt);
  }
  --indentation_;
  absl::StrAppend(out_, indent(), "}\n");
}

void Dump::Visit(ast::BuiltinFn const *node) {
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

void Dump::Visit(ast::Call const *node) {
  Visit(node->callee());
  absl::StrAppend(out_, "(");
  DumpFnArgs(this, node->args());
  absl::StrAppend(out_, ")");
}

void Dump::Visit(ast::Cast const *node) {
  absl::StrAppend(out_, "(");
  Visit(node->expr());
  absl::StrAppend(out_, ") as (");
  Visit(node->type());
  absl::StrAppend(out_, ")");
}

void Dump::Visit(ast::ChainOp const *node) {
  absl::StrAppend(out_, "(");
  for (size_t i = 0; i < node->ops().size(); ++i) {
    Visit(node->exprs()[i]);
    absl::StrAppend(out_, OpStr(node->ops()[i]));
  }
  Visit(node->exprs().back());
}

void Dump::Visit(ast::CommaList const *node) {
  if (node->parenthesized_) {
    absl::StrAppend(out_, "(", absl::StrJoin(node->exprs_, ", ", Joiner{this}),
                    ")");
  } else {
    absl::StrAppend(out_, absl::StrJoin(node->exprs_, ", ", Joiner{this}));
  }
}

void Dump::Visit(ast::Declaration const *node) {
  absl::StrAppend(out_, node->id());
  if (node->type_expr()) {
    absl::StrAppend(
        out_, (node->flags() & ast::Declaration::f_IsConst) ? " :: " : ": ");
    Visit(node->type_expr());
    if (node->init_val()) {
      absl::StrAppend(out_, " = ");
      Visit(node->init_val());
    }
  } else {
    if (node->init_val()) {
      absl::StrAppend(out_, (node->flags() & ast::Declaration::f_IsConst)
                                ? " ::= "
                                : " := ");
      Visit(node->init_val());
    }
  }
}

void Dump::Visit(ast::EnumLiteral const *node) {
  switch (node->kind()) {
    case ast::EnumLiteral::Kind::Enum: absl::StrAppend(out_, "enum {\n"); break;
    case ast::EnumLiteral::Kind::Flags:
      absl::StrAppend(out_, "flags {\n");
      break;
  }
  ++indentation_;
  for (auto const *elem : node->elems()) {
    absl::StrAppend(out_, indent());
    Visit(elem);
    absl::StrAppend(out_, "\n");
  }
  --indentation_;
  absl::StrAppend(out_, indent(), "}");
}

void Dump::Visit(ast::FunctionLiteral const *node) {
  absl::StrAppend(
      out_, "(",
      absl::StrJoin(
          node->params(), ", ",
          [](std::string *out,
             core::Param<std::unique_ptr<ast::Declaration>> const &p) {
            Dump dump(out);
            dump.Visit(p.value.get());
          }),
      ")");

  if (node->is_short()) {
    absl::StrAppend(out_, " => ");
    Visit(node->stmts()[0]);
  } else {
    absl::StrAppend(out_, " -> ");
    if (node->outputs()) {
      absl::StrAppend(out_, "(",
                      absl::StrJoin(*node->outputs(), ", ", Joiner{this}), ")");
    }
    absl::StrAppend(out_, "{");
    ++indentation_;
    for (auto const *stmt : node->stmts()) {
      absl::StrAppend(out_, "\n", indent());
      Visit(stmt);
    }
    --indentation_;
    absl::StrAppend(out_, "\n", indent(), "}");
  }
}

void Dump::Visit(ast::Identifier const *node) {
  absl::StrAppend(out_, node->token());
}

void Dump::Visit(ast::Import const *node) {
  absl::StrAppend(out_, "import ");
  Visit(node->operand());
}

void Dump::Visit(ast::Index const *node) {
  Visit(node->lhs());
  absl::StrAppend(out_, "[");
  Visit(node->rhs());
  absl::StrAppend(out_, "]");
}

void Dump::Visit(ast::Jump const *node) {
  absl::StrAppend(out_, "jump ");
  for (auto const &opt : node->options_) {
    absl::StrAppend(out_, opt.block, "(");
    DumpFnArgs(this, opt.args);
    absl::StrAppend(out_, ")");
  }
}

void Dump::Visit(ast::PrintStmt const *node) {
  absl::StrAppend(out_, "print ",
                  absl::StrJoin(node->exprs(), ", ", Joiner{this}));
}

void Dump::Visit(ast::ReturnStmt const *node) {
  absl::StrAppend(out_, "return ",
                  absl::StrJoin(node->exprs(), ", ", Joiner{this}));
}

void Dump::Visit(ast::YieldStmt const *node) {
  absl::StrAppend(out_, "yield ",
                  absl::StrJoin(node->exprs(), ", ", Joiner{this}));
}

void Dump::Visit(ast::ScopeLiteral const *node) {
  absl::StrAppend(out_, "scope {\n");
  ++indentation_;
  for (auto const *decl : node->decls()) {
    absl::StrAppend(out_, indent());
    Visit(decl);
    absl::StrAppend(out_, "\n");
  }
  --indentation_;
  absl::StrAppend(out_, indent(), "}");
}

void Dump::Visit(ast::ScopeNode const *node) {
  Visit(node->name());
  absl::StrAppend(out_, " ");

  auto const &args = node->args();
  if (not args.empty()) {
    absl::StrAppend(out_, "(");
    DumpFnArgs(this, args);
    absl::StrAppend(out_, ")");
  }
  for (auto const &block : node->blocks()) { Visit(&block); }
}

void Dump::Visit(ast::StructLiteral const *node) {
  absl::StrAppend(
      out_, "struct (",
      absl::StrJoin(node->args_, ", ",
                    [](std::string *out, ast::Declaration const &d) {
                      Dump dump(out);
                      dump.Visit(&d);
                    }),
      ") {\n");
  ++indentation_;
  for (const auto &f : node->fields_) {
    absl::StrAppend(out_, indent());
    Visit(&f);
    absl::StrAppend(out_, "\n");
  }
  --indentation_;
  absl::StrAppend(out_, indent(), "}");
}

void Dump::Visit(ast::StructType const *node) {
  absl::StrAppend(out_, "[", absl::StrJoin(node->args_, ", ", Joiner{this}),
                  "; struct]");
}

void Dump::Visit(ast::Switch const *node) {
  absl::StrAppend(out_, "switch ");
  if (node->expr_) {
    absl::StrAppend(out_, "(");
    Visit(node->expr_.get());
    absl::StrAppend(out_, ")");
  }
  absl::StrAppend(out_, "{\n");
  ++indentation_;
  for (auto const &[body, cond] : node->cases_) {
    absl::StrAppend(out_, indent());
    Visit(body.get());
    absl::StrAppend(out_, " when ");
    Visit(cond.get());
    absl::StrAppend(out_, "\n");
  }
  --indentation_;
  absl::StrAppend(out_, indent(), "}");
}

void Dump::Visit(ast::Terminal const *node) {
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
      // TODO make ast not depend on this so it can once again depend ont type.
      absl::StrAppend(out_, "{{unknown-type}}");
      return;
    case type::BasicType::Bool:
      absl::StrAppend(out_, node->as<bool>() ? "true" : "false");
      return;
    default:;
  }
  UNREACHABLE();
}

void Dump::Visit(ast::Unop const *node) {
  if (node->op() == frontend::Operator::TypeOf) {
    absl::StrAppend(out_, "(");
    Visit(node->operand());
    absl::StrAppend(out_, "):?");
  }
  absl::StrAppend(out_, OpStr(node->op()));
  Visit(node->operand());
}

void Dump::Visit(frontend::Token const *node) {
  absl::StrAppend(out_, node->token);
}
}  // namespace ast

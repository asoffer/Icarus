#include "visitor/dump_ast.h"

#include "frontend/token.h"
#include "absl/strings/str_cat.h"
#include "absl/strings/str_join.h"
#include "ast/ast.h"
#include "base/stringify.h"
#include "ir/results.h"

namespace visitor {
std::string DumpAst::ToString(ast::Node const *node) {
  std::string result;
  DumpAst d(&result);
  node->DumpAst(&d);
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
    case frontend::Operator::BufPtr: return "[*]";
    case frontend::Operator::Copy: return "copy ";
    case frontend::Operator::Move: return "move ";
    default: UNREACHABLE();
  }
}

struct Joiner {
  constexpr Joiner(DumpAst *d) : d_(d) {}

  template <typename Node>
  void operator()(std::string *out, Node &&node) {
    std::string *old_out = std::exchange(d_->out_, out);
    node->DumpAst(d_);
    d_->out_ = old_out;
  }

  DumpAst *d_ = nullptr;
};

void DumpFnArgs(DumpAst *d,
                core::FnArgs<std::unique_ptr<ast::Expression>> const &fnargs) {
  bool seen_one = false;
  fnargs.ApplyWithIndex(
      [&](auto &&index, std::unique_ptr<ast::Expression> const &expr) {
        absl::StrAppend(d->out_, seen_one ? ", " : "");
        if constexpr (!std::is_same_v<std::decay_t<decltype(index)>, size_t>) {
          absl::StrAppend(d->out_, index, " = ");
        }
        expr->DumpAst(d);
        seen_one = true;
      });
}

}  // namespace

void DumpAst::operator()(ast::Access const *node) {
  if (node->operand()->is<ast::Identifier>() ||
      node->operand()->is<ast::Index>()) {
    node->operand()->DumpAst(this);
  } else {
    absl::StrAppend(out_, "(");
    node->operand()->DumpAst(this);
    absl::StrAppend(out_, ")");
  }
  absl::StrAppend(out_, ".", node->member_name());
}

void DumpAst::operator()(ast::ArrayLiteral const *node) {
  absl::StrAppend(out_, "[", absl::StrJoin(node->elems(), ", ", Joiner{this}),
                  "]");
}

void DumpAst::operator()(ast::ArrayType const *node) {
  absl::StrAppend(out_, "[", absl::StrJoin(node->lengths(), ", ", Joiner{this}),
                  "; ");
  node->data_type()->DumpAst(this);
  absl::StrAppend(out_, "]");
}

void DumpAst::operator()(ast::Binop const *node) {
  absl::StrAppend(out_, "(");
  node->lhs()->DumpAst(this);
  absl::StrAppend(out_, OpStr(node->op()));
  node->rhs()->DumpAst(this);
  absl::StrAppend(out_, ")");
}

void DumpAst::operator()(ast::BlockLiteral const *node) {
  absl::StrAppend(out_, "block", (node->is_required() ? "" : "?"), " {\n");

  ++indentation_;
  for (auto const *b : node->before()) {
    absl::StrAppend(out_, indent());
    b->DumpAst(this);
  }
  for (auto const *a : node->after()) {
    absl::StrAppend(out_, indent());
    a->DumpAst(this);
  }
  --indentation_;
  absl::StrAppend(out_, indent(), "}\n");
}

void DumpAst::operator()(ast::BlockNode const *node) {
  absl::StrAppend(out_, node->name(), " {\n");
  ++indentation_;
  for (auto *stmt : node->stmts()) {
    absl::StrAppend(out_, "\n", indent());
    stmt->DumpAst(this);
  }
  --indentation_;
  absl::StrAppend(out_, indent(), "}\n");
}

void DumpAst::operator()(ast::BuiltinFn const *node) {
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

void DumpAst::operator()(ast::Call const *node) {
  node->fn_->DumpAst(this);
  absl::StrAppend(out_, "(");
  DumpFnArgs(this, node->args_);
  absl::StrAppend(out_, ")");
}

void DumpAst::operator()(ast::Cast const *node) {
  absl::StrAppend(out_, "(");
  node->expr_->DumpAst(this);
  absl::StrAppend(out_, ") as (");
  node->type_->DumpAst(this);
  absl::StrAppend(out_, ")");
}

void DumpAst::operator()(ast::ChainOp const *node) {
  absl::StrAppend(out_, "(");
  for (size_t i = 0; i < node->ops.size(); ++i) {
    node->exprs[i]->DumpAst(this);
    absl::StrAppend(out_, OpStr(node->ops[i]));
  }
  node->exprs.back()->DumpAst(this);
}

void DumpAst::operator()(ast::CommaList const *node) {
  if (node->parenthesized_) {
    absl::StrAppend(out_, "(", absl::StrJoin(node->exprs_, ", ", Joiner{this}),
                    ")");
  } else {
    absl::StrAppend(out_, absl::StrJoin(node->exprs_, ", ", Joiner{this}));
  }
}

void DumpAst::operator()(ast::Declaration const *node) {
  absl::StrAppend(out_, node->id_);
  if (node->type_expr) {
    absl::StrAppend(out_, node->const_ ? " :: " : ": ");
    node->type_expr->DumpAst(this);
    if (node->init_val) {
      absl::StrAppend(out_, " = ");
      node->init_val->DumpAst(this);
    }
  } else {
    if (node->init_val) {
      absl::StrAppend(out_, node->const_ ? " ::= " : " := ");
      node->init_val->DumpAst(this);
    }
  }
}

void DumpAst::operator()(ast::EnumLiteral const *node) {
  switch (node->kind_) {
    case ast::EnumLiteral::Kind::Enum: absl::StrAppend(out_, "enum {\n"); break;
    case ast::EnumLiteral::Kind::Flags:
      absl::StrAppend(out_, "flags {\n");
      break;
  }
  ++indentation_;
  for (auto &elem : node->elems_) {
    absl::StrAppend(out_, indent());
    elem->DumpAst(this);
    absl::StrAppend(out_, "\n");
  }
  --indentation_;
  absl::StrAppend(out_, indent(), "}");
}

void DumpAst::operator()(ast::FunctionLiteral const *node) {
  absl::StrAppend(
      out_, "(",
      absl::StrJoin(
          node->inputs_, ", ",
          [](std::string *out,
             core::Param<std::unique_ptr<ast::Declaration>> const &p) {
            DumpAst dump(out);
            p.value->DumpAst(&dump);
          }),
      ") -> ");
  if (!node->return_type_inferred_) {
    absl::StrAppend(out_, "(",
                    absl::StrJoin(node->outputs_, ", ", Joiner{this}), ")");
  }
  absl::StrAppend(out_, "{");
  ++indentation_;
  for (auto const &stmt : node->statements_) {
    absl::StrAppend(out_, "\n", indent());
    stmt->DumpAst(this);
  }
  --indentation_;
  absl::StrAppend(out_, "\n", indent(), "}");
}

void DumpAst::operator()(ast::Identifier const *node) {
  absl::StrAppend(out_, node->token);
}

void DumpAst::operator()(ast::Import const *node) {
  absl::StrAppend(out_, "import ");
  node->operand()->DumpAst(this);
}

void DumpAst::operator()(ast::Index const *node) {
  node->lhs()->DumpAst(this);
  absl::StrAppend(out_, "[");
  node->rhs()->DumpAst(this);
  absl::StrAppend(out_, "]");
}

void DumpAst::operator()(ast::Interface const *node) {
  absl::StrAppend(out_, "interface {");
  if (!node->decls().empty()) {
    ++indentation_;
    for (auto const *decl : node->decls()) {
      absl::StrAppend(out_, indent());
      decl->DumpAst(this);
      absl::StrAppend(out_, "\n");
    }
    --indentation_;
  }
  absl::StrAppend(out_, "}");
}

void DumpAst::operator()(ast::RepeatedUnop const *node) {
  absl::StrAppend(out_, OpStr(node->op()));
  for (auto *expr : node->exprs()) { expr->DumpAst(this); }
}

void DumpAst::operator()(ast::ScopeLiteral const *node) {
  absl::StrAppend(out_, "scope ", (node->is_stateful() ? "!" : ""), "{\n");
  ++indentation_;
  for (auto const *decl : node->decls()) {
    absl::StrAppend(out_, indent());
    decl->DumpAst(this);
    absl::StrAppend(out_, "\n");
  }
  --indentation_;
  absl::StrAppend(out_, indent(), "}");
}

void DumpAst::operator()(ast::ScopeNode const *node) {
  node->name_->DumpAst(this);
  absl::StrAppend(out_, " ");

  if (!node->args_.empty()) {
    absl::StrAppend(out_, "(");
    DumpFnArgs(this, node->args_);
    absl::StrAppend(out_, ")");
  }
  for (auto const &block : node->blocks_) { block.DumpAst(this); }
}

void DumpAst::operator()(ast::StructLiteral const *node) {
  absl::StrAppend(
      out_, "struct (",
      absl::StrJoin(node->args_, ", ",
                    [](std::string *out, ast::Declaration const &d) {
                      DumpAst dump(out);
                      d.DumpAst(&dump);
                    }),
      ") {\n");
  ++indentation_;
  for (const auto &f : node->fields_) {
    absl::StrAppend(out_, indent());
    f.DumpAst(this);
    absl::StrAppend(out_, "\n");
  }
  --indentation_;
  absl::StrAppend(out_, indent(), "}");
}

void DumpAst::operator()(ast::StructType const *node) {
  absl::StrAppend(out_, "[", absl::StrJoin(node->args_, ", ", Joiner{this}),
                  "; struct]");
}

void DumpAst::operator()(ast::Switch const *node) {
  absl::StrAppend(out_, "switch ");
  if (node->expr_) {
    absl::StrAppend(out_, "(");
    node->expr_->DumpAst(this);
    absl::StrAppend(out_, ")");
  }
  absl::StrAppend(out_, "{\n");
  ++indentation_;
  for (auto const & [ body, cond ] : node->cases_) {
    absl::StrAppend(out_, indent());
    body->DumpAst(this);
    absl::StrAppend(out_, " when ");
    cond->DumpAst(this);
    absl::StrAppend(out_, "\n");
  }
  --indentation_;
  absl::StrAppend(out_, indent(), "}");
}

void DumpAst::operator()(ast::SwitchWhen const *node) {
  node->body->DumpAst(this);
  absl::StrAppend(out_, " when ");
  node->cond->DumpAst(this);
}

void DumpAst::operator()(ast::Terminal const *node) {
  if (node->type_ == type::Bool) {
    absl::StrAppend(out_, node->results_.get<bool>(0).val_ ? "true" : "false");
  } else if (node->type_ == type::Int64) {
    absl::StrAppend(out_, node->results_.get<int64_t>(0).val_, "_i64");
  } else if (node->type_ == type::Nat64) {
    absl::StrAppend(out_, node->results_.get<uint64_t>(0).val_, "_u64");
  } else if (node->type_ == type::Int32) {
    absl::StrAppend(out_, node->results_.get<int32_t>(0).val_, "_i32");
  } else if (node->type_ == type::Nat32) {
    absl::StrAppend(out_, node->results_.get<uint32_t>(0).val_, "_u32");
  } else if (node->type_ == type::Int16) {
    absl::StrAppend(out_, node->results_.get<int16_t>(0).val_, "_i16");
  } else if (node->type_ == type::Nat16) {
    absl::StrAppend(out_, node->results_.get<uint16_t>(0).val_, "_u16");
  } else if (node->type_ == type::Int8) {
    absl::StrAppend(out_, node->results_.get<int8_t>(0).val_, "_i8");
  } else if (node->type_ == type::Nat8) {
    absl::StrAppend(out_, node->results_.get<uint8_t>(0).val_, "_u8");
  } else if (node->type_ == type::Type_) {
    absl::StrAppend(
        out_, node->results_.get<type::Type const *>(0).val_->to_string());
  } else if (node->type_ == type::Block) {
    absl::StrAppend(out_,
                    base::stringify(node->results_.get<ir::Block>(0).val_));
  } else {
    absl::StrAppend(out_, "<<terminal: ", node->type_->to_string(), ">>");
  }
}

void DumpAst::operator()(ast::Unop const *node) {
  if (node->op == frontend::Operator::TypeOf) {
    absl::StrAppend(out_, "(");
    node->operand->DumpAst(this);
    absl::StrAppend(out_, "):?");
  }
  absl::StrAppend(out_, OpStr(node->op));
  node->operand->DumpAst(this);
}

void DumpAst::operator()(frontend::Token const *node) {
  absl::StrAppend(out_, node->token);
}
}  // namespace visitor

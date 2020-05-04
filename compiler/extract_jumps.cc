#include "compiler/extract_jumps.h"

#include <vector>

#include "ast/ast.h"
#include "base/scope.h"

namespace compiler {
namespace {

template <typename T>
struct SaveVar : public base::UseWithScope {
  template <typename U>
  constexpr SaveVar(std::vector<T> &stack, U new_val) : stack_(stack) {
    stack_.push_back(std::move(new_val));
  }
  ~SaveVar() { stack_.pop_back(); }

  std::vector<T> &stack_;
};

struct Extractor : ast::Visitor<void()> {
  void Visit(ast::Node const *node) { ast::Visitor<void()>::Visit(node); }

  void Visit(ast::Access const *node) final { Visit(node->operand()); }

  void Visit(ast::ArgumentType const *node) final {}

  void Visit(ast::ArrayLiteral const *node) final {
    for (auto const *expr : node->elems()) { Visit(expr); }
  }

  void Visit(ast::ArrayType const *node) final {
    for (auto const &len : node->lengths()) { Visit(len); }
    Visit(node->data_type());
  }

  void Visit(ast::Binop const *node) final {
    Visit(node->lhs());
    Visit(node->rhs());
  }

  void Visit(ast::BlockLiteral const *node) final {
    for (auto const *b : node->before()) { Visit(b); }
    for (auto const *a : node->after()) { Visit(a); }
  }

  void Visit(ast::BlockNode const *node) final {
    for (auto const *stmt : node->stmts()) { Visit(stmt); }
  }

  void Visit(ast::BuiltinFn const *node) final {}

  void Visit(ast::Call const *node) final {
    Visit(node->callee());
    for (ast::Expression const *expr : node->args()) { Visit(expr); }
  }

  void Visit(ast::Cast const *node) final {
    Visit(node->expr());
    Visit(node->type());
  }

  void Visit(ast::ChainOp const *node) final {
    for (auto *expr : node->exprs()) { Visit(expr); }
  }

  void Visit(ast::CommaList const *node) final {
    for (auto &expr : node->exprs_) { Visit(expr.get()); }
  }

  void Visit(ast::Declaration const *node) final {
    if (node->type_expr()) { Visit(node->type_expr()); }
    if (node->init_val()) { Visit(node->init_val()); }
  }

  void Visit(ast::DesignatedInitializer const *node) final {
    Visit(node->type());
    for (auto &[field, expr] : node->assignments()) { Visit(expr.get()); }
  }

  void Visit(ast::EnumLiteral const *node) final {
    for (auto const *elem : node->elems()) { Visit(elem); }
  }

  void Visit(ast::FunctionLiteral const *node) final {
    for (auto const &param : node->params()) { Visit(param.value.get()); }
    auto outputs = node->outputs();
    ICARUS_SCOPE(SaveVar(node_stack_, node)) {
      if (outputs) {
        for (auto *out : *outputs) { Visit(out); }
      }
      for (auto *stmt : node->stmts()) { Visit(stmt); }
    }
  }

  void Visit(ast::Identifier const *node) final {}

  void Visit(ast::Import const *node) final { Visit(node->operand()); }

  void Visit(ast::Index const *node) final {
    Visit(node->lhs());
    Visit(node->rhs());
  }

  void Visit(ast::Goto const *node) final {
    // TODO Can you return or yield or jump from inside a jump block?!
    for (auto const &opt : node->options()) {
      for (std::unique_ptr<ast::Expression> const &expr : opt.args()) {
        Visit(expr.get());
      }
    }
    (*jumps_)[node_stack_.back()].push_back(node);
  }

  void Visit(ast::Label const *node) final {}

  void Visit(ast::Jump const *node) final {
    // TODO Can you return or yield or jump from inside a jump block?!
    for (auto const &param : node->params()) { Visit(param.value.get()); }
    ICARUS_SCOPE(SaveVar(node_stack_, node)) {
      for (auto const *stmt : node->stmts()) { Visit(stmt); }
    }
  }

  void Visit(ast::ReturnStmt const *node) final {
    for (auto *expr : node->exprs()) { Visit(expr); }
    for (auto iter = node_stack_.rbegin(); iter != node_stack_.rend(); ++iter) {
      if ((*iter)->is<ast::FunctionLiteral>()) {
        (*jumps_)[*iter].push_back(node);
        return;
      }
    }
    UNREACHABLE();
  }

  void Visit(ast::YieldStmt const *node) final {
    for (auto *expr : node->exprs()) { Visit(expr); }
    if (auto *label = node->label()) {
      ir::Label yield_label_val = label->value();
      for (auto iter = node_stack_.rbegin(); iter != node_stack_.rend();
           ++iter) {
        auto *scope_node = (*iter)->if_as<ast::ScopeNode>();
        if (not scope_node) { continue; }
        auto *scope_node_label = scope_node->label();
        if (not scope_node_label) { continue; }
        if (label->value() == yield_label_val) {
          (*jumps_)[scope_node].push_back(node);
          return;
        }
      }
    } else {
      for (auto iter = node_stack_.rbegin(); iter != node_stack_.rend();
           ++iter) {
        auto *scope_node = (*iter)->if_as<ast::ScopeNode>();
        if (not scope_node) { continue; }
        (*jumps_)[scope_node].push_back(node);
        return;
      }
    }
    UNREACHABLE();
  }

  void Visit(ast::ScopeLiteral const *node) final {
    for (auto const *decl : node->decls()) { Visit(decl); }
  }

  void Visit(ast::ScopeNode const *node) final {
    Visit(node->name());
    for (auto const *expr : node->args()) { Visit(expr); }

    ICARUS_SCOPE(SaveVar(node_stack_, node)) {
      for (auto const &block : node->blocks()) { Visit(&block); }
    }
  }

  void Visit(ast::StructLiteral const *node) final {
    for (auto const &f : node->fields()) { Visit(&f); }
  }

  void Visit(ast::ParameterizedStructLiteral const *node) final {
    for (auto const &p : node->params()) { Visit(&p); }
    for (auto const &f : node->fields()) { Visit(&f); }
  }

  void Visit(ast::StructType const *node) final {
    for (auto &arg : node->args_) { Visit(arg.get()); }
  }

  void Visit(ast::Switch const *node) final {
    if (node->expr()) { Visit(node->expr()); }
    for (auto &[body, cond] : node->cases()) {
      Visit(body.get());
      Visit(cond.get());
    }
  }

  void Visit(ast::Terminal const *node) final {}

  void Visit(ast::Unop const *node) final { Visit(node->operand()); }

  std::vector<ast::Node const *> node_stack_;
  absl::flat_hash_map</* to = */ ast::Node const *,
                      /* from = */ std::vector<ast::Node const *>>*
      jumps_;
};

}  // namespace

void ExtractJumps(
    absl::flat_hash_map</* to = */ ast::Node const *,
                        /* from = */ std::vector<ast::Node const *>> *map,
    ast::Node const *node) {
  Extractor e;
  e.jumps_ = map;
  e.Visit(node);
}

}  // namespace compiler


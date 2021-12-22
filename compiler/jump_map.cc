#include "compiler/jump_map.h"
#include "ast/visitor.h"
#include "base/scope.h"

namespace compiler {
namespace {

template <typename T>
struct SaveVar : base::UseWithScope {
  constexpr SaveVar(std::vector<T> &stack, std::convertible_to<T> auto new_val)
      : stack_(stack) {
    stack_.push_back(std::move(new_val));
  }
  ~SaveVar() { stack_.pop_back(); }

  std::vector<T> &stack_;
};

}  // namespace

struct JumpMap::NodeExtractor : ast::Visitor<void()> {
  explicit NodeExtractor(JumpMap *jumps) : jumps_(jumps) {}

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

  void Visit(ast::Assignment const *node) final {
    for (auto const *l : node->lhs()) { Visit(l); }
    for (auto const *r : node->rhs()) { Visit(r); }
  }

  void Visit(ast::BinaryOperator const *node) final {
    Visit(&node->lhs());
    Visit(&node->rhs());
  }

  void Visit(ast::BinaryAssignmentOperator const *node) final {
    Visit(&node->lhs());
    Visit(&node->rhs());
  }

  void Visit(ast::BindingDeclaration const *node) final {}

  void Visit(ast::BlockNode const *node) final {
    // Even if this function literal has no return statements, We want to track
    // the fact that we've seen this node.
    if (not jumps_->yields_.try_emplace(node).second) { return; }

    LOG("JumpMap", "Visiting %s", node->DebugString());

    ICARUS_SCOPE(SaveVar(node_stack_, node)) {
      for (auto const *stmt : node->stmts()) { Visit(stmt); }
    }
  }

  void Visit(ast::BuiltinFn const *node) final {}

  void Visit(ast::Call const *node) final {
    Visit(node->callee());
    for (auto const &arg : node->arguments()) { Visit(&arg.expr()); }
  }

  void Visit(ast::Cast const *node) final {
    Visit(node->expr());
    Visit(node->type());
  }

  void Visit(ast::ComparisonOperator const *node) final {
    for (auto *expr : node->exprs()) { Visit(expr); }
  }

  void Visit(ast::Declaration const *node) final {
    if (node->type_expr()) { Visit(node->type_expr()); }
    if (node->init_val()) { Visit(node->init_val()); }
  }

  void Visit(ast::DesignatedInitializer const *node) final {
    Visit(node->type());
    for (auto const *assignment : node->assignments()) {
      // Note: lhs is guaranteed to be an identifier and therefore doesn't need
      // to be extracted.
      for (auto const *expr : assignment->rhs()) { Visit(expr); }
    }
  }

  void Visit(ast::EnumLiteral const *node) final {
    for (auto const &[name, value] : node->specified_values()) {
      Visit(value.get());
    }
  }

  void Visit(ast::FunctionLiteral const *node) final {
    for (auto const &param : node->params()) { Visit(param.value.get()); }
    if (auto outputs = node->outputs(); outputs) {
      for (auto *out : *outputs) { Visit(out); }
    }

    // Even if this function literal has no return statements, We want to track
    // the fact that we've seen this node.
    if (not jumps_->returns_.try_emplace(node).second) { return; }

    ICARUS_SCOPE(SaveVar(node_stack_, node)) {
      for (auto const *stmt : node->stmts()) { Visit(stmt); }
    }
  }

  void Visit(ast::FunctionType const *node) final {
    for (auto const *param : node->params()) { Visit(param); }
    for (auto *out : node->outputs()) { Visit(out); }
  }

  void Visit(ast::Identifier const *node) final {}

  void Visit(ast::Import const *node) final { Visit(node->operand()); }

  void Visit(ast::Index const *node) final {
    Visit(node->lhs());
    Visit(node->rhs());
  }

  void Visit(ast::PatternMatch const *node) final {
    if (node->is_binary()) { Visit(&node->expr()); }
    Visit(&node->pattern());
  }
  void Visit(ast::Label const *node) final {}

  void Visit(ast::ReturnStmt const *node) final {
    for (auto *expr : node->exprs()) { Visit(expr); }
    for (auto iter = node_stack_.rbegin(); iter != node_stack_.rend(); ++iter) {
      if (auto const *fn_lit = (*iter)->if_as<ast::FunctionLiteral>()) {
        jumps_->Insert(fn_lit, node);
        return;
      }
    }
    UNREACHABLE();
  }

  void Visit(ast::YieldStmt const *node) final {
    for (auto const &argument : node->arguments()) { Visit(&argument.expr()); }
    if (auto *label = node->label()) {
      ir::Label yield_label_val = label->value();
      LOG("JumpMap", "Labeled yield: %s", label->DebugString());
      for (auto iter = node_stack_.rbegin(); iter != node_stack_.rend();
           ++iter) {
        auto *scope_node = (*iter)->if_as<ast::ScopeNode>();
        if (not scope_node) {
          LOG("JumpMap", "Ignoring %s", (*iter)->DebugString());
          continue;
        }
        auto *scope_node_label = scope_node->label();
        if (not scope_node_label) {
          LOG("JumpMap", "Ignoring unlabeled %s", scope_node->DebugString());
          continue;
        }
        if (label->value() == yield_label_val) {
          LOG("JumpMap", "Matching labeled yield to scope node %s",
              scope_node->DebugString());
          jumps_->Insert(scope_node, node);
          return;
        }
      }
    } else {
      LOG("JumpMap", "Unlabeled yield");
      jumps_->Insert(&node_stack_.back()->as<ast::BlockNode>(), node);
      return;
    }
    UNREACHABLE();
  }

  void Visit(ast::ScopeLiteral const *node) final {
    for (auto const &param : node->params()) { Visit(param.value.get()); }
    for (auto const *stmt: node->stmts()) { Visit(stmt); }
  }

  void Visit(ast::ScopeNode const *node) final {
    if (not jumps_->yields_.try_emplace(node).second) { return; }

    LOG("JumpMap", "Visiting %s", node->DebugString());
    Visit(node->name());
    for (auto const &argument : node->arguments()) { Visit(&argument.expr()); }

    ICARUS_SCOPE(SaveVar(node_stack_, node)) {
      for (auto const &block : node->blocks()) { Visit(&block); }
    }
  }

  void Visit(ast::ShortFunctionLiteral const *node) final {
    for (auto const &param : node->params()) { Visit(param.value.get()); }
    // Even if this function literal has no return statements, We want to track
    // the fact that we've seen this node.
    if (not jumps_->returns_.try_emplace(node).second) { return; }

    // TODO: We won't find a return statement here in general despite the
    // primary body expression being the return. Figure out what to do about
    // that. (That being said, returns are possible from inside scopes).
    ICARUS_SCOPE(SaveVar(node_stack_, node)) { Visit(node->body()); }
  }

  void Visit(ast::SliceType const *node) final { Visit(node->data_type()); }

  void Visit(ast::StructLiteral const *node) final {
    for (auto const &f : node->fields()) { Visit(&f); }
  }

  void Visit(ast::ParameterizedStructLiteral const *node) final {
    for (auto const &p : node->params()) { Visit(p.value.get()); }
    for (auto const &f : node->fields()) { Visit(&f); }
  }

  void Visit(ast::Terminal const *node) final {}

  void Visit(ast::UnaryOperator const *node) final { Visit(node->operand()); }

  void Visit(ast::IfStmt const *node) final {
    Visit(&node->condition());
    if (not jumps_->yields_.try_emplace(node).second) { return; }

    ICARUS_SCOPE(SaveVar(node_stack_, node)) {
      for (auto const *stmt : node->true_block()) { Visit(stmt); }
      for (auto const *stmt : node->false_block()) { Visit(stmt); }
    }
  }

  void Visit(ast::WhileStmt const *node) final {
    Visit(&node->condition());
    if (not jumps_->yields_.try_emplace(node).second) { return; }

    ICARUS_SCOPE(SaveVar(node_stack_, node)) {
      for (auto const *stmt : node->body()) { Visit(stmt); }
    }
  }

  std::vector<ast::Node const *> node_stack_;
  JumpMap *jumps_;
};

void JumpMap::TrackJumps(ast::Node const *p) {
  JumpMap::NodeExtractor(this).Visit(p);
}

}  // namespace compiler

#include "AST.h"


namespace AST {
  NPtr Unop::build(NPtrVec&& nodes) {
    auto unop_ptr = std::make_shared<Unop>();
    unop_ptr->expr_ = std::static_pointer_cast<Expression>(nodes[1]);
    unop_ptr->line_num_ = nodes[0]->line_num_;

    unop_ptr->type_ = Language::expression;
    if (nodes[0]->node_type() == Language::reserved_return) {
      unop_ptr->token_ = "return";

    } else if (nodes[0]->node_type() == Language::reserved_return) {
      unop_ptr->token_ = "print";

    } else {
      unop_ptr->token_ = nodes[0]->token();
    }

    unop_ptr->precedence_ = Language::op_prec.at(unop_ptr->token());

    return unop_ptr;
  }

  NPtr Unop::build_paren_operator(NPtrVec&& nodes) {
    auto unop_ptr = std::make_shared<Unop>();
    unop_ptr->line_num_ = nodes[1]->line_num_;

    unop_ptr->expr_ =
      std::static_pointer_cast<Expression>(nodes[0]);

    unop_ptr->token_ = "()";
    unop_ptr->type_ = Language::expression;

    unop_ptr->precedence_ = Language::op_prec.at("()");

    return unop_ptr;
  }

  NPtr Binop::build_operator(NPtrVec&& nodes, std::string op_symbol) {
    auto binop_ptr = std::make_shared<Binop>();
    binop_ptr->line_num_ = nodes[1]->line_num_;

    binop_ptr->lhs_ =
      std::static_pointer_cast<Expression>(nodes[0]);

    binop_ptr->rhs_ =
      std::static_pointer_cast<Expression>(nodes[2]);

    binop_ptr->token_ = op_symbol;
    binop_ptr->type_ = Language::generic_operator;

    binop_ptr->precedence_ = Language::op_prec.at(op_symbol);

    return binop_ptr;
  }

  NPtr ChainOp::join(NPtrVec&& nodes) {
    auto lhs_prec = std::static_pointer_cast<Expression>(nodes[0])->precedence();
    auto op_prec = Language::op_prec.at(nodes[1]->token());
    auto rhs_prec = std::static_pointer_cast<Expression>(nodes[2])->precedence();

    if (lhs_prec == op_prec && op_prec == rhs_prec) {
      auto rhs = std::static_pointer_cast<ChainOp>(std::move(nodes[2]));

      auto chain_ptr = std::static_pointer_cast<ChainOp>(std::move(nodes[0]));
      chain_ptr->ops_.emplace_back(std::move(nodes[1]));
      chain_ptr->ops_.insert(chain_ptr->ops_.end(),
          std::make_move_iterator(rhs->ops_.begin()),
          std::make_move_iterator(rhs->ops_.end()));

      chain_ptr->ops_.insert(chain_ptr->ops_.begin(),
          std::make_move_iterator(rhs->ops_.begin()),
          std::make_move_iterator(rhs->ops_.end()));
      return chain_ptr;

    } else if (op_prec != rhs_prec) {
      return build(std::forward<NPtrVec>(nodes));

    } else {  // op_prec == rhs_prec
      auto rhs = std::static_pointer_cast<ChainOp>(std::move(nodes[2]));
      auto chain_ptr = std::make_shared<ChainOp>();
      chain_ptr->exprs_.emplace_back(
          std::move(std::static_pointer_cast<Expression>(nodes[0])));

      chain_ptr->ops_.emplace_back(std::move(nodes[1]));
      chain_ptr->ops_.insert(chain_ptr->ops_.end(),
          std::make_move_iterator(rhs->ops_.begin()),
          std::make_move_iterator(rhs->ops_.end()));

      chain_ptr->exprs_.insert(chain_ptr->exprs_.begin(),
          std::make_move_iterator(rhs->exprs_.begin()),
          std::make_move_iterator(rhs->exprs_.end()));
      return chain_ptr;
    }
  }

  NPtr ChainOp::build(NPtrVec&& nodes) {
    std::shared_ptr<ChainOp> chain_ptr(nullptr);

    // Add to a chain so long as the precedence levels match. The only thing at
    // that precedence level should be the operators which can be chained.
    bool use_old_chain_op = nodes[0]->is_chain_op();
    if (use_old_chain_op) {
      ChainOp* lhs_ptr = static_cast<ChainOp*>(nodes[0].get());

      if (lhs_ptr->precedence() != Language::op_prec.at(nodes[1]->token())) {
        use_old_chain_op = false;
      }
    }

    if (use_old_chain_op) {
      chain_ptr = std::static_pointer_cast<ChainOp>(nodes[0]);

    } else {
      chain_ptr = std::make_shared<ChainOp>();
      chain_ptr->line_num_ = nodes[1]->line_num_;

      chain_ptr->exprs_.push_back(std::static_pointer_cast<Expression>(nodes[0]));
      chain_ptr->precedence_ = Language::op_prec.at(nodes[1]->token());
    }

    chain_ptr->ops_.push_back(nodes[1]);

    chain_ptr->exprs_.push_back(
        std::static_pointer_cast<Expression>(nodes[2]));

    return std::static_pointer_cast<Node>(chain_ptr);
  }

  NPtr ArrayLiteral::build(NPtrVec&& nodes) {
    auto array_lit_ptr = std::make_shared<ArrayLiteral>();
    array_lit_ptr->precedence_ = Language::op_prec.at("MAX");
    array_lit_ptr->line_num_ = nodes[0]->line_num_;

    if (nodes[1]->is_comma_list()) {
      array_lit_ptr->elems_ = std::static_pointer_cast<ChainOp>(nodes[1])->exprs_;

    } else {
      array_lit_ptr->elems_.push_back(std::static_pointer_cast<Expression>(nodes[1]));
    }

    return array_lit_ptr;
  }

  NPtr ArrayType::build(NPtrVec&& nodes) {
    if (nodes[1]->is_comma_list()) {
      auto len_chain = std::static_pointer_cast<ChainOp>(nodes[1]);

      auto iter = len_chain->exprs_.rbegin();
      EPtr prev = std::static_pointer_cast<Expression>(nodes[3]);
      while (iter != len_chain->exprs_.rend()) {
        auto array_type_ptr = new ArrayType;
        array_type_ptr->line_num_ = (*iter)->line_num();
        array_type_ptr->len_ = *iter;

        array_type_ptr->token_ = "";
        array_type_ptr->precedence_ = Language::op_prec.at("MAX");

        array_type_ptr->array_type_ = prev;
        prev = EPtr(array_type_ptr);
        ++iter;
      }
      return std::static_pointer_cast<Node>(prev);

    } else {
      auto array_type_ptr = std::make_shared<ArrayType>();
      array_type_ptr->line_num_ = nodes[0]->line_num_;

      array_type_ptr->len_ =
        std::static_pointer_cast<Expression>(nodes[1]);

      array_type_ptr->array_type_ =
        std::static_pointer_cast<Expression>(nodes[3]);

      array_type_ptr->token_ = "";
      array_type_ptr->precedence_ = Language::op_prec.at("MAX");

      return array_type_ptr;
    }
  }

  NPtr ArrayType::build_unknown(NPtrVec&& nodes) {
    auto array_type_ptr = std::make_shared<ArrayType>();
    array_type_ptr->line_num_ = nodes[0]->line_num_;

    // len_ == nullptr means we do not know the length of the array can change.
    array_type_ptr->len_ = nullptr;

    array_type_ptr->array_type_ =
      std::static_pointer_cast<Expression>(nodes[3]);

    array_type_ptr->token_ = "";
    array_type_ptr->precedence_ = Language::op_prec.at("MAX");

    return array_type_ptr;
  }

  NPtr Terminal::build(NPtrVec&& nodes, Type* t) {
    auto term_ptr = std::make_shared<Terminal>();
    term_ptr->line_num_ = nodes[0]->line_num_;
    term_ptr->expr_type_ = t;
    term_ptr->token_ = nodes[0]->token();
    term_ptr->precedence_ = Language::op_prec.at("MAX");

    return term_ptr;
  }

  NPtr Assignment::build(NPtrVec&& nodes) {
    auto assign_ptr = std::make_shared<Assignment>();
    assign_ptr->line_num_ = nodes[1]->line_num_;

    assign_ptr->lhs_ = std::static_pointer_cast<Expression>(nodes[0]);
    assign_ptr->rhs_ = std::static_pointer_cast<Expression>(nodes[2]);

    assign_ptr->token_ = nodes[1]->token();
    assign_ptr->type_ = Language::assign_operator;

    assign_ptr->precedence_ = Language::op_prec.at(assign_ptr->token_);

    return assign_ptr;
  }

  NPtr Declaration::build(NPtrVec&& nodes,
      const std::string& op, Language::NodeType node_type, bool infer) {

    auto decl_ptr = Scope::make_declaration(nodes[1]->line_num_, nodes[0]->token());
    decl_ptr->decl_type_ = std::static_pointer_cast<Expression>(nodes[2]);

    decl_ptr->token_ = op;
    decl_ptr->type_ = node_type;

    decl_ptr->precedence_ = Language::op_prec.at(op);
    decl_ptr->infer_type_ = infer;

    return std::static_pointer_cast<Node>(decl_ptr);
  }

  NPtr KVPairList::build_one(NPtrVec&& nodes) {
    auto pair_list = std::make_shared<KVPairList>();
    pair_list->line_num_ = nodes[0]->line_num_;
    EPtr key_ptr;

    if (nodes[0]->node_type() == Language::reserved_else) {
      key_ptr = std::make_shared<Terminal>();
      key_ptr->line_num_ = nodes[0]->line_num();
      key_ptr->expr_type_ = Type::get_bool();
      key_ptr->token_ = "else";
      key_ptr->precedence_ = Language::op_prec.at("MAX");

    } else {
      key_ptr = std::static_pointer_cast<Expression>(nodes[0]);
    }

    auto val_ptr = std::static_pointer_cast<Expression>(nodes[2]);

    pair_list->kv_pairs_.emplace_back(std::move(key_ptr), std::move(val_ptr));
    return NPtr(pair_list);
  }

  NPtr KVPairList::build_more(NPtrVec&& nodes) {
    auto pair_list = std::static_pointer_cast<KVPairList>(nodes[0]);
    EPtr key_ptr;

    if (nodes[1]->node_type() == Language::reserved_else) {
      key_ptr = std::make_shared<Terminal>();
      key_ptr->line_num_ = nodes[1]->line_num();
      key_ptr->expr_type_ = Type::get_bool();
      key_ptr->token_ = "else";
      key_ptr->precedence_ = Language::op_prec.at("MAX");

    } else {
      key_ptr = std::static_pointer_cast<Expression>(nodes[1]);
    }

    auto val_ptr = std::static_pointer_cast<Expression>(nodes[3]);

    pair_list->kv_pairs_.emplace_back(std::move(key_ptr), std::move(val_ptr));

    return pair_list;
  }

  NPtr FunctionLiteral::build(NPtrVec&& nodes) {
    auto fn_lit = std::make_shared<FunctionLiteral>();
    fn_lit->line_num_ = nodes[0]->line_num_;

    fn_lit->statements_ = std::static_pointer_cast<Statements>(nodes[2]);

    // TODO scopes inside these statements should point to fn_scope_.

    auto binop_ptr = std::static_pointer_cast<Binop>(nodes[0]);
    fn_lit->return_type_ = std::move(binop_ptr->rhs_);
    auto input_args = binop_ptr->lhs_;

    // TODO What if the fn_expression is more complicated, like a function
    // of the form (int -> int) -> int? I'm not sure how robust this is
    if (input_args->is_declaration()) {
      auto decl_ptr = std::static_pointer_cast<Declaration>(input_args);

      fn_lit->inputs_.push_back(decl_ptr);

    } else if (input_args->is_comma_list()) {
      auto decl_list = std::static_pointer_cast<ChainOp>(input_args);

      // resize the input arg list
      fn_lit->inputs_.resize(decl_list->exprs_.size(), nullptr);

      size_t index = 0;
      for (const auto& expr : decl_list->exprs_) {
        auto decl = std::static_pointer_cast<Declaration>(expr);
        fn_lit->inputs_[index++] = decl;
      }
    }

    return fn_lit;
  }

  NPtr TypeLiteral::build(NPtrVec&& nodes) {
    auto type_lit_ptr = std::make_shared<TypeLiteral>();
    type_lit_ptr->line_num_ = nodes[0]->line_num_;
    type_lit_ptr->expr_type_ = Type::get_type();

    auto stmts = std::static_pointer_cast<Statements>(std::move(nodes[2]));
    for (auto&& stmt : stmts->statements_) {
      // TODO we ignore everything that isn't a declaration.
      // This is a cheap way to get started, but probably not ideal.
      if (!stmt->is_declaration()) continue;

      auto decl = std::static_pointer_cast<Declaration>(std::move(stmt));
      type_lit_ptr->decls_.emplace_back(std::move(decl));
    }

    return type_lit_ptr;
  }

}  // namespace AST

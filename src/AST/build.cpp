#include "AST.h"

#include <queue>

extern std::queue<std::string> file_queue;

namespace AST {
  NPtr Unop::build(NPtrVec&& nodes) {
    auto unop_ptr = std::make_shared<Unop>();
    unop_ptr->expr_ = std::static_pointer_cast<Expression>(nodes[1]);
    auto tk_node = std::static_pointer_cast<TokenNode>(nodes[0]);
    unop_ptr->line_num_ = tk_node->line_num();

    unop_ptr->type_ = Language::expression;
    unop_ptr->op_ = tk_node->operator_type();

    unop_ptr->precedence_ = Language::precedence(unop_ptr->op_);

    return unop_ptr;
  }

  NPtr Unop::build_paren_operator(NPtrVec&& nodes) {
    auto unop_ptr = std::make_shared<Unop>();
    unop_ptr->line_num_ = nodes[1]->line_num();

    unop_ptr->expr_ =
      std::static_pointer_cast<Expression>(nodes[0]);

    unop_ptr->type_ = Language::expression;
    unop_ptr->op_ = Language::Operator::Call;
    unop_ptr->precedence_ = Language::precedence(unop_ptr->op_);

    return unop_ptr;
  }

  NPtr Binop::build_operator(NPtrVec&& nodes, Language::Operator op_class) {
    auto binop_ptr = std::make_shared<Binop>();
    binop_ptr->line_num_ = nodes[1]->line_num();

    binop_ptr->lhs_ =
      std::static_pointer_cast<Expression>(nodes[0]);

    binop_ptr->rhs_ =
      std::static_pointer_cast<Expression>(nodes[2]);

    binop_ptr->type_ = Language::generic_operator;
    binop_ptr->op_ = op_class;

    binop_ptr->precedence_ = Language::precedence(binop_ptr->op_);

    return binop_ptr;
  }

  NPtr Binop::build(NPtrVec&& nodes) {
    auto op = std::static_pointer_cast<TokenNode>(nodes[1]);
    return Binop::build_operator(std::forward<NPtrVec&&>(nodes), op->operator_type());
  }

  NPtr Binop::build_paren_operator(NPtrVec&& nodes) {
    return Binop::build_operator(std::forward<NPtrVec&&>(nodes), Language::Operator::Call);
  }

  NPtr Binop::build_bracket_operator(NPtrVec&& nodes) {
    return Binop::build_operator(std::forward<NPtrVec&&>(nodes), Language::Operator::Index);
  }



  NPtr ChainOp::join(NPtrVec&& nodes) {
    // TODO FIXME
    auto lhs_prec = std::static_pointer_cast<Expression>(nodes[0])->precedence();
    auto op_node = std::static_pointer_cast<TokenNode>(nodes[1]);
    auto op_prec = Language::precedence(op_node->operator_type());
    auto rhs_prec = std::static_pointer_cast<Expression>(nodes[2])->precedence();

    if (lhs_prec == op_prec && op_prec == rhs_prec) {
      auto rhs = std::static_pointer_cast<ChainOp>(std::move(nodes[2]));

      auto chain_ptr = std::static_pointer_cast<ChainOp>(std::move(nodes[0]));

      chain_ptr->ops_.push_back(op_node->operator_type());

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

      const std::string& token = nodes[1]->token();
      // TODO move to lookup table
      if (token == "<") {
        chain_ptr->ops_.push_back(Language::Operator::LessThan);
      } else if (token == "<=") {
        chain_ptr->ops_.push_back(Language::Operator::LessEq);
      } else if (token == "==") {
        chain_ptr->ops_.push_back(Language::Operator::Equal);
      } else if (token == "!=") {
        chain_ptr->ops_.push_back(Language::Operator::NotEqual);
      } else if (token == ">=") {
        chain_ptr->ops_.push_back(Language::Operator::GreaterEq);
      } else if (token == ">") {
        chain_ptr->ops_.push_back(Language::Operator::GreaterThan);
      } else if (token == "|") {
        chain_ptr->ops_.push_back(Language::Operator::Or);
      } else if (token == "^") {
        chain_ptr->ops_.push_back(Language::Operator::Xor);
      } else if (token == "&") {
        chain_ptr->ops_.push_back(Language::Operator::And);
      } else if (token == ",") {
        chain_ptr->ops_.push_back(Language::Operator::Comma);
      }

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
    auto op_node = std::static_pointer_cast<TokenNode>(nodes[1]);
    auto op_prec = Language::precedence(op_node->operator_type());

    std::shared_ptr<ChainOp> chain_ptr(nullptr);

    // Add to a chain so long as the precedence levels match. The only thing at
    // that precedence level should be the operators which can be chained.
    bool use_old_chain_op = nodes[0]->is_chain_op();
    if (use_old_chain_op) {
      ChainOp* lhs_ptr = static_cast<ChainOp*>(nodes[0].get());

      if (lhs_ptr->precedence() != op_prec) {
        use_old_chain_op = false;
      }
    }

    if (use_old_chain_op) {
      chain_ptr = std::static_pointer_cast<ChainOp>(nodes[0]);

    } else {
      chain_ptr = std::make_shared<ChainOp>();
      chain_ptr->line_num_ = nodes[1]->line_num();

      chain_ptr->exprs_.push_back(std::static_pointer_cast<Expression>(nodes[0]));
      chain_ptr->precedence_ = op_prec;
    }

    chain_ptr->ops_.push_back(op_node->operator_type());
    chain_ptr->exprs_.push_back(
        std::static_pointer_cast<Expression>(nodes[2]));

    return std::static_pointer_cast<Node>(chain_ptr);
  }

  NPtr ArrayLiteral::build(NPtrVec&& nodes) {
    auto array_lit_ptr = std::make_shared<ArrayLiteral>();
    array_lit_ptr->precedence_ =
      Language::precedence(Language::Operator::NotAnOperator);
    array_lit_ptr->line_num_ = nodes[0]->line_num();

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

        array_type_ptr->precedence_ = Language::precedence(Language::Operator::NotAnOperator);

        array_type_ptr->array_type_ = prev;
        prev = EPtr(array_type_ptr);
        ++iter;
      }
      return std::static_pointer_cast<Node>(prev);

    } else {
      auto array_type_ptr = std::make_shared<ArrayType>();
      array_type_ptr->line_num_ = nodes[0]->line_num();

      array_type_ptr->len_ =
        std::static_pointer_cast<Expression>(nodes[1]);

      array_type_ptr->array_type_ =
        std::static_pointer_cast<Expression>(nodes[3]);

      array_type_ptr->precedence_ = Language::precedence(Language::Operator::NotAnOperator);

      return array_type_ptr;
    }
  }

  NPtr ArrayType::build_unknown(NPtrVec&& nodes) {
    auto array_type_ptr = std::make_shared<ArrayType>();
    array_type_ptr->line_num_ = nodes[0]->line_num();

    // len_ == nullptr means we do not know the length of the array can change.
    array_type_ptr->len_ = nullptr;

    array_type_ptr->array_type_ =
      std::static_pointer_cast<Expression>(nodes[3]);

    array_type_ptr->precedence_ = Language::precedence(Language::Operator::NotAnOperator);

    return array_type_ptr;
  }

  NPtr Terminal::build(Language::Terminal term_type, NPtrVec&& nodes, Type* t) {
    // TODO token FIXME
    auto term_ptr = std::make_shared<Terminal>();
    term_ptr->line_num_ = nodes[0]->line_num();
    term_ptr->terminal_type_ = term_type;
    term_ptr->expr_type_ = t;
    term_ptr->token_ = nodes[0]->token();
    term_ptr->precedence_ = Language::precedence(Language::Operator::NotAnOperator);

    return term_ptr;
  }

  NPtr Terminal::build_type_literal(NPtrVec&& nodes) {
    return build(Language::Terminal::Type, std::forward<NPtrVec&&>(nodes), Type::get_type());
  }

  NPtr Terminal::build_string_literal(NPtrVec&& nodes) {
    file_queue.emplace("lib/string.ic");
    return build(Language::Terminal::StringLiteral, std::forward<NPtrVec&&>(nodes), Type::get_unknown());
  }

  NPtr Terminal::build_true(NPtrVec&& nodes) {
    return build(Language::Terminal::True, std::forward<NPtrVec&&>(nodes), Type::get_bool());
  }

  NPtr Terminal::build_false(NPtrVec&& nodes) {
    return build(Language::Terminal::False, std::forward<NPtrVec&&>(nodes), Type::get_bool());
  }

  NPtr Terminal::build_unsigned_integer_literal(NPtrVec&& nodes) {
    return build(Language::Terminal::UInt, std::forward<NPtrVec&&>(nodes), Type::get_uint());
  }

  NPtr Terminal::build_integer_literal(NPtrVec&& nodes) {
    return build(Language::Terminal::Int, std::forward<NPtrVec&&>(nodes), Type::get_int());
  }

  NPtr Terminal::build_real_literal(NPtrVec&& nodes) {
    return build(Language::Terminal::Real, std::forward<NPtrVec&&>(nodes), Type::get_real());
  }

  NPtr Terminal::build_character_literal(NPtrVec&& nodes) {
    return build(Language::Terminal::Char, std::forward<NPtrVec&&>(nodes), Type::get_char());
  }

  NPtr Terminal::build_void_return(NPtrVec&& nodes) {
    return build(Language::Terminal::Return, std::forward<NPtrVec&&>(nodes), Type::get_void());
  }

  NPtr Terminal::build_ASCII(NPtrVec&& nodes) {
    return build(Language::Terminal::ASCII, std::forward<NPtrVec&&>(nodes), Type::get_function(Type::get_uint(), Type::get_char()));
  }



  NPtr Assignment::build(NPtrVec&& nodes) {
    auto assign_ptr = std::make_shared<Assignment>();
    assign_ptr->line_num_ = nodes[1]->line_num();

    assign_ptr->lhs_ = std::static_pointer_cast<Expression>(nodes[0]);
    assign_ptr->rhs_ = std::static_pointer_cast<Expression>(nodes[2]);

    auto op_node = std::static_pointer_cast<TokenNode>(nodes[1]);
    assign_ptr->op_ = op_node->operator_type();
    assign_ptr->type_ = Language::assign_operator;

    assign_ptr->precedence_ = Language::precedence(assign_ptr->op_);

    return assign_ptr;
  }

  NPtr Declaration::build(NPtrVec&& nodes, Language::NodeType node_type, bool infer) {
    auto decl_ptr = Scope::make_declaration(nodes[1]->line_num(), nodes[0]->token());
    decl_ptr->decl_type_ = std::static_pointer_cast<Expression>(nodes[2]);

    decl_ptr->type_ = node_type;

    decl_ptr->op_ = infer
      ? Language::Operator::ColonEq
      : Language::Operator::Colon;

    decl_ptr->precedence_ = Language::precedence(decl_ptr->op_);
    decl_ptr->infer_type_ = infer;

    return std::static_pointer_cast<Node>(decl_ptr);
  }

  NPtr Declaration::build_decl(NPtrVec&& nodes) {
    return build(std::forward<NPtrVec&&>(nodes), Language::decl_operator, false);
  }

  NPtr Declaration::build_assign(NPtrVec&& nodes) {
    return build(std::forward<NPtrVec&&>(nodes), Language::decl_assign_operator, true);
  }


  NPtr KVPairList::build_one(NPtrVec&& nodes) {
    auto pair_list = std::make_shared<KVPairList>();
    pair_list->line_num_ = nodes[0]->line_num();
    EPtr key_ptr;

    if (nodes[0]->node_type() == Language::reserved_else) {
      key_ptr = std::make_shared<Terminal>();
      key_ptr->line_num_ = nodes[0]->line_num();
      key_ptr->expr_type_ = Type::get_bool();
      key_ptr->token_ = "else";
      key_ptr->precedence_ = 
        Language::precedence(Language::Operator::NotAnOperator);

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
      key_ptr->precedence_ = 
        Language::precedence(Language::Operator::NotAnOperator);

    } else {
      key_ptr = std::static_pointer_cast<Expression>(nodes[1]);
    }

    auto val_ptr = std::static_pointer_cast<Expression>(nodes[3]);

    pair_list->kv_pairs_.emplace_back(std::move(key_ptr), std::move(val_ptr));

    return pair_list;
  }

  NPtr KVPairList::build_one_assignment_error(NPtrVec&& nodes) {
    nodes[1] = error_log.assignment_vs_equality(nodes[1]);
    return build_one(std::forward<NPtrVec&&>(nodes));
  }

  NPtr KVPairList::build_more_assignment_error(NPtrVec&& nodes) {
    nodes[1] = error_log.assignment_vs_equality(nodes[1]);
    return build_more(std::forward<NPtrVec&&>(nodes));
  }

  NPtr FunctionLiteral::build(NPtrVec&& nodes) {
    auto fn_lit = std::make_shared<FunctionLiteral>();
    fn_lit->line_num_ = nodes[0]->line_num();

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
    type_lit_ptr->line_num_ = nodes[0]->line_num();
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

  NPtr Statements::build_one(NPtrVec&& nodes) {
    auto output = std::make_shared<Statements>();
    output->statements_.push_back(std::move(nodes[0]));

    return output;
  }

  NPtr Statements::build_more(NPtrVec&& nodes) {
    auto output = std::static_pointer_cast<Statements>(nodes[0]);
    output->statements_.push_back(std::move(nodes[1]));

    return output;
  }

  NPtr Statements::build_double_expression_error(NPtrVec&& nodes) {
    error_log.log(nodes[0]->line_num(), "Adjacent expressions");

    auto output = std::make_shared<Statements>();
    output->line_num_ = nodes[0]->line_num();
    output->statements_.push_back(std::move(nodes[0]));
    output->statements_.push_back(std::move(nodes[1]));

    return output;
  }

  NPtr Statements::build_extra_expression_error(NPtrVec&& nodes) {
    error_log.log(nodes[0]->line_num(), "Adjacent expressions");

    auto output = std::static_pointer_cast<Statements>(nodes[0]);
    output->statements_.push_back(std::move(nodes[1]));

    return output;
  }

  NPtr Conditional::build_if(NPtrVec&& nodes) {
    auto if_stmt = std::make_shared<Conditional>();
    if_stmt->conds_ = { std::static_pointer_cast<Expression>(nodes[1]) };
    if_stmt->statements_ = { std::static_pointer_cast<Statements>(nodes[3]) };
    if_stmt->body_scopes_.push_back(Scope::build<CondScope>());
    return if_stmt;
  }

  NPtr Conditional::build_extra_else_error(NPtrVec&& nodes) {
    auto if_stmt = std::static_pointer_cast<Conditional>(nodes[0]);
    error_log.log(nodes[1]->line_num(), "If-statement already has an else-branch. The first else-branch is on line " + std::to_string(if_stmt->else_line_num_) + ".");

    return std::move(nodes[0]);
  }

  NPtr Conditional::build_extra_else_if_error(NPtrVec&& nodes) {
    auto if_stmt = std::static_pointer_cast<Conditional>(nodes[0]);
    error_log.log(nodes[1]->line_num(), "Else-if block is unreachable because it follows an else block. The else-block is on line " + std::to_string(if_stmt->else_line_num_) + ".");

    return std::move(nodes[0]);
  }

  NPtr Conditional::build_else_if(NPtrVec&& nodes) {
    auto if_stmt = std::static_pointer_cast<Conditional>(std::move(nodes[0]));
    auto else_if = std::static_pointer_cast<Conditional>(std::move(nodes[2]));

#ifdef DEBUG
    if (else_if->conds_.size()       != 1 ||
        else_if->statements_.size()  != 1 ||
        else_if->body_scopes_.size() != 1) {
      std::cerr << "FATAL: Else-if statement constructed by parser with multiple conditional blocks." << std::endl;
    }
#endif

    if_stmt->conds_.push_back(std::move(else_if->conds_.front()));
    if_stmt->statements_.push_back(std::move(else_if->statements_.front()));
    if_stmt->body_scopes_.push_back(Scope::build<CondScope>());
    return if_stmt;
  }

  NPtr Conditional::build_else(NPtrVec&& nodes) {
    auto if_stmt = std::static_pointer_cast<Conditional>(std::move(nodes[0]));
    if_stmt->else_line_num_ = nodes[1]->line_num();
    if_stmt->statements_.push_back(
        std::static_pointer_cast<Statements>(std::move(nodes[3])));
    if_stmt->body_scopes_.push_back(Scope::build<CondScope>());
    return std::move(if_stmt);
  }

  NPtr Conditional::build_if_assignment_error(NPtrVec&& nodes) {
    nodes[1] = error_log.assignment_vs_equality(nodes[1]);
    return build_if(std::forward<NPtrVec&&>(nodes));
  }

  NPtr EnumLiteral::build(NPtrVec&& nodes) {
    auto enum_lit_ptr = std::make_shared<EnumLiteral>();
    enum_lit_ptr->line_num_ = nodes[0]->line_num();
    enum_lit_ptr->expr_type_ = Type::get_type();

    auto stmts = std::static_pointer_cast<Statements>(std::move(nodes[2]));
    for (auto&& stmt : stmts->statements_) {
      if (!stmt->is_identifier()) {
        error_log.log(stmt->line_num(), "Enum members must be identifiers.");
      }

      // TODO repeated terms?
      auto decl = std::static_pointer_cast<Identifier>(std::move(stmt))->token();
      enum_lit_ptr->vals_.emplace_back(std::move(decl));
    }

    return enum_lit_ptr;
  }

}  // namespace AST

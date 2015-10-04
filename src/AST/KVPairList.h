#ifndef ICARUS_AST_KEY_VALUE_PAIR_LIST_H
#define ICARUS_AST_KEY_VALUE_PAIR_LIST_H

#include <vector>
#include <string>
#include "typedefs.h"
#include "AST/Node.h"
#include "AST/Expression.h"
#include "AST/Terminal.h"
#include "Type.h"

namespace AST {
  class KVPairList : public Node {
    public:
      static NPtr build_one(NPtrVec&& nodes);
      static NPtr build_more(NPtrVec&& nodes);
      static NPtr build_one_assignment_error(NPtrVec&& nodes);
      static NPtr build_more_assignment_error(NPtrVec&& nodes);


      virtual std::string to_string(size_t n) const;
      virtual void join_identifiers(Scope* scope);

      virtual Type verify_types_with_key(Type key_type);

    private:
      KVPairList() {}

      std::vector<std::pair<EPtr, EPtr>> kv_pairs_;
  };

  inline NPtr KVPairList::build_one(NPtrVec&& nodes) {
    auto pair_list = new KVPairList;
    Expression* key_ptr;

    if (nodes[0]->node_type() == Language::reserved_else) {
      key_ptr = new Terminal;
      key_ptr->expr_type_ = Type::Bool;
      key_ptr->token_ = "else";
      key_ptr->precedence_ = Language::op_prec.at("MAX");

    } else {
      key_ptr = static_cast<Expression*>(nodes[0].release());
    }

    auto val_ptr =
      static_cast<Expression*>(nodes[2].release());

    pair_list->kv_pairs_.emplace_back(
        std::unique_ptr<Expression>(key_ptr),
        std::unique_ptr<Expression>(val_ptr));
    return NPtr(pair_list);
  }

  inline NPtr KVPairList::build_more(NPtrVec&& nodes) {
    auto pair_list = static_cast<KVPairList*>(nodes[0].release());
    Expression* key_ptr;

    if (nodes[1]->node_type() == Language::reserved_else) {
      key_ptr = new Terminal;
      key_ptr->expr_type_ = Type::Bool;
      key_ptr->token_ = "else";
      key_ptr->precedence_ = Language::op_prec.at("MAX");

    } else {
      key_ptr = static_cast<Expression*>(nodes[1].release());
    }

    auto val_ptr =
      static_cast<Expression*>(nodes[3].release());

    pair_list->kv_pairs_.emplace_back(
        std::unique_ptr<Expression>(key_ptr),
        std::unique_ptr<Expression>(val_ptr));

    return NPtr(pair_list);
  }

  inline NPtr KVPairList::build_one_assignment_error(NPtrVec&& nodes) {
    std::cerr << "You probably meant `==` instead of `=`" << std::endl;
    
    std::unique_ptr<Assignment> assignment_node(
        static_cast<Assignment*>(nodes[0].release()));

    // TODO this is mostly the same code as Binop::build_operator
    auto binop_ptr = new Binop;
    binop_ptr->lhs_ =
      std::static_pointer_cast<Expression>(assignment_node->lhs_);
    binop_ptr->rhs_ =
      std::static_pointer_cast<Expression>(assignment_node->rhs_);

    binop_ptr->token_ = "==";
    binop_ptr->type_ = Language::generic_operator;
    binop_ptr->precedence_ = Language::op_prec.at("==");

    nodes[0] = NPtr(binop_ptr);


    return build_one(std::forward<NPtrVec>(nodes));
  }

  inline NPtr KVPairList::build_more_assignment_error(NPtrVec&& nodes) {
    std::cerr << "You probably meant `==` instead of `=`" << std::endl;

    std::unique_ptr<Assignment> assignment_node(
        static_cast<Assignment*>(nodes[1].release()));

    // TODO this is mostly the same code as Binop::build_operator
    auto binop_ptr = new Binop;
    binop_ptr->lhs_ =
      std::static_pointer_cast<Expression>(assignment_node->lhs_);
    binop_ptr->rhs_ =
      std::static_pointer_cast<Expression>(assignment_node->rhs_);

    binop_ptr->token_ = "==";
    binop_ptr->type_ = Language::generic_operator;
    binop_ptr->precedence_ = Language::op_prec.at("==");

    nodes[0] = NPtr(binop_ptr);

    return build_more(std::forward<NPtrVec>(nodes));
  }


}  // namespace AST

#endif  // ICARUS_AST_KEY_VALUE_PAIR_LIST_H

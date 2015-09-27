#ifndef ICARUS_AST_KEY_VALUE_PAIR_LIST_H
#define ICARUS_AST_KEY_VALUE_PAIR_LIST_H

#include <vector>
#include <string>
#include "typedefs.h"
#include "AST/Node.h"
#include "AST/Expression.h"


namespace AST {
  class KVPairList : public Node {
    public:
      static NPtr build_one(NPtrVec&& nodes);
      static NPtr build_more(NPtrVec&& nodes);

      virtual std::string to_string(size_t n) const;

    private:
      KVPairList() {}

      std::vector<EPtr> kv_pairs_;
  };

  inline NPtr KVPairList::build_one(NPtrVec&& nodes) {
    auto pair_list = new KVPairList;
    auto expr_ptr =
      static_cast<Expression*>(nodes[0].release());
    pair_list->kv_pairs_.emplace_back(expr_ptr);
    return NPtr(pair_list);
  }

  inline NPtr KVPairList::build_more(NPtrVec&& nodes) {
    auto pair_list = static_cast<KVPairList*>(nodes[0].release());
    auto expr_ptr =
      static_cast<Expression*>(nodes[1].release());
    pair_list->kv_pairs_.emplace_back(expr_ptr);

    return NPtr(pair_list);
  }
}  // namespace AST

#endif  // ICARUS_AST_KEY_VALUE_PAIR_LIST_H

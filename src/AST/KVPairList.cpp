#include "AST/KVPairList.h"

namespace AST {
  std::string KVPairList::to_string(size_t n) const {
    std::string output = "";
    for (const auto& expr_ptr : kv_pairs_) {
      output += expr_ptr->to_string(n + 1);
    }

    return output;
  }

  void KVPairList::join_identifiers(Scope* scope) {
    for (const auto& pair : kv_pairs_) {
      pair->join_identifiers(scope);
    }
  }


}  // namespace AST

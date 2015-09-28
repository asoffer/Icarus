#include "AST/KVPairList.h"

namespace AST {
  std::string KVPairList::to_string(size_t n) const {
    std::string output = "";
    for (const auto& expr_ptr : kv_pairs_) {
      output += expr_ptr->to_string(n + 1);
    }

    return output;
  }

  std::set<std::string> KVPairList::identifiers() const {
    std::set<std::string> output;
    for (const auto& pair : kv_pairs_) {
      auto id_set = pair->identifiers();
      output.insert(id_set.begin(), id_set.end());
    }

    return output;
  }
}  // namespace AST

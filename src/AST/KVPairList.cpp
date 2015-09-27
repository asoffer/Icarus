#include "AST/KVPairList.h"

namespace AST {
  std::string KVPairList::to_string(size_t n) const {
    std::string output = "";
    for (const auto& expr_ptr : kv_pairs_) {
      output += expr_ptr->to_string(n + 1);
    }

    return output;
  }
}  // namespace AST

#include "AST/KVPairList.h"

#include <set>

namespace AST {
  std::string KVPairList::to_string(size_t n) const {
    std::string output, spaces;

    for (size_t i = 0;i < n; ++i) {
      spaces += "  ";
    }

    for (const auto& kv : kv_pairs_) {
      output += spaces + "[=>]\n";
      output += kv.first->to_string(n + 1);
      output += kv.second->to_string(n + 1);
    }

    return output;
  }

  void KVPairList::join_identifiers(Scope* scope) {
    for (const auto& pair : kv_pairs_) {
      pair.first->join_identifiers(scope);
      pair.second->join_identifiers(scope);
    }
  }

  // Verifies that all keys have the same given type `key_type` and that all
  // values have the same (but unspecified type.
  Type KVPairList::verify_types_with_key(Type key_type) {
    std::set<Type> value_types;

    for (const auto& kv : kv_pairs_) {
      kv.first->verify_type_is(key_type);
      kv.second->verify_types();

      //value_types.insert(kv->verify_value_type());
    }


    // TODO guess what type was intended
    if (value_types.size() != 1) {
      std::cout << "Type error: Value do not match in key-value pairs" << std::endl;
      return Type::TypeError;
    }

    // FIXME this paradigm fits really well with Case statements but not
    // KVPairLists so much
    return Type::Unknown;//*value_types.begin();
  }

}  // namespace AST

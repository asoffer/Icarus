#include "AST/Scope.h"
#include "AST/Identifier.h"

namespace AST {
  std::vector<Scope*> Scope::all_scopes = {};

  IdPtr Scope::identifier(const std::string& token_string) {
    auto iter = id_map_.find(token_string);
    if (iter != id_map_.end()) {
        return iter->second;
    }

    return id_map_[token_string] = IdPtr(new Identifier(token_string));
  }

  void Scope::join_identifiers() {
    join_identifiers(this);
  }
}  // namespace AST

#ifndef ICARUS_AST_SCOPE_H
#define ICARUS_AST_SCOPE_H

#include <string>
#include <map>
#include <vector>
#include "typedefs.h"

namespace AST {
  class Scope {
    public:
      static std::vector<Scope*> all_scopes;

      virtual std::string to_string(size_t n) const = 0;
      void join_identifiers();
      virtual void join_identifiers(Scope*) = 0;

      IdPtr identifier(const std::string& token_string);

      Scope() {
        all_scopes.push_back(this);
      }

    private:
      std::map<std::string, IdPtr> id_map_;
  };
}  // namespace AST

#endif  // ICARUS_SCOPE_H

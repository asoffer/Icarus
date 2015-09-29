#ifndef ICARUS_AST_SCOPE_H
#define ICARUS_AST_SCOPE_H

#include <map>
#include <vector>
#include "typedefs.h"

namespace AST {
  class Scope {
    public:
      static std::vector<Scope*> all_scopes;

      virtual void register_declared_variables() = 0;
      virtual std::string to_string(size_t n) const = 0;

      Scope() {
        all_scopes.push_back(this);
      }

    private:
      std::map<std::string, IdPtr> id_map_;
  };
}  // namespace AST

#endif  // ICARUS_SCOPE_H

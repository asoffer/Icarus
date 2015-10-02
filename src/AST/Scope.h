#ifndef ICARUS_AST_SCOPE_H
#define ICARUS_AST_SCOPE_H

#include <string>
#include <map>
#include <set>
#include <vector>
#include "typedefs.h"

namespace AST {
  class Declaration;

  class Scope {
    public:
      static std::vector<Scope*> all_scopes;

      virtual std::string to_string(size_t n) const = 0;
      void join_identifiers();
      void show_identifiers() const;
      bool log_undeclared_identifiers() const;
      void register_declaration(Declaration*);

      virtual void join_identifiers(Scope*) = 0;
      virtual void find_all_decls(Scope*) = 0;

      IdPtr identifier(const std::string& token_string);

      Scope() {
        all_scopes.push_back(this);
      }

    private:
      std::map<std::string, IdPtr> id_map_;
      //std::set<Declaration> decls_;
  };
}  // namespace AST

#endif  // ICARUS_SCOPE_H

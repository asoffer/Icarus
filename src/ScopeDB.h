#ifndef ICARUS_SCOPE_DB_H
#define ICARUS_SCOPE_DB_H

#include <vector>
#include <map>
#include <set>
#include <string>

#include "typedefs.h"

namespace ScopeDB {
  // Entry is the id-number for the parent scope, -1 if no parent exists
  extern std::vector<int> parent_;

  // TODO are these first two necessary?
  extern std::map<IdPtr, DeclPtr> decl_of_;

  extern std::vector<DeclPtr> decl_registry_;

  // To each IdPtr we associate a set holding IdPtrs for which it is needed
  extern std::map<IdPtr, std::set<IdPtr>> needed_for_;
  extern std::vector<std::map<std::string, IdPtr>> ids_in_scope_;


  extern void set_parent(size_t child_id, size_t parent_id);
  extern size_t add_scope();
  extern size_t add_global();
  extern IdPtr identifier(size_t scope_id, std::string id_string);
  extern void verify_no_shadowing();
  extern void determine_declared_types();
  extern DeclPtr make_declaration();

  extern void fill_db();
}  // namespace ScopeDB

#endif  // ICARUS_SCOPE_DB_H

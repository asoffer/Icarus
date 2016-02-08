#ifndef ICARUS_DEPENDENCY_SYSTEM_H
#define ICARUS_DEPENDENCY_SYSTEM_H

#include <map>
#include <set>

#include "typedefs.h"
#include "AST.h"
#include "DependencyTypes.h"

namespace Dependency {
  extern void type_type  (AST::Expression* depender, AST::Expression* dependee);
  extern void type_value (AST::Expression* depender, AST::Expression* dependee);
  extern void value_type (AST::Expression* depender, AST::Expression* dependee);
  extern void value_value(AST::Expression* depender, AST::Expression* dependee);

  extern void traverse_from(PtrWithTorV, std::map<AST::Expression*, Flag>&);

  extern void record(AST::Node* node);
  extern void add_to_table(AST::Expression* depender);
  extern void fill_db();
  extern void assign_order();
}  // namespace Dependency

#endif  // ICARUS_DEPENDENCY_SYSTEM_H

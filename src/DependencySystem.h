#ifndef ICARUS_DEPENDENCY_SYSTEM_H
#define ICARUS_DEPENDENCY_SYSTEM_H

#include <map>
#include <set>

#include "typedefs.h"
#include "AST.h"
#include "DependencyTypes.h"

namespace Dependency {
  extern void type_type  (AST::Node* depender, AST::Node* dependee);
  extern void type_value (AST::Node* depender, AST::Node* dependee);
  extern void value_type (AST::Node* depender, AST::Node* dependee);
  extern void value_value(AST::Node* depender, AST::Node* dependee);

  extern void traverse_from(PtrWithTorV);

  extern void record(AST::Node* node);

  extern void mark_as_done(AST::Node* e);
  extern void add_to_table(AST::Node* depender);
  extern void assign_order();
  extern void write_graphviz();
}  // namespace Dependency

#endif  // ICARUS_DEPENDENCY_SYSTEM_H

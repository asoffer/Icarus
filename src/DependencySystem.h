#ifndef ICARUS_DEPENDENCY_SYSTEM_H
#define ICARUS_DEPENDENCY_SYSTEM_H

#include <map>
#include <set>

#include "typedefs.h"
#include "AST.h"

namespace Dependency {
  // TODO hide this
  extern void type_type(AST::Expression* depender, AST::Expression* dependee);
  extern void type_value(AST::Expression* depender, AST::Expression* dependee);

  extern void record(AST::Node* node);
  extern void add_to_table(AST::Expression* depender);
  extern void fill_db();
  extern void assign_type_order();
}  // namespace Dependency

#endif  // ICARUS_DEPENDENCY_SYSTEM_H

#ifndef ICARUS_TYPEDEFS_H
#define ICARUS_TYPEDEFS_H

#include <vector>
#include <memory>

namespace AST {
  class Node;
  class Expression;
}

typedef std::unique_ptr<AST::Node> NPtr;
typedef std::unique_ptr<AST::Expression> EPtr;
typedef std::vector<NPtr> NPtrVec;
typedef NPtr (*fnptr)(NPtrVec&&);


#endif  // ICARUS_TYPEDEFS_H

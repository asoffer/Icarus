#ifndef ICARUS_TYPEDEFS_H
#define ICARUS_TYPEDEFS_H

#include <vector>
#include <memory>

namespace AST {
  class Node;
  class Expression;
  class Identifier;
  class Declaration;
}

typedef std::shared_ptr<AST::Node> NPtr;
typedef std::shared_ptr<AST::Expression> EPtr;
typedef std::shared_ptr<AST::Identifier> IdPtr;
typedef std::shared_ptr<AST::Declaration> DeclPtr;

typedef std::vector<std::unique_ptr<AST::Node>> NPtrVec;
typedef std::unique_ptr<AST::Node> (*fnptr)(NPtrVec&&);

#endif  // ICARUS_TYPEDEFS_H

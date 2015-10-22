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

typedef std::vector<NPtr> NPtrVec;
typedef NPtr (*fnptr)(NPtrVec&&);

/*
template<typename T> class uid {
  private:
    static size_t count;

  protected:
    uid() : id_(count++) {}
    uid(const uid<T>&) = delete;
    size_t id_;
}

template<typename T> size_t uid::count = 0;
*/

#endif  // ICARUS_TYPEDEFS_H

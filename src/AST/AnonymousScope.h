#ifndef ICARUS_AST_ANONYMOUS_SCOPE_H
#define ICARUS_AST_ANONYMOUS_SCOPE_H

#include <set>
#include "AST/Expression.h"
#include "AST/Node.h"
#include "AST/Statements.h"
#include "AST/Scope.h"
#include "typedefs.h"

namespace AST {
  class AnonymousScope : public Expression, Scope {
    public:
      static NPtr build(NPtrVec&& nodes);

      virtual std::set<std::string> identifiers() const;
      virtual void verify_no_declarations() const;
      virtual std::string to_string(size_t n) const;
      virtual void register_declared_variables();
      virtual void separate_declarations_and_assignments();

    protected:
      std::unique_ptr<Statements> statements_;
      AnonymousScope() {}
  };

  inline NPtr AnonymousScope::build(NPtrVec&& nodes) {
    auto anon_scope = new AnonymousScope;
  
    anon_scope->statements_ =
      std::unique_ptr<Statements>(static_cast<Statements*>(nodes[1].release()));
   
    return NPtr(anon_scope);
  }
}  // namespace AST
#endif  // ICARUS_AST_ANONYMOUS_SCOPE_H

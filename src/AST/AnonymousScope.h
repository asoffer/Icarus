#ifndef ICARUS_AST_ANONYMOUS_SCOPE_H
#define ICARUS_AST_ANONYMOUS_SCOPE_H

#include "AST/Expression.h"
#include "AST/Node.h"
#include "AST/Statements.h"
#include "AST/Scope.h"
#include "typedefs.h"

namespace AST {
  class AnonymousScope : public Expression, public Scope {
    public:
      static NPtr build(NPtrVec&& nodes);
      static std::unique_ptr<AnonymousScope> build_empty();


      virtual std::string to_string(size_t n) const;
      virtual void join_identifiers(Scope* scope);
      virtual void verify_types();
      virtual void find_all_decls(Scope* scope);

      void add_statements(NPtr&& stmts_ptr);

    protected:
      AnonymousScope() {}
      std::unique_ptr<Statements> statements_;
  };


  inline std::unique_ptr<AnonymousScope> AnonymousScope::build_empty() {
    std::unique_ptr<AnonymousScope> anon_scope(new AnonymousScope);
    anon_scope->statements_ = std::unique_ptr<Statements>(new Statements);

    return anon_scope;
  }

  inline NPtr AnonymousScope::build(NPtrVec&& nodes) {
    auto anon_scope = new AnonymousScope;
  
    anon_scope->statements_ =
      std::unique_ptr<Statements>(static_cast<Statements*>(nodes[1].release()));
   
    return NPtr(anon_scope);
  }
}  // namespace AST
#endif  // ICARUS_AST_ANONYMOUS_SCOPE_H

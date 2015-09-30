#ifndef ICARUS_AST_CASE_H
#define ICARUS_AST_CASE_H

#include <memory>
#include "typedefs.h"
#include "AST/Node.h"
#include "AST/KVPairList.h"

namespace AST {
  class Case : public Expression, Scope {
    public:
      static NPtr build(NPtrVec&& nodes);

      virtual std::string to_string(size_t n) const;
      virtual void join_identifiers(Scope* scope);

    private:
       Case() {}
       std::unique_ptr<KVPairList> pairs_;
  };

  inline NPtr Case::build(NPtrVec&& nodes) {
    auto output = new Case;
    output->pairs_ =
      std::unique_ptr<KVPairList>(static_cast<KVPairList*>(nodes[3].release()));
    return NPtr(output);
  }

}  // namespace AST

#endif  // ICARUS_AST_CASE_H

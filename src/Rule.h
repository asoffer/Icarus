#ifndef ICARUS_RULE_H
#define ICARUS_RULE_H

#include <vector>
#include <memory>
#include <set>
#include "Language.h"

namespace AST {
struct Node;
} // namespace AST

class Rule {
  public:
    using NodeTypeVec = std::vector<std::set<Language::NodeType>>;
    using NPtrVec     = std::vector<AST::Node *>;
    using fnptr       = AST::Node *(*)(NPtrVec &&);

    Rule(Language::NodeType output, const NodeTypeVec &input, fnptr fn);

    size_t size() const { return input_.size(); }

    bool match(const NPtrVec &node_stack) const;
    void apply(NPtrVec &node_stack) const;

  private:
    Language::NodeType output_;
    NodeTypeVec input_;
    fnptr fn_;
};

namespace Language {
extern const std::vector<Rule> rules;
} // namespace Language

#endif // ICARUS_RULE_H

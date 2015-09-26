#ifndef ICARUS_RULE_H
#define ICARUS_RULE_H

#include <vector>
#include <memory>
#include "typedefs.h"
#include "AST/Node.h"

class Rule {
  public:
    Rule(AST::Node::Type output, const std::vector<AST::Node::Type>& input, fnptr fn);

    inline size_t size() const { return input_.size(); }

    bool match(const std::vector<NPtr>& node_stack) const;
    void apply(std::vector<NPtr>& node_stack) const;

  private:
    AST::Node::Type output_;
    std::vector<AST::Node::Type> input_;
    fnptr fn_;
};

#endif  // ICARUS_RULE_H

#ifndef ICARUS_RULE_H
#define ICARUS_RULE_H

#include <vector>
#include <memory>
#include "typedefs.h"
#include "AST/Node.h"
#include "Language.h"

class Rule {
  public:
    Rule(Language::NodeType output, const std::vector<Language::NodeType>& input, fnptr fn);

    inline size_t size() const { return input_.size(); }

    bool match(const std::vector<NPtr>& node_stack) const;
    void apply(std::vector<NPtr>& node_stack) const;

  private:
    Language::NodeType output_;
    std::vector<Language::NodeType> input_;
    fnptr fn_;
};

#endif  // ICARUS_RULE_H

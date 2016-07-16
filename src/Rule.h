#ifndef ICARUS_RULE_H
#define ICARUS_RULE_H

class Rule {
public:
  using OptVec = std::vector<unsigned int>;
  using fnptr  = AST::Node *(*)(NPtrVec &&);

  Rule(unsigned short preced, Language::NodeType output, const OptVec &input,
       fnptr fn);

  size_t size() const { return input_.size(); }

  bool match(const std::vector<Language::NodeType> &node_type_stack) const;
  void apply(NPtrVec &node_stack, std::vector<Language::NodeType> &node_type_stack) const;

  unsigned short prec;

private:
  Language::NodeType output_;
  OptVec input_;
  fnptr fn_;
};

#endif // ICARUS_RULE_H

#ifndef ICARUS_RULE_H
#define ICARUS_RULE_H

class Rule {
public:
  using OptVec = std::vector<std::set<Language::NodeType>>;
  using fnptr  = AST::Node *(*)(NPtrVec &&);

  Rule(unsigned short preced, Language::NodeType output, const OptVec &input,
       fnptr fn, ParserMode new_mode = ParserMode::Same);

  size_t size() const { return input_.size(); }

  bool match(const NPtrVec &node_stack) const;
  void apply(NPtrVec &node_stack, ParserMode &mode_) const;

  unsigned short prec;

private:
  Language::NodeType output_;
  OptVec input_;
  fnptr fn_;
  ParserMode new_mode_;
};

#endif // ICARUS_RULE_H

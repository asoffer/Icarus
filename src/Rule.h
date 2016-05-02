#ifndef ICARUS_RULE_H
#define ICARUS_RULE_H

struct Opt {
  std::set<Language::NodeType> node_types;
  bool inside;

  bool match(Language::NodeType nt) const {
    for (auto elem : node_types) {
      if (nt == elem) {
        return inside;
      }
    }

    return !inside;
  }

  Opt(std::set<Language::NodeType> nts, bool in = true)
      : node_types(nts), inside(in) {}
};

class Rule {
public:
  using OptVec = std::vector<Opt>;
  using fnptr  = AST::Node *(*)(NPtrVec &&);

  Rule(Language::NodeType output, const OptVec &input, fnptr fn,
       ParserMode new_mode = ParserMode::Same);

  size_t size() const { return input_.size(); }

  bool match(const NPtrVec &node_stack) const;
  void apply(NPtrVec &node_stack, ParserMode &mode_) const;

private:
  Language::NodeType output_;
  OptVec input_;
  fnptr fn_;
  ParserMode new_mode_;
};

#endif // ICARUS_RULE_H

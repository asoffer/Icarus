#ifndef ICARUS_AST_STATEMENTS_H
#define ICARUS_AST_STATEMENTS_H

#include <vector>

#include "node.h"

namespace AST {
struct Statements : public Node {
  Statements() {}
  Statements(const Statements &statements) : Node(statements.span) {
    content_.reserve(statements.content_.size());
    for (const auto &stmt : statements.content_) {
      content_.emplace_back(stmt->Clone());
    }
  }
  ~Statements() override {}
  Statements(Statements &&) noexcept = default;
  Statements &operator=(Statements &&) noexcept = default;
  Statements &operator=(const Statements& stmts) noexcept {
    content_.reserve(stmts.content_.size());
    content_.clear();
    for (const auto &stmt : stmts.content_) {
      content_.emplace_back(stmt->Clone());
    }
    return *this;
  }

  std::string to_string(size_t n) const override;
  void assign_scope(Scope *scope) override;
  void ClearIdDecls() override;
  void VerifyType(Context *) override;
  void Validate(Context *) override;
  void SaveReferences(Scope *scope, std::vector<IR::Val> *args) override;
  void ExtractReturns(std::vector<const Expression *> *) const override;
  void contextualize(
      const Node *correspondant,
      const std::unordered_map<const Expression *, IR::Val> &) override;

  Statements *Clone() const { return new Statements(*this); }

  IR::Val EmitIR(Context *) override;

  inline size_t size() const { return content_.size(); }

  static Statements Merge(std::vector<Statements> &&stmts_vec) {
    size_t num_stmts = 0;
    for (const auto &stmts : stmts_vec) { num_stmts += stmts.size(); }

    Statements result;
    result.content_.reserve(num_stmts);

    for (auto &&stmts : stmts_vec) {
      for (auto &&stmt : stmts.content_) {
        result.content_.push_back(std::move(stmt));
      }
    }

    return result;
  }

  std::vector<std::unique_ptr<Node>> content_;
};

} // namespace AST
#endif // ICARUS_AST_STATEMENTS_H

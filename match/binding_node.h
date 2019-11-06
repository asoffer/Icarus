#ifndef ICARUS_MATCH_BINDING_NODE_H
#define ICARUS_MATCH_BINDING_NODE_H

#include <string>

#include "absl/strings/str_cat.h"
#include "ast/expression.h"
#include "match/binding_id.h"

namespace match {
inline constexpr std::string_view kMatchPrefix = "@%";

struct BindingNode : public ast::Expression {
 public:
  explicit BindingNode(BindingId id, frontend::SourceRange span)
      : ast::Expression(std::move(span)), id_(id) {}
  ~BindingNode() override {}

#include ICARUS_AST_VISITOR_METHODS

  void Accept(ast::VisitorBase *visitor, void *ret,
              void *arg_tuple) const override {
    visitor->ErasedVisit(this, ret, arg_tuple);
  }

  BindingId id() const { return id_; }

 private:
  BindingId id_;
};

}  // namespace match

#endif  // ICARUS_MATCH_BINDING_NODE_H

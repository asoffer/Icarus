#ifndef ICARUS_MATCH_BINDING_NODE_H
#define ICARUS_MATCH_BINDING_NODE_H

#include <string>

#include "absl/strings/str_cat.h"
#include "ast/expression.h"
#include "match/binding_id.h"

namespace match {
constexpr std::string_view kMatchPrefix = "@%";

struct BindingNode : public ast::Expression {
 public:
  explicit BindingNode(BindingId id, TextSpan span)
      : ast::Expression(std::move(span)), id_(id) {}
  ~BindingNode() override {}
  std::string to_string(size_t) const override {
    return absl::StrCat(kMatchPrefix, id_.view());
  }

#include "visitor/visitors.xmacro.h"

  BindingId id() const { return id_; }

 private:
  BindingId id_;
};

}  // namespace match

#endif  // ICARUS_MATCH_BINDING_NODE_H

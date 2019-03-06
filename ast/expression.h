#ifndef ICARUS_AST_EXPRESSION_H
#define ICARUS_AST_EXPRESSION_H

#include "ast/hashtag.h"
#include "ast/node.h"
#include "ir/addr.h"
#include "ir/register.h"
#include "type/typed_value.h"

namespace ast {
struct Expression : public Node {
  Expression(TextSpan const &span = TextSpan()) : Node(span) {}

  Expression(Expression &&) noexcept      = default;
  Expression(Expression const &) noexcept = default;
  Expression &operator=(Expression &&) noexcept = default;
  Expression &operator=(Expression const &) noexcept = default;

  virtual ~Expression() {}
  virtual std::string to_string(size_t n) const                     = 0;
  virtual void assign_scope(core::Scope *scope)                     = 0;
  virtual VerifyResult VerifyType(Context *ctx)                     = 0;
  virtual ir::Results EmitIr(Context *)                             = 0;
  virtual std::vector<ir::RegisterOr<ir::Addr>> EmitLVal(Context *) = 0;
  virtual void EmitCopyInit(type::Typed<ir::Register> reg, Context *ctx);
  virtual void EmitMoveInit(type::Typed<ir::Register> reg, Context *ctx);
  virtual void DependentDecls(base::Graph<Declaration *> *g,
                              Declaration *d) const = 0;

  virtual bool needs_expansion() const { return false; }
  std::vector<Hashtag> hashtags_;
  bool parenthesized_ = false;

  bool contains_hashtag(Hashtag needle) const {
    for (auto const &tag : hashtags_) {
      if (tag.kind_ == needle.kind_) { return true; }
    }
    return false;
  }
};

}  // namespace ast

#endif  // ICARUS_AST_EXPRESSION_H

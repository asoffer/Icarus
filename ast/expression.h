#ifndef ICARUS_AST_EXPRESSION_H
#define ICARUS_AST_EXPRESSION_H

#include "ast/hashtag.h"
#include "ast/node.h"
#include "ir/addr.h"
#include "ir/register.h"
#include "type/typed_value.h"
#include "type/util.h"

namespace ast {

struct Expression : public Node {
  Expression(TextSpan const &span = TextSpan()) : Node(span) {}

  Expression(Expression &&) noexcept      = default;
  Expression(Expression const &) noexcept = default;
  Expression &operator=(Expression &&) noexcept = default;
  Expression &operator=(Expression const &) noexcept = default;

  virtual ~Expression() {}
  virtual std::string to_string(size_t n) const                     = 0;
  virtual ir::Results EmitIr(Context *)                             = 0;
  virtual std::vector<ir::RegisterOr<ir::Addr>> EmitLVal(Context *) = 0;
  virtual void EmitCopyInit(type::Typed<ir::Reg> reg, Context *ctx);
  virtual void EmitMoveInit(type::Typed<ir::Reg> reg, Context *ctx);

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

// TODO pick a better name
struct ExprPtr {
  constexpr ExprPtr(Expression const *expr, int8_t offset = 0)
      : value_(reinterpret_cast<uintptr_t>(expr) | (offset & 0x03)) {}

  template <typename H>
  friend H AbslHashValue(H h, ExprPtr e) {
    return H::combine(std::move(h), e.value_);
  }

  friend constexpr bool operator==(ExprPtr lhs, ExprPtr rhs) {
    return lhs.value_ == rhs.value_;
  }

  Expression *get() const {
    return (value_ & 0x1) ? nullptr : reinterpret_cast<Expression *>(value_);
  }

  friend std::ostream &operator<<(std::ostream &os, ExprPtr e) {
    return os << "expr-ptr(" << e.value_ << ")";
  }

 private:
  uintptr_t value_;
};

}  // namespace ast

#endif  // ICARUS_AST_EXPRESSION_H

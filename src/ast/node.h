#ifndef ICARUS_AST_NODE_H
#define ICARUS_AST_NODE_H

#include <iosfwd>
#include <limits>
#include <set>
#include <string>
#include <type_traits>
#include <utility>
#include "base/container/unordered_map.h"
#include "base/container/vector.h"
#include "base/untyped_buffer.h"

#include "base/util.h"
#include "frontend/text_span.h"

struct Context;
struct Scope;
namespace type {
struct Type;
}  // namespace type

namespace ir {
struct Val;
}  // namespace ir

namespace ast {
struct Expression;

struct VerifyResult {
  type::Type const *type_;
  bool const_;

  constexpr VerifyResult() : type_(nullptr), const_(false) {}
  constexpr VerifyResult(type::Type const *t, bool b) : type_(t), const_(b) {}

  // TODO you could actually pass some information through successfully. Like
  // maybe there's a type error but you do at least know it's a constant.
  static constexpr VerifyResult Error() { return VerifyResult{nullptr, false}; }
  static constexpr VerifyResult Constant(type::Type const *t) {
    return VerifyResult{t, true};
  }
  static constexpr VerifyResult NonConstant(type::Type const *t) {
    return VerifyResult{t, false};
  }

  explicit operator bool() const { return type_ != nullptr; }
  bool ok() const { return type_ != nullptr; }
  VerifyResult operator*() const { return *this; }
};

enum class JumpKind { Return, Yield };
struct JumpExprs
    : public base::unordered_map<JumpKind, base::vector<Expression const *>> {};

struct Node : public base::Cast<Node> {
  virtual std::string to_string(size_t n) const   = 0;
  virtual void assign_scope(Scope *)              = 0;
  virtual VerifyResult VerifyType(Context *)      = 0;
  virtual void Validate(Context *)                = 0;
  virtual base::vector<ir::Val> EmitIR(Context *) = 0;
  virtual void ExtractJumps(JumpExprs *) const    = 0;

  Node(const TextSpan &span = TextSpan()) : span(span) {}
  virtual ~Node() {}

  std::string to_string() const { return to_string(0); }

  inline friend std::ostream &operator<<(std::ostream &os, const Node &node) {
    return os << node.to_string(0);
  }

  Scope *scope_ = nullptr;
  TextSpan span;
};

}  // namespace ast
#endif  // ICARUS_AST_NODE_H

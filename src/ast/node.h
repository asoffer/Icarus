#ifndef ICARUS_AST_NODE_H
#define ICARUS_AST_NODE_H

#include <iosfwd>
#include <limits>
#include <set>
#include <string>
#include <type_traits>
#include "base/container/unordered_map.h"
#include <utility>
#include "base/container/vector.h"

#include "ast/stages.h"
#include "base/util.h"
#include "frontend/text_span.h"

struct Context;
struct Scope;
namespace type {
struct Type;
} // namespace type

namespace IR {
struct Val;
} // namespace IR

namespace AST {
struct Expression;

struct Node : public base::Cast<Node> {
  virtual std::string to_string(size_t n) const                         = 0;
  virtual void assign_scope(Scope *)                                    = 0;
  virtual void VerifyType(Context *)                                    = 0;
  virtual void Validate(Context *)                                      = 0;
  virtual base::vector<IR::Val> EmitIR(Context *)                        = 0;
  virtual void SaveReferences(Scope *scope, base::vector<IR::Val> *args) = 0;
  virtual void
  contextualize(const Node *correspondant,
                const base::unordered_map<const Expression *, IR::Val> &) = 0;
  virtual Node *Clone() const                                            = 0;
  virtual void ExtractReturns(base::vector<const Expression *> *) const   = 0;

  template <typename T> void limit_to(T &&t) {
    if constexpr (std::is_same_v<std::decay_t<T>, int>) {
      stage_range_.high = std::min(t, stage_range_.high);
    } else {
      stage_range_.high =
          std::min(stage_range_.high, std::forward<T>(t)->stage_range_.high);
    }
  }

  Node(const TextSpan &span = TextSpan()) : span(span) {}
  virtual ~Node() {}

  std::string to_string() const { return to_string(0); }

  inline friend std::ostream &operator<<(std::ostream &os, const Node &node) {
    return os << node.to_string(0);
  }

  Scope *scope_ = nullptr;
  StageRange stage_range_;
  TextSpan span;
};
} // namespace AST
#endif // ICARUS_AST_NODE_H

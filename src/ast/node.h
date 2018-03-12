#ifndef ICARUS_AST_NODE_H
#define ICARUS_AST_NODE_H

#include <iosfwd>
#include <limits>
#include <string>
#include <type_traits>
#include <utility>
#include <vector>

#include "../base/util.h"
#include "../frontend/text_span.h"

#define VIRTUAL_METHODS_FOR_NODES                                              \
  virtual std::string to_string(size_t n) const override;                      \
  std::string to_string() const { return to_string(0); }                       \
  virtual void assign_scope(Scope *scope) override;                            \
  virtual void ClearIdDecls() override;                                        \
  virtual void Validate(Context *) override;                                   \
  virtual void SaveReferences(Scope *scope, std::vector<IR::Val> *args)        \
      override;                                                                \
  virtual void contextualize(                                                  \
      const Node *correspondant,                                               \
      const std::unordered_map<const Expression *, IR::Val> &) override

struct Context;
struct Scope;
namespace type {
struct Type;
} // namespace type

namespace IR {
struct Val;
} // namespace IR

namespace AST {
struct StageRange {
  // Last stage completed so far.
  int low = -1;
  // Last stage you can safely compute.
  int high = std::numeric_limits<int>::max();
  static constexpr int Nothing() { return -1; }
  static constexpr int NoEmitIR() { return 2; }
};

struct Expression;

struct Node : public base::Cast<Node> {
  virtual std::string to_string(size_t n) const = 0;
  virtual void assign_scope(Scope *) {}
  virtual void ClearIdDecls() {}
  virtual void Validate(Context *) = 0;

  virtual IR::Val EmitIR(Context *);
  virtual void SaveReferences(Scope *scope, std::vector<IR::Val> *args) = 0;
  virtual void
  contextualize(const Node *correspondant,
                const std::unordered_map<const Expression *, IR::Val> &) = 0;
  virtual Node *Clone() const                                            = 0;

  std::string to_string() const { return to_string(0); }

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

  inline friend std::ostream &operator<<(std::ostream &os, const Node &node) {
    return os << node.to_string(0);
  }

  Scope *scope_ = nullptr;
  StageRange stage_range_;
  TextSpan span;
};
} // namespace AST
#endif // ICARUS_AST_NODE_H

#ifndef ICARUS_AST_NODE_H
#define ICARUS_AST_NODE_H

#include <iosfwd>
#include <limits>
#include <set>
#include <string>
#include <type_traits>
#include <utility>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "base/graph.h"
#include "base/util.h"
#include "frontend/text_span.h"
#include "ir/results.h"
#include "misc/inference_state.h"
#include "type/typed_value.h"

struct JumpExprs;
struct Context;

namespace core {
struct Scope;
}  // namespace core

namespace type {
struct Type;
}  // namespace type

namespace ast {
struct Expression;
struct Declaration;

struct DeclDepGraph {
  base::Graph<Declaration *> graph_;

  // Some declarations may depend on identifiers, but it could be expensive to
  // compute which declaration that identifier refers to. We may also not have
  // verified everything about that declaration or determined it's type to know
  // which overloads we need. Arbitrarily complicated computation may be needed
  // here. However, lucky for us, we don't care about all declarations, just the
  // ones inside the parameter list. So what we do is check the identifier token
  // and match it to a declaration in this list if there is one, greatly
  // simplifying the required task.
  //
  // This may lead to problems with function overloading though if functions
  // visible in this scope have the same name as a parameter (which would also
  // have to be a function). The problem is that we would pick up the function
  // parameter even if we intended the overload that wasn't a parameter (which
  // we might know if we bothered to check types). This would create an
  // unintended edge in the graph which could create loops. I think we may just
  // want to disallow this.
  //
  // TODO Either disallow this type of shadowing, or figure out another way to
  // solve this problem.
  absl::flat_hash_map<std::string_view, std::vector<Declaration *>> ids_;
};

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

inline std::ostream& operator<<(std::ostream& os, VerifyResult r) {
  if (!r.ok()) { return os << "error"; }
  return os << (r.const_ ? "const[" : "non-const[") << r.type_->to_string()
            << "]";
}

constexpr bool operator==(VerifyResult lhs, VerifyResult rhs) {
  return lhs.type_ == rhs.type_ && lhs.const_ == rhs.const_;
}

constexpr bool operator!=(VerifyResult lhs, VerifyResult rhs) {
  return !(lhs == rhs);
}

struct Node : public base::Cast<Node> {
  virtual std::string to_string(size_t n) const                            = 0;
  virtual void assign_scope(core::Scope *)                                 = 0;
  virtual VerifyResult VerifyType(Context *)                               = 0;
  virtual ir::Results EmitIr(Context *ctx)                                 = 0;
  virtual void ExtractJumps(JumpExprs *) const                             = 0;
  virtual void DependentDecls(DeclDepGraph *g, Declaration *d) const       = 0;
  virtual bool InferType(type::Type const *t, InferenceState *state) const = 0;

  Node(const TextSpan &span = TextSpan()) : span(span) {}
  virtual ~Node() {}

  std::string to_string() const { return to_string(0); }

  inline friend std::ostream &operator<<(std::ostream &os, const Node &node) {
    return os << node.to_string(0);
  }

  core::Scope *scope_ = nullptr;
  TextSpan span;
};

}  // namespace ast
#endif  // ICARUS_AST_NODE_H

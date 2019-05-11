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
#include "ast_visitor/assign_scope.h"
#include "ast_visitor/extract_jumps.h"
#include "ast_visitor/verify_type.h"
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

struct Node : public base::Cast<Node> {
  virtual std::string to_string(size_t n) const                            = 0;
  virtual ir::Results EmitIr(Context *ctx)                                 = 0;
  virtual void DependentDecls(DeclDepGraph *g, Declaration *d) const       = 0;
  virtual bool InferType(type::Type const *t, InferenceState *state) const = 0;

#define ICARUS_AST_VISITOR(ret_type, name, args, body) virtual ret_type name args = 0;
#include "ast_visitor/visitors.xmacro.h"
#undef ICARUS_AST_VISITOR

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

#define ICARUS_AST_VISITOR(ret_type, name, args, body)                         \
  ret_type name args override body

#endif  // ICARUS_AST_NODE_H

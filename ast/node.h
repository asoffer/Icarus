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
#include "ast_visitor/dependent_decls.h"
#include "ast_visitor/extract_jumps.h"
#include "ast_visitor/verify_type.h"
#include "base/util.h"
#include "frontend/text_span.h"
#include "ir/results.h"
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

struct Node : public base::Cast<Node> {
  virtual std::string to_string(size_t n) const = 0;
  virtual ir::Results EmitIr(Context *ctx)      = 0;

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

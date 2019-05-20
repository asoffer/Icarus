#ifndef ICARUS_AST_NODE_H
#define ICARUS_AST_NODE_H

#include <iosfwd>
#include <limits>
#include <sstream>
#include <string>
#include <type_traits>
#include <utility>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "base/util.h"
#include "frontend/text_span.h"
#include "ir/results.h"

#ifdef ICARUS_VISITOR_EMIT_IR
#include "visitor/assign_scope.h"
#include "visitor/dependent_decls.h"
#include "visitor/emit_ir.h"
#include "visitor/extract_jumps.h"
#include "visitor/verify_type.h"
#endif  // ICARUS_VISITOR_EMIT_IR

#ifdef ICARUS_VISITOR_FORMAT
#include "visitor/format.h"
#endif  // ICARUS_VISITOR_FORMAT

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
  Node(const TextSpan &span = TextSpan()) : span(span) {}
  virtual ~Node() {}

#define ICARUS_AST_VISITOR(signature, body)                                    \
  virtual signature { UNREACHABLE(); }
#include "visitor/visitors.xmacro.h"
#undef ICARUS_AST_VISITOR

  virtual std::string to_string(size_t n) const = 0;
  std::string to_string() const { return to_string(0); }

  inline friend std::ostream &operator<<(std::ostream &os, const Node &node) {
    return os << node.to_string(0);
  }

  core::Scope *scope_ = nullptr;
  TextSpan span;
};

}  // namespace ast

#define ICARUS_AST_VISITOR(signature, body) signature override body

#endif  // ICARUS_AST_NODE_H

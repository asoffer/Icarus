#ifndef ICARUS_AST_NODE_H
#define ICARUS_AST_NODE_H

#include <utility>

#include "base/util.h"
#include "frontend/text_span.h"

#include "visitor/dump_ast.h"

#ifdef ICARUS_MATCHER
#include "visitor/match.h"
#endif  // ICARUS_MATCHER

#ifdef ICARUS_VISITOR_EMIT_IR
#include "ir/results.h"
#include "visitor/assign_scope.h"
#include "visitor/dependent_decls.h"
#include "visitor/emit_ir.h"
#include "visitor/extract_jumps.h"
#include "visitor/traditional_compilation.h"
#include "visitor/verify_type.h"
#endif  // ICARUS_VISITOR_EMIT_IR

#ifdef ICARUS_VISITOR_FORMAT
#include "visitor/format.h"
#endif  // ICARUS_VISITOR_FORMAT

namespace core {
struct Scope;
}  // namespace core

namespace ast {
struct Node : public base::Cast<Node> {
  Node(TextSpan span = TextSpan()) : span(std::move(span)) {}
  virtual ~Node() {}

#define ICARUS_AST_VISITOR(signature, body)                                    \
  virtual signature body
#include "visitor/visitors.xmacro.h"
#undef ICARUS_AST_VISITOR

  core::Scope *scope_ = nullptr;
  TextSpan span;
};

}  // namespace ast

#define ICARUS_AST_VISITOR(signature, body) signature override body

#endif  // ICARUS_AST_NODE_H

#ifndef ICARUS_AST_NODE_H
#define ICARUS_AST_NODE_H

#include <utility>

#include "base/util.h"
#include "frontend/source/range.h"

#include "visitor/dump_ast.h"

#ifdef ICARUS_MATCHER
#include "visitor/match.h"
#endif  // ICARUS_MATCHER

#ifdef ICARUS_VISITOR_EMIT_IR
#include "ir/results.h"
#include "visitor/assign_scope.h"
#include "visitor/dependent_decls.h"
#include "compiler/compiler.h"
#include "visitor/extract_jumps.h"
#include "compiler/compiler.h"
#endif  // ICARUS_VISITOR_EMIT_IR

#ifdef ICARUS_VISITOR_FORMAT
#include "format/token_extractor.h"
#endif  // ICARUS_VISITOR_FORMAT

namespace core {
struct Scope;
}  // namespace core

namespace ast {
struct Node : public base::Cast<Node> {
  Node(frontend::SourceRange span = frontend::SourceRange())
      : span(std::move(span)) {}
  virtual ~Node() {}

#define ICARUS_AST_VISITOR(signature, body) virtual signature body
#include "visitor/visitors.xmacro.h"
#undef ICARUS_AST_VISITOR

  core::Scope *scope_ = nullptr;
  frontend::SourceRange span;
};

}  // namespace ast

#define ICARUS_AST_VISITOR(signature, body) signature override body

#endif  // ICARUS_AST_NODE_H
